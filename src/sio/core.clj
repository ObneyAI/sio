(ns sio.core
  "Structured Input/Output for LLM prompts.

  `sio` manages the boundary between your program's data and an LLM: it renders
  a typed I/O contract into a prompt, and parses the model's text (or
  function-call) response back into validated Clojure data. It is deliberately
  provider-agnostic — it never makes an LLM call. Bring your own client
  (litellm-clj, an HTTP call, whatever): `sio` gives you the prompt to send and
  parses whatever comes back.

  ## Terminology

  A **spec** is the I/O contract — a map:

      {:inputs       [<field> ...]   ; fields you fill in
       :outputs      [<field> ...]   ; fields you want the model to produce
       :instructions \"...\"}          ; optional task description / rules

  A **field** is a map:

      {:name        :answer          ; keyword identifier
       :spec        :string          ; a Malli schema (the field's type)
       :description \"The answer\"      ; human-readable hint (optional)
       :type        :image}          ; optional; :image marks a multimodal input

  Note the deliberate overlap: the *container* is a spec, and each *field* also
  carries a `:spec` — its Malli schema. Everything typed here is Malli.

  ## Flow

      (spec->prompt spec)                  ; contract -> prompt template
      (build-message-content spec p input) ; inject inputs (+ images) -> message content
      (parse-output llm-text spec)         ; model text -> {output-field value ...}
      (validate-outputs (:outputs spec) m) ; enforce the Malli schemas

  For function-calling providers, `outputs->tool-definition` emits a tool schema
  and `parse-tool-call-response` reads the arguments back out."
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [malli.core :as m]
            [malli.json-schema :as mjs]))

;; =============================================================================
;; Malli Schema Support
;; =============================================================================

(defn complex-spec?
  "Check if a Malli spec requires JSON serialization.
  Returns true for :map, :map-of, :vector, :sequential, :set, :tuple, etc.
  Handles both bare keywords (:map) and vector forms ([:map ...]).
  Note: :enum is NOT included - enums are plain string values."
  [spec]
  (or (#{:map :map-of :vector :sequential :set :tuple} spec)
      (and (vector? spec)
           (#{:map :map-of :vector :sequential :set :tuple :or :and :maybe} (first spec)))))

(defn spec->type-str
  "Convert a Malli spec to a string type representation.
  For complex types (maps, vectors, enums), returns a JSON-descriptive string."
  [spec]
  (cond
    ;; Primitives
    (= spec :string) "str"
    (= spec :int) "int"
    (= spec :double) "float"
    (= spec :float) "float"
    (= spec :boolean) "bool"
    (= spec :any) "any"
    (= spec 'string?) "str"
    (= spec 'int?) "int"
    (= spec 'double?) "float"
    (= spec 'float?) "float"
    (= spec 'boolean?) "bool"

    ;; Bare keyword complex types
    (= spec :map) "json object"
    (= spec :map-of) "json object"
    (= spec :vector) "json array"
    (= spec :sequential) "json array"
    (= spec :set) "json array (unique items)"
    (= spec :tuple) "json array"

    ;; Map - describe fields as JSON object
    (and (vector? spec) (= :map (first spec)))
    (let [fields (filter vector? (rest spec))
          field-strs (for [[k & rest] fields
                           :let [opts (when (map? (first rest)) (first rest))
                                 field-spec (if opts (second rest) (first rest))
                                 optional? (:optional opts)]]
                       (str (name k) (when optional? "?") ": " (spec->type-str field-spec)))]
      (str "json {" (str/join ", " field-strs) "}"))

    ;; Map-of - describe as JSON object with dynamic keys
    (and (vector? spec) (= :map-of (first spec)))
    (let [[_ key-spec val-spec] spec]
      (str "json object with " (spec->type-str key-spec) " keys and " (spec->type-str val-spec) " values"))

    ;; Vector/sequential - describe as JSON array
    (and (vector? spec) (#{:vector :sequential} (first spec)))
    (str "json array of " (spec->type-str (second spec)))

    ;; Enum - list options
    (and (vector? spec) (= :enum (first spec)))
    (str "one of: " (str/join ", " (map str (rest spec))))

    ;; Maybe - nullable. A union child gets a comma before the null so the null
    ;; reads as an alternative to the WHOLE union, not to its last branch.
    (and (vector? spec) (= :maybe (first spec)))
    (let [child (second spec)
          child-str (spec->type-str child)]
      (if (and (vector? child) (= :or (first child)))
        (str child-str ", or null")
        (str child-str " or null")))

    ;; Union — render every branch. Inherited DSCloj behavior had no :or case and
    ;; fell through to "str", telling the model every union field was a string —
    ;; which actively prompted the singleton-string / quoted-scalar failures the
    ;; parse side then had to repair. Properties (e.g. a :description carried on
    ;; the union) are skipped in the type string, as elsewhere.
    (and (vector? spec) (= :or (first spec)))
    (let [branches (cond-> (rest spec)
                     (map? (second spec)) rest)]
      (str/join " or " (map spec->type-str branches)))

    ;; Wrapped specs like [:string {:min 1}] - recurse on first element
    (vector? spec) (spec->type-str (first spec))

    :else "str"))

(defn validate-field
  "Validate a single field value against its Malli spec.

  Parameters:
  - field: Field definition with :name, :spec, :description
  - value: The value to validate

  Returns value if valid, throws exception if invalid."
  [field value]
  (let [{:keys [name spec]} field]
    (if (m/validate spec value)
      value
      (throw (ex-info (str "Validation failed for field " name)
                      {:field name
                       :spec spec
                       :value value
                       :errors (m/explain spec value)})))))

(defn validate-inputs
  "Validate all input fields against their Malli specs.

  Parameters:
  - fields: Vector of field definitions with :name, :spec, :description
  - input-map: Map of field names to values

  Image fields (:type :image) are skipped — their values are URLs/data URIs,
  not schema-typed data.

  Returns input-map if valid, throws exception if invalid."
  [fields input-map]
  (doseq [field fields]
    (let [{:keys [name spec type]} field]
      (when (and spec (not= type :image))
        (when-let [value (get input-map name)]
          (validate-field field value)))))
  input-map)

(defn validate-outputs
  "Validate all output fields against their Malli specs.

  Parameters:
  - fields: Vector of field definitions with :name, :spec, :description
  - output-map: Map of field names to values

  Returns output-map if valid, throws exception if invalid."
  [fields output-map]
  (doseq [field fields]
    (let [{:keys [name spec]} field]
      (when spec
        (when-let [value (get output-map name)]
          (validate-field field value)))))
  output-map)

(defn- strip-markdown-code-block
  "Strip markdown code block formatting from a string.
   Handles ```json, ```JSON, ``` (plain), etc."
  [s]
  (let [trimmed (str/trim s)]
    (if (str/starts-with? trimmed "```")
      ;; Remove opening ``` with optional language identifier and closing ```
      (let [;; Find end of first line (after ```json or similar)
            first-newline (str/index-of trimmed "\n")
            ;; Content starts after first newline
            content-start (if first-newline (inc first-newline) 3)
            ;; Find closing ```
            closing-idx (str/last-index-of trimmed "```")
            ;; Extract content between markers
            content (if (and closing-idx (> closing-idx content-start))
                      (subs trimmed content-start closing-idx)
                      (subs trimmed content-start))]
        (str/trim content))
      trimmed)))

(defn- parse-json-value
  "Parse a string as JSON if it looks like JSON (starts with { or [).
  Handles markdown code block formatting (```json ... ```).
  Returns the parsed Clojure data structure, or the original value if parsing fails."
  [value]
  (when value
    (let [trimmed (str/trim value)
          ;; Strip markdown code blocks if present
          cleaned (strip-markdown-code-block trimmed)]
      (if (or (str/starts-with? cleaned "{")
              (str/starts-with? cleaned "["))
        (try
          (json/read-str cleaned :key-fn keyword)
          (catch Exception _e
            ;; Return original value if JSON parsing fails
            value))
        ;; Not JSON, return as-is
        value))))

(defn- vector-output-spec?
  "True when a schema requires a vector, allowing for Malli properties and a
  nullable (`:maybe`) wrapper."
  [spec]
  (when (vector? spec)
    (let [schema-type (first spec)
          children (cond-> (rest spec)
                     (map? (second spec)) rest)]
      (or (= :vector schema-type)
          (and (= :maybe schema-type)
               (vector-output-spec? (first children)))))))

;; =============================================================================
;; JSON Schema Conversion (for function calling)
;; =============================================================================

(defn- keyword->json-string [value]
  (if-let [keyword-ns (namespace value)]
    (str keyword-ns "/" (name value))
    (name value)))

(defn- json-compatible-schema [value]
  (cond
    (keyword? value) (keyword->json-string value)
    (map? value) (into (empty value)
                       (map (fn [[k v]]
                              [k (if (#{:properties :definitions} k)
                                   (into {}
                                         (map (fn [[schema-k schema-v]]
                                                [(if (keyword? schema-k)
                                                   (keyword->json-string schema-k)
                                                   schema-k)
                                                 (json-compatible-schema schema-v)]))
                                         v)
                                   (json-compatible-schema v))]))
                       value)
    (vector? value) (mapv json-compatible-schema value)
    (seq? value) (mapv json-compatible-schema value)
    :else value))

(defn- json-pointer->definition-key
  "Decode a `#/definitions/...` JSON Pointer into its definitions-map key.
   Per RFC 6901 `~1` decodes to `/` and `~0` to `~`, and `~1` must be decoded
   first — a namespaced Malli ref like :t/addr arrives as `t~1addr`."
  [pointer]
  (-> pointer
      (str/replace #"^#/definitions/" "")
      (str/replace "~1" "/")
      (str/replace "~0" "~")))

(defn- inline-definitions
  "Splice `:definitions` into the schema so it is self-contained.

  Malli renders a registry reference as
  `{:$ref \"#/definitions/x\" :definitions {\"x\" {...}}}`. The pointer addresses
  the JSON Schema DOCUMENT ROOT, but a field's schema is nested at
  `parameters.properties.<field>`, so the definitions map lands nested too and the
  pointer can never resolve. Consumers pass the tool schema through untouched, so
  an unresolvable pointer reaches the provider verbatim — and provider function-call
  schema subsets (Gemini's, notably) do not reliably support `$ref` at all.

  Resolution is RECURSIVE, because a definition may reference another; inlining only
  the top level leaves nested refs dangling. A self-referential schema degrades to a
  bare object rather than recursing forever, and an unknown pointer is left untouched
  rather than silently becoming nil."
  [schema]
  (if-let [definitions (:definitions schema)]
    (letfn [(resolve* [x seen]
              (cond
                (and (map? x) (:$ref x))
                ;; Malli emits a referring schema's annotations BESIDE the pointer
                ;; ({:$ref "#/definitions/x" :description "..."}). The siblings are the
                ;; MORE specific guidance — authored at the point of use — so they are
                ;; merged over the resolved definition rather than dropped with the
                ;; pointer (issue #2: replacing the whole map silently lost them).
                (let [k (json-pointer->definition-key (:$ref x))
                      siblings (resolve* (dissoc x :$ref) seen)]
                  (cond
                    (contains? seen k) (merge {:type "object"} siblings)
                    (contains? definitions k) (merge (resolve* (get definitions k) (conj seen k))
                                                    siblings)
                    :else x))
                (map? x) (into (empty x) (map (fn [[k v]] [k (resolve* v seen)])) x)
                (vector? x) (mapv #(resolve* % seen) x)
                (seq? x) (mapv #(resolve* % seen) x)
                :else x))]
      (resolve* (dissoc schema :definitions) #{}))
    schema))

(def ^:private numeric-json-types #{"integer" "number"})

(defn- redundant-numeric-union?
  "Is this a union whose branches are all bare numeric types? JSON Schema's
   `number` already admits integers, so integer|number is exactly `number`."
  [branches]
  (and (seq branches)
       (every? (fn [b] (and (map? b)
                            (= #{:type} (set (keys b)))
                            (contains? numeric-json-types (:type b))))
               branches)))

(defn- collapse-numeric-unions
  "Collapse a redundant numeric union into a single `{:type \"number\"}`.

  Malli renders a Clojure-side `[:or :int :double]` — which exists so validation
  accepts both a Long and a Double — as `{:anyOf [{:type \"integer\"} {:type
  \"number\"}]}`. In JSON Schema that union is a no-op, and wrapping it in
  `:maybe` nests a union inside a union:

      {:oneOf [{:anyOf [{:type \"integer\"} {:type \"number\"}]} {:type \"null\"}]}

  That is noise in the contract the model reads, and models comply with a plain
  type more reliably than with nested unions. Collapsing changes nothing
  semantically. A union that is not entirely numeric is left alone, and a lone
  `{:type \"integer\"}` is never widened."
  [schema]
  (letfn [(collapse [x]
            (cond
              (map? x)
              (let [x (into (empty x) (map (fn [[k v]] [k (collapse v)])) x)]
                (if-let [branches (some #(when (redundant-numeric-union? (get x %)) (get x %))
                                        [:anyOf :oneOf])]
                  (-> x (dissoc :anyOf :oneOf) (assoc :type "number"))
                  x))
              (vector? x) (mapv collapse x)
              (seq? x) (mapv collapse x)
              :else x))]
    (collapse schema)))

(defn malli-spec->json-schema
  "Convert a Malli spec to JSON Schema format for function calling parameters.

  Delegates conversion to Malli's JSON Schema transformer, inlines any registry
  references so the result is self-contained (see `inline-definitions`), collapses
  redundant numeric unions (see `collapse-numeric-unions`), then normalizes Clojure
  keywords to their canonical JSON string representation."
  [spec]
  (json-compatible-schema
   (collapse-numeric-unions
    (inline-definitions (mjs/transform spec)))))

(defn outputs->tool-definition
  "Convert a spec's outputs to a function-calling tool definition.

  Creates a `submit_response` tool that accepts all output fields as parameters,
  suitable for providers that support structured output via function calling."
  [spec]
  (let [{:keys [outputs instructions]} spec
        properties (into {}
                         (for [{:keys [name spec description]} outputs]
                           [(clojure.core/name name)
                            (cond-> (malli-spec->json-schema spec)
                              description (assoc :description description))]))
        required (mapv #(clojure.core/name (:name %)) outputs)]
    {:type "function"
     :function {:name "submit_response"
                :description (or instructions "Submit the structured response")
                :parameters {:type "object"
                             :properties properties
                             :required required}}}))

(defn parse-tool-call-response
  "Parse a function-calling response from an LLM into a map of output values.

  Reads the first tool call's JSON arguments and maps them onto the given
  `outputs` field definitions (keyed by field :name). Returns nil if there is
  no tool call, or a map of {output-field-name value} otherwise.

  Parameters:
  - response: The provider response map (expects
              [:choices 0 :message :tool-calls 0 :function :arguments])
  - outputs: Vector of output field definitions"
  [response outputs]
  (let [tool-calls (-> response :choices first :message :tool-calls)
        first-call (first tool-calls)]
    (when first-call
      (let [arguments-str (-> first-call :function :arguments)
            parsed (try
                     (json/read-str arguments-str :key-fn keyword)
                     (catch Exception _e nil))]
        ;; Convert string keys to keyword keys matching output names.
        ;; Cardinality is preserved on THIS path too: a provider that returns a bare
        ;; scalar for a declared vector output gets the same singleton wrap as the
        ;; marker/streaming parse (parse-output) — otherwise the FC path leaks the
        ;; scalar through unwrapped and only one of the two parse paths is safe.
        (when parsed
          (into {}
                (for [{:keys [name spec]} outputs
                      :let [k (keyword (clojure.core/name name))
                            v (get parsed k (get parsed (clojure.core/name name)))
                            v (if (and (vector-output-spec? spec)
                                       (some? v)
                                       (not (vector? v)))
                                [v]
                                v)]]
                  [name v])))))))

;; =============================================================================
;; Prompt Rendering
;; =============================================================================

(defn spec->prompt
  "Render a spec (I/O contract) into a prompt template.

  A spec is a map with:
  - :inputs - Vector of field definitions with :name, :spec, :description keys
  - :outputs - Vector of field definitions with :name, :spec, :description keys
  - :instructions - Optional string describing the task instructions, rules, and examples

  The rendered prompt uses DSPy-style field markers ([[ ## field ## ]]) and
  emits per-type notes (booleans, enums, JSON) for the output fields. Image
  inputs (:type :image) are omitted from the text template — send them as
  content parts via `build-message-content`.

  Returns a formatted prompt string."
  [spec]
  (let [{:keys [inputs outputs instructions]} spec
        format-field (fn [idx {:keys [name spec description]}]
                       (let [type-str (spec->type-str spec)]
                         (str (inc idx) ". `" (clojure.core/name name) "` (" type-str "): " description)))

        ;; Input fields section (exclude image inputs — they're sent as content parts, not text)
        text-inputs (remove #(= :image (:type %)) inputs)
        input-section (when (seq text-inputs)
                        (str "Your input fields are:\n"
                             (str/join "\n" (map-indexed format-field text-inputs))))

        ;; Output fields section
        output-section (when (seq outputs)
                         (str "Your output fields are:\n"
                              (str/join "\n" (map-indexed format-field outputs))))

        ;; Interaction format section (includes both inputs and outputs, excluding image inputs)
        non-image-inputs (remove #(= :image (:type %)) inputs)
        interaction-format (when (or (seq non-image-inputs) (seq outputs))
                             (str "All interactions will be structured in the following way, with the appropriate values filled in.\n\n"
                                  (str/join "\n\n"
                                    (concat
                                      (for [{:keys [name]} non-image-inputs]
                                        (str "[[ ## " (clojure.core/name name) " ## ]]\n"
                                             "{" (clojure.core/name name) "}"))
                                      (for [{:keys [name spec]} outputs]
                                        (let [type-str (spec->type-str spec)]
                                          (str "[[ ## " (clojure.core/name name) " ## ]]\n"
                                               "{" (clojure.core/name name) "}"
                                               (cond
                                                 (= type-str "bool")
                                                 "        # note: the value you produce must be True or False"

                                                 (and (vector? spec) (= :enum (first spec)))
                                                 "        # note: respond with just the value, no quotes"

                                                 (complex-spec? spec)
                                                 "        # note: respond with valid JSON"

                                                 :else ""))))))))

        ;; Instructions section
        instructions-section (when instructions
                               (str "[[ ## completed ## ]]\n"
                                    "In adhering to this structure, your instructions are: " instructions))

        ;; Combine all sections
        sections (filter some? [input-section output-section interaction-format instructions-section])]
    (str/join "\n" sections)))

(defn build-message-content
  "Build message content for an LLM call, supporting multimodal inputs.
  If the spec has any inputs with :type :image, returns a vector of content
  parts (text + images) in the OpenAI content-parts shape. Otherwise returns
  the prompt as a plain string.

  Parameters:
  - spec: The spec with :inputs (image inputs are pulled from input-map by name)
  - prompt: The rendered text prompt (with input values already injected)
  - input-map: Map of input field names to values; image fields may hold a
               single URL/data-URI or a sequence of them"
  [spec prompt input-map]
  (let [image-inputs (filter #(= :image (:type %)) (:inputs spec))]
    (if (empty? image-inputs)
      prompt
      (let [image-parts (for [{:keys [name]} image-inputs
                              :let [v (get input-map name)]
                              :when v
                              img (if (sequential? v) v [v])]
                          {:type "image_url" :image_url {:url img}})]
        (into [{:type "text" :text prompt}] image-parts)))))

;; =============================================================================
;; Output Parsing
;; =============================================================================

(defn- strip-completion-marker
  "Remove the [[ ## completed ## ]] marker and anything after it from a string."
  [s]
  (when s
    (-> s
        (str/replace #"\[\[\s*##\s*completed\s*##\s*\]\].*$" "")
        (str/trim))))

;; ---------------------------------------------------------------------------
;; The marker block
;;
;; A marker block is `[[ ## field-name ## ]]` followed by that field's value.
;; It is the whole of the text protocol, and five things about it are contract.
;; Each one below was, at some point, left implicit — and every one of those
;; produced a measured failure in which a model answered correctly and the
;; answer was lost or altered on the read side.
;;
;;   IDENTITY    The field name is matched LITERALLY, never as a pattern: it is
;;               wrapped in \Q...\E. Splicing it raw made every regex
;;               metacharacter in a name active, and `spec->prompt` renders the
;;               same name into the prompt — so sio could not match its own
;;               rendered marker. Clojure's predicate convention alone
;;               (`:evidence-sufficient?`) turned the trailing `t?` into an
;;               optional `t`, leaving nothing that could match a literal
;;               `? ##`. `*` compiled to an invalid pattern and threw
;;               PatternSyntaxException; `|` threw NullPointerException; `.`
;;               silently matched a DIFFERENT field's marker.
;;
;;   DELIMITERS  `[[` and `]]` tolerate whitespace BETWEEN the brackets — the
;;               same tolerance the pattern always gave the `##` pair. Requiring
;;               the brackets to be adjacent while tolerating `\s*` everywhere
;;               else is an accident, not a design: 23 of 23 captured
;;               unparseable responses from one production node had written
;;               `[[ ## avoid-when ## ] ]`, and the drifted opening `[ [` is
;;               worse still — the PREVIOUS field silently swallows the rest.
;;
;;   START       A block starts a LINE (leading whitespace allowed). Models
;;               indent their markers (9 of 15 responses from one model), and
;;               an indented marker that is not recognised is invisible as a
;;               terminator, so the previous field swallows the remainder.
;;               Requiring a line start is what keeps this tolerance from also
;;               matching a marker QUOTED inside prose, which models also do.
;;
;;   EXTENT      The value runs to the next block's start, or to the end of the
;;               text, and it may begin on the marker's own line
;;               (`[[ ## f ## ]] value`) rather than the next.
;;
;;   OCCURRENCE  The LAST block for a field is the answer. A model's reasoning
;;               drafts the output template — often several times, often
;;               indented — before it writes the real one; taking the first
;;               match returns the SCRATCHPAD. Measured over 30 captured
;;               responses: 10 returned reasoning text rather than the answer,
;;               silently.
;;
;; START, EXTENT and OCCURRENCE are all restorations of upstream (DSPy's
;; ChatAdapter matches `line.strip()`, keeps `line[match.end():]` as content,
;; and assembles sections into a dict so later ones replace earlier ones).
;; DELIMITER tolerance is a deliberate divergence: upstream is stricter, but
;; upstream also RAISES where sio returns nil, so strictness costs it less.
;; ---------------------------------------------------------------------------

(defn- marker-block-pattern
  "Regex matching one marker block for `field-name` and capturing its value."
  [field-name]
  (re-pattern
   (str "(?:\\A|\\n)[ \\t]*"                        ; START: the block begins a line
        "\\[\\s*\\[\\s*##\\s*"                      ; DELIMITER: opening
        (java.util.regex.Pattern/quote (name field-name))  ; IDENTITY: literal
        "\\s*##\\s*\\]\\s*\\]"                      ; DELIMITER: closing
        "[ \\t]*\\n?"                               ; EXTENT: value starts here…
        "([\\s\\S]*?)"                              ; …or on the following line
        "(?=\\n[ \\t]*\\[\\s*\\[\\s*##|$)")))       ; EXTENT: to the next block

(def ^:private true-spellings
  "Spellings that mean TRUE. `True` is what `spec->type-str` renders into the
   prompt (\"must be True or False\"); `true` is JSON's, which the same prompt
   asks for on every complex field and which models therefore reach for; `TRUE`
   is kept for symmetry with the pair below. Nothing else is accepted — a
   spelling the prompt never taught can only be reached by ignoring it, and
   guessing at intent is the generator of the defect this set exists to fix."
  #{"true" "True" "TRUE"})

(def ^:private false-spellings
  "The mirror of `true-spellings`. These previously produced `false` only by
   ACCIDENT — they fell through to a default — which made them indistinguishable
   from junk. They are now honoured by contract."
  #{"false" "False" "FALSE"})

(defn- coerce-boolean
  "A declared boolean is `true`, `false`, or NOTHING.

   The defect this replaces returned `false` for every value outside a
   three-element set. `false` is a VALID boolean, so the fabricated value
   validated, reached the caller, and was reported a success: a model wrote
   `true` and the caller stored `false`, with no error anywhere. An
   uninterpretable value is therefore nil — a declared field with no value —
   which callers already treat as a failure to extract, and which cannot be
   mistaken for an answer."
  [value]
  (let [v (str/trim value)]
    (cond
      (contains? true-spellings v) true
      (contains? false-spellings v) false
      :else nil)))

(defn parse-output
  "Parse LLM output based on a spec's output field definitions.

  Parameters:
  - response: The LLM response string
  - spec: The spec with :outputs

  Returns a map with output field names as keys and parsed, type-coerced values.
  For complex specs (maps, vectors), parses JSON responses; ints/floats are
  coerced from their string form; enums stay plain strings.

  A declared field with no readable value is nil. In particular a BOOLEAN whose
  value is neither a true- nor a false-spelling is nil rather than `false`: a
  fabricated `false` is a valid value and would be indistinguishable from an
  answer. See the marker-block contract above for what counts as a block.

  Single-field fallback: when a spec has exactly one string-typed output and the
  model wrote plain prose without the field markers, the whole response becomes
  that field's value. Multi-field specs stay strict.

  Example:
    (parse-output llm-response {:outputs [{:name :answer :spec :string}
                                          {:name :confidence :spec :boolean}]})"
  [response {:keys [outputs]}]
  (let [extract-field (fn [field-name text]
                        ;; OCCURRENCE: the last block wins — see above.
                        (when-let [match (last (re-seq (marker-block-pattern field-name)
                                                       (or text "")))]
                          (-> (second match)
                              (str/trim)
                              (strip-completion-marker))))

        ;; Get base type from spec (unwrap [:string {:min 1}] -> :string)
        base-type (fn [spec]
                    (if (and (vector? spec) (not (complex-spec? spec)))
                      (first spec)
                      spec))

        ;; Convert string value to appropriate type based on spec
        convert-value (fn [value spec]
                        (let [base (base-type spec)]
                          (cond
                            (nil? value) nil

                            ;; Complex specs - parse as JSON
                            (complex-spec? spec)
                            (let [parsed-value (parse-json-value value)]
                              ;; Models commonly omit JSON array brackets for a
                              ;; singleton. Preserve the schema's cardinality
                              ;; instead of leaking a scalar through parse-output.
                              (if (and (vector-output-spec? spec)
                                       (some? parsed-value)
                                       (not (vector? parsed-value)))
                                [parsed-value]
                                parsed-value))

                            ;; Booleans
                            (#{:boolean 'boolean?} base)
                            (coerce-boolean value)

                            ;; Integers
                            (#{:int 'int?} base)
                            (try (Long/parseLong value) (catch Exception _ value))

                            ;; Floats
                            (#{:double :float 'double? 'float?} base)
                            (try (Double/parseDouble value) (catch Exception _ value))

                            :else value)))
        parsed (into {}
                     (for [{:keys [name spec]} outputs]
                       (let [raw-value (extract-field name response)
                             converted-value (convert-value raw-value spec)]
                         [name converted-value])))]
    ;; Single-field fallback: models answering a one-output-field spec
    ;; often write plain prose and skip the field markers entirely —
    ;; especially under long task instructions. When that single field is
    ;; string-typed, treat the whole response as its value rather than
    ;; returning nil. Multi-field specs stay strict (no way to split
    ;; unmarked text). This is also what makes single-field specs
    ;; reliably streamable: progressive parses yield text-so-far from the
    ;; first delta.
    (let [single (when (= 1 (count outputs)) (first outputs))
          single-base (when single
                        (let [spec (:spec single)]
                          (if (and (vector? spec) (not (complex-spec? spec)))
                            (first spec)
                            spec)))]
      (if (and single
               (contains? #{:string 'string? nil} single-base)
               (every? nil? (vals parsed))
               (string? response)
               (not (str/blank? response)))
        {(:name single) (-> response str/trim strip-completion-marker)}
        parsed))))

;; =============================================================================
;; Streaming Parse Helpers (pure — no channels, no provider)
;; =============================================================================

(defn- parse-json-elem
  "Parse one top-level element substring of a JSON array as JSON.
  Returns the decoded value, or nil if the substring is not valid JSON (e.g. a
  still-incomplete element). Never evaluates code."
  [t]
  (try (json/read-str (str/trim t) :key-fn keyword)
       (catch Exception _ nil)))

(defn parse-streaming-json-array
  "Progressively parse a JSON array from accumulated (possibly incomplete) text.

  Returns a vector of the COMPLETE top-level items decoded so far; a trailing
  item that is still being streamed is omitted, so the vector grows as more
  text arrives. Returns [] before any array has started.

  Safe on untrusted model text: it scans for element boundaries with a
  string/escape-aware depth counter (so brackets and commas inside JSON strings
  and nested arrays/objects are handled correctly) and decodes each complete
  element with clojure.data.json — it never uses the Clojure reader, so a #=(...)
  reader macro in the text cannot execute code."
  [accumulated-text]
  (let [^String s (or accumulated-text "")
        open (str/index-of s "[")
        n (count s)]
    (if (nil? open)
      []
      (loop [i (inc open)          ; scanning position, just past the opening [
             depth 1               ; nesting depth; 1 == top level inside the array
             in-str? false         ; are we inside a JSON string literal?
             escaped? false        ; was the previous char an unescaped backslash?
             elem-start nil        ; start index of the element currently being read
             complete (transient [])] ; substrings of fully-delimited elements
        (if (>= i n)
          ;; Input ran out mid-element — decode only the fully-delimited ones.
          (into [] (keep parse-json-elem (persistent! complete)))
          (let [c (.charAt s i)]
            (cond
              ;; Inside a string: only an unescaped quote can end it.
              in-str?
              (recur (inc i) depth
                     (if (and (= c \") (not escaped?)) false true)
                     (and (not escaped?) (= c \\))
                     elem-start complete)

              (= c \")
              (recur (inc i) depth true false (or elem-start i) complete)

              (or (= c \{) (= c \[))
              (recur (inc i) (inc depth) false false (or elem-start i) complete)

              (or (= c \}) (= c \]))
              (let [d (dec depth)]
                (if (zero? d)
                  ;; Closing bracket of the array: finalize the current element.
                  (let [complete' (if elem-start
                                    (conj! complete (subs s elem-start i))
                                    complete)]
                    (into [] (keep parse-json-elem (persistent! complete'))))
                  (recur (inc i) d false false (or elem-start i) complete)))

              ;; Top-level comma: the current element is complete.
              (and (= c \,) (= depth 1))
              (recur (inc i) depth false false nil
                     (if elem-start (conj! complete (subs s elem-start i)) complete))

              ;; First non-whitespace char of a new element.
              (and (nil? elem-start) (not (Character/isWhitespace c)))
              (recur (inc i) depth false false i complete)

              :else
              (recur (inc i) depth false false elem-start complete))))))))

(defn parse-streaming-output
  "Parse streaming LLM output progressively.

  Parameters:
  - accumulated-text: The accumulated response text so far
  - spec: The spec with :outputs

  Returns parsed output, which may be partial/incomplete. This is just
  `parse-output` applied to the text accumulated so far — pair it with your own
  streaming loop (e.g. over a stream of provider chunks)."
  [accumulated-text spec]
  (parse-output accumulated-text spec))
