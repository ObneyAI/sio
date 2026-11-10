(ns sio.structured-output
  "A bounded strict JSON protocol compiled together with its presence decoder."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.set :as set]
            [malli.core :as m]
            [sio.core :as sio]))

(defn- fail! [phase reason path]
  (throw (ex-info "Structured output contract rejected"
                  {:failure-kind (if (= phase :compile) :unsupported-output-schema :schema-validation-failed)
                   :phase phase :reason reason :path path})))

(defn- output-name [x]
  (if (or (keyword? x) (string? x))
    (clojure.core/name x)
    (fail! :compile :invalid-output-name [])))

(defn- enum-wire-value [x path]
  (cond
    (keyword? x) (subs (str x) 1)
    (or (string? x) (boolean? x) (nil? x)) x
    (and (number? x) (Double/isFinite (double x)) (not (ratio? x))) x
    :else (fail! :compile :unsupported-enum-value path)))

(defn- same-json-value? [a b]
  (if (and (number? a) (number? b))
    ;; Compare the exact decimal values actually serialized on the wire. Host
    ;; numeric promotion (==) can round an adjacent integer or precise decimal
    ;; to the enum's double, inventing membership before original validation.
    (zero? (.compareTo (java.math.BigDecimal. (json/write-str a))
                       (java.math.BigDecimal. (json/write-str b))))
    (= a b)))

(defn- typed-key-wire [value path]
  {"type" (cond (keyword? value) "keyword" (string? value) "string"
                (nil? value) "null" (boolean? value) "boolean"
                (number? value) "number"
                :else (fail! :compile :unsupported-enum-value path))
   "value" (enum-wire-value value path)})

(defn- same-typed-key? [a b]
  (and (= (get a "type") (get b "type"))
       (same-json-value? (get a "value") (get b "value"))))

(defn- finite-key-options [form]
  (let [schema (m/schema form) children (m/children schema)]
    (case (m/type schema)
      (:enum :=) (vec children)
      :or (let [options (mapv finite-key-options children)]
            (when (every? some? options) (vec (mapcat identity options))))
      nil)))

(defn- collisions? [same? values]
  (boolean (some true? (for [i (range (count values)) j (range i)]
                         (same? (nth values i) (nth values j))))))

(defn- tagged-dictionary-keys? [form path]
  (when-let [options (finite-key-options form)]
    (collisions? same-json-value? (mapv #(enum-wire-value % path) (distinct options)))))

(defn- enum-meanings
  ([props children path] (enum-meanings props children path false))
  ([props children path typed-keys?]
  (when (contains? props :descriptions)
    (let [annotation (:descriptions props)
          wire-value #(if typed-keys? (typed-key-wire % path) (enum-wire-value % path))
          same? (if typed-keys? same-typed-key? same-json-value?)
          options (mapv wire-value children)]
      (when-not (map? annotation)
        (fail! :compile :invalid-enum-descriptions path))
      (let [entries (mapv (fn [[id meaning]]
                            (let [wire-id (try (wire-value id)
                                              (catch clojure.lang.ExceptionInfo _
                                                (fail! :compile :invalid-enum-descriptions path)))]
                              (when-not (and (string? meaning) (not (str/blank? meaning))
                                             (some #(same? wire-id %) options))
                                (fail! :compile :invalid-enum-descriptions path))
                              [wire-id meaning])) annotation)]
        (when (some true? (for [i (range (count entries)) j (range i)]
                           (same? (first (nth entries i)) (first (nth entries j)))))
          (fail! :compile :invalid-enum-descriptions path))
        ;; Keep declared option order and exact wire IDs, including namespaces.
        (str/join "\n"
          (keep (fn [option]
                  (when-let [[id meaning] (some #(when (same? option (first %)) %) entries)]
                    (str (json/write-str id) ": " meaning))) options)))))))

(defn- with-description [schema description]
  (if description
    (assoc schema :description
           (str/join "\n" (distinct (remove str/blank? [description (:description schema)]))))
    schema))

(defn- check-schema!
  ([form path optional?] (check-schema! form path optional? false))
  ([form path optional? typed-keys?]
  (let [schema (try (m/schema form) (catch Exception _ (fail! :compile :invalid-schema path)))
        kind (m/type schema)
        props (m/properties schema)
        children (m/children schema)
        allowed (case kind
                  (:string :int :double :float :vector :map-of) #{:description :min :max}
                  :map #{:description :closed}
                  :multi #{:description :dispatch}
                  :enum #{:description :descriptions :ordered-bands}
                  #{:description})]
    (when (some #(not (contains? allowed %)) (keys props))
      (fail! :compile :unsupported-properties path))
    (case kind
      (:string :int :double :float :boolean :nil) nil
      := (enum-wire-value (first children) path)
      :enum (let [values (mapv #(enum-wire-value % path) children)]
              (when (and (contains? props :ordered-bands)
                         (not (boolean? (:ordered-bands props))))
                (fail! :compile :invalid-ordered-bands path))
              (enum-meanings props children path typed-keys?)
              (when (and (not typed-keys?) (some true?
                          (for [i (range (count values)) j (range i)
                                :let [a (nth values i) b (nth values j)]]
                            (same-json-value? a b))))
                (fail! :compile :ambiguous-enum-values path)))
      :multi (do
               (when-not (keyword? (:dispatch props))
                 (fail! :compile :unsupported-dispatch path))
               (doseq [[tag entry child] children]
                 (when (= :malli.core/default tag)
                   (fail! :compile :unsupported-dispatch path))
                 (when (some #(not= :description %) (keys entry))
                   (fail! :compile :unsupported-properties (conj path tag)))
                 (check-schema! child (conj path tag) false)
                 (let [field (when (= :map (m/type child))
                               (some #(when (= (:dispatch props) (first %)) %) (m/children child)))
                       [_ field-props literal] field]
                   (when-not (and field (not (:optional field-props))
                                  (contains? #{:= :enum} (m/type literal))
                                  (= [tag] (vec (m/children literal))))
                     (fail! :compile :nonliteral-dispatch (conj path tag))))))
      :or (doseq [[i child] (map-indexed vector children)]
            (check-schema! child (conj path :anyOf i) false typed-keys?))
      :maybe (check-schema! (first children) path false)
      :vector (check-schema! (first children) (conj path :items) false)
      :map-of (do (check-schema! (first children) (conj path :keys) false
                                (tagged-dictionary-keys? (first children) (conj path :keys)))
                  (check-schema! (second children) (conj path :values) false))
      :map (do
             (when-not (true? (:closed props)) (fail! :compile :open-object path))
             (doseq [[k entry child] children]
               (when (some #(not (contains? #{:optional :description} %)) (keys entry))
                 (fail! :compile :unsupported-properties (conj path k)))
               (check-schema! child (conj path k) (:optional entry))))
      (fail! :compile :unsupported-shape path)))))

(defn- json-name [x]
  (cond (keyword? x) (subs (str x) 1) (string? x) x
        :else (fail! :compile :unsupported-key [])))

(defn- assign-wire-names [entries]
  (let [counts (frequencies (map :wire-name entries))
        reserved (set (keys counts))]
    (loop [remaining entries assigned [] used reserved ordinal 0]
      (if-let [entry (first remaining)]
        (if (= 1 (counts (:wire-name entry)))
          (recur (next remaining) (conj assigned entry) used ordinal)
          (let [[alias next-ordinal]
                (loop [n ordinal]
                  (let [candidate (str "__sio_key_" n)]
                    (if (contains? used candidate) (recur (inc n)) [candidate (inc n)])))
                key (:key entry)]
            (recur (next remaining)
                   (conj assigned (assoc entry :wire-name alias
                                         :wire-identity-description
                                         (str "Original map key (" (if (keyword? key) "keyword" "string")
                                              "): " (pr-str key) ".")))
                   (conj used alias) next-ordinal)))
        assigned))))

(declare compile-node decode-node)

(defn- wire-type [value]
  (cond (nil? value) :null (string? value) :string (boolean? value) :boolean
        (number? value) :number (map? value) :object (vector? value) :array))

(defn- wire-types [node]
  (case (:kind node)
    (:enum :=) (set (map #(wire-type (enum-wire-value % []))
                    (m/children (m/schema (:original node)))))
    (:or :multi) (apply set/union (map wire-types (:branches node)))
    :maybe (conj (wire-types (:child node)) :null)
    (:int :double :float) #{:number}
    :nil #{:null}
    :map #{:object}
    (:vector :map-of) #{:array}
    :string #{:string}
    :boolean #{:boolean}))

(defn- disjoint? [left right]
  (cond
    (contains? #{:or :multi} (:kind left)) (every? #(disjoint? % right) (:branches left))
    (contains? #{:or :multi} (:kind right)) (every? #(disjoint? left %) (:branches right))
    (= :maybe (:kind left)) (and (not (contains? (wire-types right) :null))
                                (disjoint? (:child left) right))
    (= :maybe (:kind right)) (disjoint? right left)
    (empty? (set/intersection (wire-types left) (wire-types right))) true
    (and (contains? #{:enum :=} (:kind left)) (contains? #{:enum :=} (:kind right)))
    (not-any? true? (for [a (m/children (m/schema (:original left)))
                         b (m/children (m/schema (:original right)))]
                     (same-json-value? (enum-wire-value a []) (enum-wire-value b []))))
    (and (= :map (:kind left)) (= :map (:kind right)))
    (let [left-entries (into {} (map (juxt :wire-name identity) (:entries left)))
          right-entries (into {} (map (juxt :wire-name identity) (:entries right)))]
      ;; Strict objects require every wire slot and forbid additional slots.
      (or (not= (set (keys left-entries)) (set (keys right-entries)))
          (boolean (some (fn [[name entry]]
                           (let [other (get right-entries name)]
                             (and other (not (:optional entry)) (not (:optional other))
                                  (disjoint? (:node entry) (:node other))))) left-entries))))
    :else false))

(defn- compile-union [forms path]
  (let [branches (mapv #(compile-node %1 (conj path :anyOf %2)) forms (range))]
    (doseq [i (range (count branches)) j (range i)]
      (when-not (disjoint? (nth branches i) (nth branches j))
        (fail! :compile :ambiguous-union path)))
    {:branches branches :schema {:anyOf (mapv :schema branches)}}))

(defn- decode-union [node value path]
  (let [matches (keep (fn [branch]
                        (try
                          (let [decoded (decode-node branch value path)]
                            (when (m/validate (:original branch) decoded) [decoded]))
                          (catch clojure.lang.ExceptionInfo error
                            (if (= :parse (:phase (ex-data error))) nil (throw error)))))
                      (:branches node))]
    (case (count matches)
      0 (fail! :parse :no-matching-branch path)
      1 (ffirst matches)
      (fail! :parse :ambiguous-union path))))

(defn- nullable [schema]
  {:anyOf [schema {:type "null"}]})

(defn- presence-schema [schema]
  {:anyOf [{:type "object" :additionalProperties false
            :required ["presence"]
            :properties {"presence" {:enum ["absent"]}}
            :description "The original optional field is absent. Do not supply a value."}
           {:type "object" :additionalProperties false
            :required ["presence" "value"]
            :properties (array-map "presence" {:enum ["present"]} "value" schema)
            :description "The original field is present. Supply its exact value, including explicit null."}]})

(defn- decode-field [node value optional presence? path]
  ;; A vector represents presence without conflating [nil] with absence.
  (if presence?
    (do
      (when-not (map? value) (fail! :parse :expected-presence-object path))
      (case (get value "presence")
        "absent" (if (= #{"presence"} (set (keys value))) []
                     (fail! :parse :invalid-presence path))
        "present" (if (= #{"presence" "value"} (set (keys value)))
                    [(decode-node node (get value "value") (conj path "value"))]
                    (fail! :parse :invalid-presence path))
        (fail! :parse :invalid-presence path)))
    (if (and optional (nil? value)) [] [(decode-node node value path)])))

(defn- ordered-properties [entries]
  ;; Construct in one step: repeated assoc/into promotes array maps to hash
  ;; maps after eight fields and loses the author's output order.
  (apply array-map (mapcat identity entries)))

(defn- typed-key-entries [form path inherited-description]
  (let [schema (m/schema form) kind (m/type schema)
        props (m/properties schema) children (m/children schema)
        description (:description (with-description {:description inherited-description}
                                                     (:description props)))]
    (if (= :or kind)
      (mapcat #(typed-key-entries % path description) children)
      (map (fn [option]
             (let [wire (typed-key-wire option path)]
               {:value option :wire wire
                :schema (-> {:type "object" :additionalProperties false
                             :required ["type" "value"]
                             :properties (array-map "type" {:enum [(get wire "type")]}
                                                    "value" {:enum [(get wire "value")]})}
                            (with-description (str "Original dictionary key (" (get wire "type")
                                                   "): " (pr-str option) "."))
                            (with-description description)
                            (with-description (get (:descriptions props) option))
                            (with-description
                              (when (true? (:ordered-bands props))
                                (str "Ordered bands in declared order: "
                                     (json/write-str (mapv #(typed-key-wire % path) children)) "."))))}))
           children))))

(defn- compile-dictionary-key [form path]
  (if (tagged-dictionary-keys? form path)
    (let [entries (vec (typed-key-entries form path nil))]
      ;; Repeated or numerically indistinguishable tagged literals remain
      ;; unsupported. Never choose a domain value by declaration order.
      (when (collisions? same-typed-key? (mapv :wire entries))
        (fail! :compile :ambiguous-dictionary-key-values path))
      {:kind :typed-dictionary-key :original form :key-encoding :typed-literals-v1
       :entries entries :schema {:anyOf (mapv :schema entries)}})
    (compile-node form path)))

(defn- compile-node [form path]
  (let [s (m/schema form) kind (m/type s) children (m/children s) props (m/properties s)
        node (case kind
               :or (compile-union children path)
               :multi (let [union (compile-union (mapv #(nth % 2) children) path)]
                        (update-in union [:schema :anyOf]
                          #(mapv (fn [schema [_ entry _]]
                                   (with-description schema (:description entry))) % children)))
               := {:schema {:enum [(enum-wire-value (first children) path)]}}
               :map (let [original-entries
                          (mapv (fn [[k p child]]
                                  (cond-> {:key k :wire-name (json-name k) :optional (:optional p)
                                           :description (:description p)
                                           :node (compile-node child (conj path k))}
                                    (and (:optional p) (m/validate child nil))
                                    (assoc :presence-encoding :tagged-v1))) children)
                          entries (assign-wire-names original-entries)
                          properties
                          (ordered-properties
                            (map (fn [{:keys [wire-name optional description node
                                             presence-encoding wire-identity-description]}]
                                   [wire-name
                                    (-> (cond presence-encoding (presence-schema (:schema node))
                                              optional (nullable (:schema node))
                                              :else (:schema node))
                                        (with-description description)
                                        (with-description wire-identity-description))]) entries))]
                      (cond-> {:entries entries
                               :schema {:type "object" :additionalProperties false
                                        :required (mapv :wire-name entries) :properties properties}}
                        (not= original-entries entries) (assoc :key-encoding :aliases-v1)))
               :map-of (let [[key-form value-form] children
                             key-node (compile-dictionary-key key-form (conj path :keys))
                             value-node (compile-node value-form (conj path :values))]
                         {:key-node key-node :value-node value-node :map-encoding :entries-v1
                          :schema (cond-> {:type "array"
                                           :items {:type "object" :additionalProperties false
                                                   :required ["key" "value"]
                                                   :properties (array-map "key" (:schema key-node)
                                                                          "value" (:schema value-node))}
                                           :description "Dictionary entries. Preserve exact typed keys and values; each decoded key must be unique. Entry order has no domain meaning."}
                                    (contains? props :min) (assoc :minItems (:min props))
                                    (contains? props :max) (assoc :maxItems (:max props)))})
               :vector (let [child (compile-node (first children) (conj path :items))]
                         {:child child :schema (assoc (sio/malli-spec->json-schema form) :items (:schema child))})
               :maybe (let [child (compile-node (first children) path)]
                        {:child child :schema (nullable (:schema child))})
               :enum {:schema (-> (dissoc (sio/malli-spec->json-schema form) :description)
                                  (with-description (enum-meanings props children path))
                                  (with-description
                                    (when (true? (:ordered-bands props))
                                      (str "Ordered bands in declared order: "
                                           (json/write-str (mapv #(enum-wire-value % path) children))
                                           "."))))}
               ;; Emit authored JSON numeric constraints only. Machine-range
               ;; acceptance remains an exact local decoding/Malli obligation;
               ;; do not synthesize unrequested extrema into the provider contract.
               {:schema (sio/malli-spec->json-schema form)})]
    (-> (assoc node :kind kind :original form)
        (update :schema with-description (:description props)))))

(defn- decode-node [node value path]
  (case (:kind node)
    :typed-dictionary-key
    (do
      (when-not (and (map? value) (= #{"type" "value"} (set (keys value))))
        (fail! :parse :invalid-dictionary-key path))
      (if-let [entry (some #(when (same-typed-key? (:wire %) value) %) (:entries node))]
        (:value entry)
        (fail! :parse :invalid-dictionary-key path)))
    (:or :multi) (decode-union node value path)
    :map (do
           (when-not (map? value) (fail! :parse :expected-object path))
           (let [declared (set (map :wire-name (:entries node)))]
             (when (some #(not (contains? declared %)) (keys value))
               (fail! :parse :undeclared-property path)))
           (into {} (keep (fn [{:keys [key wire-name optional node presence-encoding]}]
                            (let [p (conj path wire-name)]
                              (when-not (contains? value wire-name) (fail! :parse :missing-property p))
                              (when-let [[decoded] (seq (decode-field node (get value wire-name) optional presence-encoding p))]
                                [key decoded])))) (:entries node)))
    :map-of (do
              (when-not (vector? value) (fail! :parse :expected-entry-array path))
              (let [pairs (mapv (fn [i entry]
                                  (let [p (conj path i)]
                                    (when-not (and (map? entry) (= #{"key" "value"} (set (keys entry))))
                                      (fail! :parse :invalid-map-entry p))
                                    [(decode-node (:key-node node) (get entry "key") (conj p "key"))
                                     (decode-node (:value-node node) (get entry "value") (conj p "value"))]))
                                (range) value)]
                (when-not (= (count pairs) (count (set (map first pairs))))
                  (fail! :parse :duplicate-map-key path))
                (into {} pairs)))
    :vector (do (when-not (vector? value) (fail! :parse :expected-array path))
                (mapv (fn [i v] (decode-node (:child node) v (conj path i))) (range) value))
    :maybe (when-not (nil? value) (decode-node (:child node) value path))
    :int (if (instance? java.math.BigDecimal value)
           (try (.longValueExact ^java.math.BigDecimal value)
                (catch ArithmeticException _ (fail! :parse :invalid-value path)))
           value)
    (:double :float) (if (number? value)
                       (let [v (double value)]
                         (when-not (Double/isFinite v) (fail! :parse :invalid-value path))
                         v)
                       value)
    (:enum :=) (let [options (m/children (m/schema (:original node)))]
            (if-let [match (some (fn [option]
                                  (when (same-json-value? (enum-wire-value option path) value)
                                    [option])) options)]
              (first match)
              (fail! :parse :invalid-value path)))
    value))

(defn compile-contract
  "Compile original SIO :outputs to :schema and opaque paired decoding plans.

  Supports scalar strings/booleans/numbers/null/enums, nullable values, vectors
  and explicitly closed finite maps. Dynamic :map-of uses a typed array of
  closed key/value entries (:map-encoding :entries-v1), preserving supported
  key/value schemas and cardinality. Duplicate decoded keys fail before map
  construction; key uniqueness is enforced locally, not by JSON uniqueItems.
  Finite dictionary key domains (:enum, :=, or finite :or combinations) whose
  distinct keys share a JSON literal use closed {type,value} key objects with
  :key-encoding :typed-literals-v1. Tags distinguish keyword, string, number,
  boolean and null identities; the paired plan restores the declared literal.
  Ambiguity within one tag (including equal JSON numeric values) is rejected.
  Unambiguous dictionaries retain their original key wire shape. This encoding
  does not enable ambiguous enums outside dictionary keys.
  Disjoint :or branches compile to anyOf;
  finite :multi branches require a keyword field dispatcher and a required
  matching scalar := or single-value :enum tag in each closed map. Branch
  descriptions are retained. Arbitrary dispatch functions/defaults are rejected.
  Disjointness is conservative: distinct JSON types, nonoverlapping literal
  enums, or closed records separated by wire keys/required disjoint fields.
  Unproven overlaps fail compilation rather than guess a decoding branch.
  Optional non-null fields use required
  nullable wire slots. Optional nullable fields use tagged closed presence
  objects: {presence: absent} or {presence: present, value: original-value}.
  Their opaque field/entry plans carry :presence-encoding :tagged-v1; existing
  supported fields retain their wire shape and metadata. Missing wire slots
  never become absent implicitly.
  Enum :descriptions maps annotate known option IDs with nonblank meanings;
  partial maps are allowed. Meanings join field/schema/entry descriptions in
  the wire schema and guide without changing enum membership or decoding.
  Enum :ordered-bands must be boolean: true describes bands in declared order;
  false retains the ordinary enum contract. Neither changes values or decoding.
  Unsupported shapes/properties fail rather than silently losing constraints.
  Output wire names follow SIO's name convention; top-level collisions fail.
  Nested fixed-map key collisions receive deterministic local aliases, reserving
  every authored wire name. Noncolliding names are unchanged; aliased nodes carry
  :key-encoding :aliases-v1 and exact original keys in the paired entry plan.

  Rejections are ExceptionInfo with :failure-kind :unsupported-output-schema,
  :phase :compile, a :reason keyword and :path. The returned value must be passed
  unchanged to parse-output. Send :schema as the provider response format and
  output-guide as model-visible text; internal decoding plans stay local."
  [spec]
  (let [names (mapv #(output-name (:name %)) (:outputs spec))
        _ (when-not (= (count names) (count (set names)))
            (fail! :compile :duplicate-output-name []))
        fields (mapv (fn [{:keys [name spec optional description] :as field}]
                       (check-schema! spec [(clojure.core/name name)] optional)
                       (let [node (compile-node spec [(clojure.core/name name)])
                             presence? (and optional (m/validate spec nil))]
                         (cond-> (assoc field :node node :wire-name (clojure.core/name name)
                                           :wire-schema (with-description
                                                          (cond presence? (presence-schema (:schema node))
                                                                optional (nullable (:schema node))
                                                                :else (:schema node))
                                                          description))
                           presence? (assoc :presence-encoding :tagged-v1))))
                     (:outputs spec))]
    {:schema {:type "object" :additionalProperties false
              :properties (ordered-properties (map (juxt :wire-name :wire-schema) fields))
              :required (mapv :wire-name fields)}
     :fields fields}))

(defn output-guide
  "Render the compiled wire contract for model-visible messages. A provider's
   constrained decoder need not expose schema descriptions to its model; callers
   send this guide as well as the identical schema in response-format."
  [compiled]
  (str "Output contract (JSON Schema):\n"
       (json/write-str (:schema compiled))
       "\nReturn one JSON object conforming to this contract. Follow its field descriptions; use null only where the contract permits it."))

(defn- restore-json-keys [value path]
  ;; data.json normally overwrites duplicate object keys. Give each key
  ;; occurrence an internal identity while reading, then reject duplicates
  ;; before constructing the ordinary JSON object used by the paired decoder.
  (cond
    (map? value)
    (let [names (map (comp first key) value)]
      (when-not (= (count names) (count (set names)))
        (fail! :parse :duplicate-json-property path))
      (into {} (map (fn [[[name _] child]]
                      [name (restore-json-keys child (conj path name))])) value))
    (vector? value) (mapv #(restore-json-keys %2 (conj path %1)) (range) value)
    :else value))

(defn parse-output
  "Consume one complete JSON object using the compiled contract, then validate
  original values. All wire slots must occur, even optional sentinel slots.
  Undeclared keys, invalid values and trailing data fail. Required null remains
  present; optional nonnull sentinel null becomes absence. Tagged optional
  nullable fields preserve absent, present-null and present-value independently.
  JSON numbers normalize to
  declared floating types, and JSON enum strings recover keyword enum values.
  Only aliases declared by the compiled plan are decoded. No inferred aliases,
  string-to-number coercion or array wrapping is performed. Duplicate JSON
  properties reject before any object property can be overwritten.

  Rejections are ExceptionInfo with :failure-kind :schema-validation-failed,
  :phase :parse, a :reason keyword and :path. Provider terminal/refusal status
  must be checked by the calling adapter before invoking this content parser."
  [compiled content]
  (let [parsed (try
                 (when-not (string? content) (fail! :parse :invalid-json []))
                 (let [occurrence (volatile! 0)]
                   (restore-json-keys
                     (json/read-str (str content "\n")
                                    :bigdec true
                                    :key-fn (fn [name] [name (vswap! occurrence inc)])
                                    :extra-data-fn (fn [value reader]
                                                     (if (str/blank? (slurp reader)) value
                                                         (fail! :parse :trailing-data []))))
                     []))
                 (catch clojure.lang.ExceptionInfo e (throw e))
                 (catch Exception _ (fail! :parse :invalid-json [])))
        declared (set (map :wire-name (:fields compiled)))]
    (when-not (map? parsed) (fail! :parse :expected-object []))
    (when (some #(not (contains? declared %)) (keys parsed))
      (fail! :parse :undeclared-property []))
    (into {}
          (keep (fn [{:keys [name spec optional wire-name node presence-encoding]}]
                  (when-not (contains? parsed wire-name)
                    (fail! :parse :missing-property [wire-name]))
                  (when-let [[decoded] (seq (decode-field node (get parsed wire-name) optional
                                                        presence-encoding [wire-name]))]
                    (when-not (m/validate spec decoded) (fail! :parse :invalid-value [wire-name]))
                    [name decoded])))
          (:fields compiled))))
