(ns sio.structured-output
  "A bounded strict JSON protocol compiled together with its presence decoder."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
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

(defn- enum-meanings [props children path]
  (when (contains? props :descriptions)
    (let [annotation (:descriptions props)
          options (mapv #(enum-wire-value % path) children)]
      (when-not (map? annotation)
        (fail! :compile :invalid-enum-descriptions path))
      (let [entries (mapv (fn [[id meaning]]
                            (let [wire-id (try (enum-wire-value id path)
                                              (catch clojure.lang.ExceptionInfo _
                                                (fail! :compile :invalid-enum-descriptions path)))]
                              (when-not (and (string? meaning) (not (str/blank? meaning))
                                             (some #(same-json-value? wire-id %) options))
                                (fail! :compile :invalid-enum-descriptions path))
                              [wire-id meaning])) annotation)]
        (when (some true? (for [i (range (count entries)) j (range i)]
                           (same-json-value? (first (nth entries i)) (first (nth entries j)))))
          (fail! :compile :invalid-enum-descriptions path))
        ;; Keep declared option order and exact wire IDs, including namespaces.
        (str/join "\n"
          (keep (fn [option]
                  (when-let [[id meaning] (some #(when (same-json-value? option (first %)) %) entries)]
                    (str (json/write-str id) ": " meaning))) options))))))

(defn- with-description [schema description]
  (if description
    (assoc schema :description
           (str/join "\n" (distinct (remove str/blank? [description (:description schema)]))))
    schema))

(defn- check-schema! [form path optional?]
  (let [schema (try (m/schema form) (catch Exception _ (fail! :compile :invalid-schema path)))
        kind (m/type schema)
        props (m/properties schema)
        children (m/children schema)
        allowed (case kind
                  (:string :int :double :float :vector) #{:description :min :max}
                  :map #{:description :closed}
                  :enum #{:description :descriptions :ordered-bands}
                  #{:description})]
    (when (and optional? (m/validate schema nil))
      (fail! :compile :ambiguous-optional-null path))
    (when (some #(not (contains? allowed %)) (keys props))
      (fail! :compile :unsupported-properties path))
    (case kind
      (:string :int :double :float :boolean :nil) nil
      :enum (let [values (mapv #(enum-wire-value % path) children)]
              (when (and (contains? props :ordered-bands)
                         (not (boolean? (:ordered-bands props))))
                (fail! :compile :invalid-ordered-bands path))
              (enum-meanings props children path)
              (when (some true?
                          (for [i (range (count values)) j (range i)
                                :let [a (nth values i) b (nth values j)]]
                            (same-json-value? a b)))
                (fail! :compile :ambiguous-enum-values path)))
      :maybe (check-schema! (first children) path false)
      :vector (check-schema! (first children) (conj path :items) false)
      :map (do
             (when-not (true? (:closed props)) (fail! :compile :open-object path))
             (doseq [[k entry child] children]
               (when (some #(not (contains? #{:optional :description} %)) (keys entry))
                 (fail! :compile :unsupported-properties (conj path k)))
               (check-schema! child (conj path k) (:optional entry))))
      (fail! :compile :unsupported-shape path))))

(defn- json-name [x]
  (cond (keyword? x) (subs (str x) 1) (string? x) x
        :else (fail! :compile :unsupported-key [])))

(declare compile-node)

(defn- nullable [schema]
  {:anyOf [schema {:type "null"}]})

(defn- ordered-properties [entries]
  ;; Construct in one step: repeated assoc/into promotes array maps to hash
  ;; maps after eight fields and loses the author's output order.
  (apply array-map (mapcat identity entries)))

(defn- compile-node [form path]
  (let [s (m/schema form) kind (m/type s) children (m/children s) props (m/properties s)
        node (case kind
               :map (let [entries (mapv (fn [[k p child]]
                                          {:key k :wire-name (json-name k) :optional (:optional p)
                                           :description (:description p)
                                           :node (compile-node child (conj path k))}) children)
                          names (mapv :wire-name entries)]
                      (when-not (= (count names) (count (set names)))
                        (fail! :compile :duplicate-property-name path))
                      {:entries entries
                       :schema {:type "object" :additionalProperties false :required names
                                :properties (ordered-properties (map (fn [{:keys [wire-name optional description node]}]
                                                           [wire-name (with-description
                                                                        (if optional (nullable (:schema node)) (:schema node))
                                                                        description)]) entries))}})
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
    :map (do
           (when-not (map? value) (fail! :parse :expected-object path))
           (let [declared (set (map :wire-name (:entries node)))]
             (when (some #(not (contains? declared %)) (keys value))
               (fail! :parse :undeclared-property path)))
           (into {} (keep (fn [{:keys [key wire-name optional node]}]
                            (let [p (conj path wire-name)]
                              (when-not (contains? value wire-name) (fail! :parse :missing-property p))
                              (when-not (and optional (nil? (get value wire-name)))
                                [key (decode-node node (get value wire-name) p)])))) (:entries node)))
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
    :enum (let [options (m/children (m/schema (:original node)))]
            (if-let [match (some (fn [option]
                                  (when (same-json-value? (enum-wire-value option path) value)
                                    [option])) options)]
              (first match)
              (fail! :parse :invalid-value path)))
    value))

(defn compile-contract
  "Compile original SIO :outputs to :schema and opaque paired decoding plans.

  Supports scalar strings/booleans/numbers/null/enums, nullable values, vectors
  and explicitly closed finite maps. Optional non-null fields use required
  nullable wire slots; optional already-nullable fields are ambiguous and fail.
  Enum :descriptions maps annotate known option IDs with nonblank meanings;
  partial maps are allowed. Meanings join field/schema/entry descriptions in
  the wire schema and guide without changing enum membership or decoding.
  Enum :ordered-bands must be boolean: true describes bands in declared order;
  false retains the ordinary enum contract. Neither changes values or decoding.
  Unsupported shapes/properties fail rather than silently losing constraints.
  Output wire names follow SIO's name convention; collisions fail explicitly.

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
                       (let [node (compile-node spec [(clojure.core/name name)])]
                         (assoc field :node node :wire-name (clojure.core/name name)
                                :wire-schema (with-description
                                               (if optional (nullable (:schema node)) (:schema node))
                                               description))))
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

(defn parse-output
  "Consume one complete JSON object using the compiled contract, then validate
  original values. All wire slots must occur, even optional sentinel slots.
  Undeclared keys, invalid values and trailing data fail. Required null remains
  present; optional sentinel null becomes absence. JSON numbers normalize to
  declared floating types, and JSON enum strings recover keyword enum values.
  No string-to-number coercion, array wrapping or field aliasing is performed.

  Rejections are ExceptionInfo with :failure-kind :schema-validation-failed,
  :phase :parse, a :reason keyword and :path. Provider terminal/refusal status
  must be checked by the calling adapter before invoking this content parser."
  [compiled content]
  (let [parsed (try
                 (when-not (string? content) (fail! :parse :invalid-json []))
                 (json/read-str (str content "\n")
                                :bigdec true
                                :extra-data-fn (fn [value reader]
                                                 (if (str/blank? (slurp reader)) value
                                                     (fail! :parse :trailing-data []))))
                 (catch clojure.lang.ExceptionInfo e (throw e))
                 (catch Exception _ (fail! :parse :invalid-json [])))
        declared (set (map :wire-name (:fields compiled)))]
    (when-not (map? parsed) (fail! :parse :expected-object []))
    (when (some #(not (contains? declared %)) (keys parsed))
      (fail! :parse :undeclared-property []))
    (into {}
          (keep (fn [{:keys [name spec optional wire-name node]}]
                  (when-not (contains? parsed wire-name)
                    (fail! :parse :missing-property [wire-name]))
                  (let [v (get parsed wire-name)]
                    (when-not (and optional (nil? v))
                      (let [decoded (decode-node node v [wire-name])]
                        (when-not (m/validate spec decoded) (fail! :parse :invalid-value [wire-name]))
                        [name decoded])))))
          (:fields compiled))))
