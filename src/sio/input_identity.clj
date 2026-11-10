(ns sio.input-identity
  "Paired model-view input fields and values. Domain schemas remain unchanged."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [malli.core :as m]))

(defn- reject! [reason path]
  (throw (ex-info "Input identity cannot be represented losslessly"
                  {:failure-kind :unsupported-input-identity :reason reason :path path})))

(defn- json-name [key path]
  (cond (keyword? key) (subs (str key) 1)
        (string? key) key
        :else (reject! :unsupported-object-key path)))

(defn- allocate [keys spelling prefix path]
  (let [names (mapv #(spelling % path) keys)
        counts (frequencies names)
        reserved (set names)]
    (loop [keys keys names names used reserved ordinal 0 result []]
      (if-let [items (seq keys)]
        (let [key (first items) name (first names)]
          (if (= 1 (counts name))
            (recur (next keys) (next names) used ordinal
                   (conj result {:original-key key :wire-name name :alias? false}))
            (let [[alias next-ordinal]
                  (loop [i ordinal]
                    (let [candidate (str prefix i)]
                      (if (contains? used candidate) (recur (inc i)) [candidate (inc i)])))]
              (recur (next keys) (next names) (conj used alias) next-ordinal
                     (conj result {:original-key key :wire-name alias :alias? true})))))
        result))))

(defn- description [existing key path]
  (str/join "\n" (remove str/blank?
                   [existing
                    (str "Original input key (" (if (keyword? key) "keyword" "string") "): "
                         (pr-str key) "; original path: " (pr-str path) ".")])))

(defn- own-description [schema]
  (when schema
   (let [s (m/schema schema)]
    (or (:description (m/properties s))
        (when (= :maybe (m/type s)) (own-description (first (m/children s))))))))

(defn- form [kind props children]
  (into (cond-> [kind] (some? props) (conj props)) children))

(declare compile-node encode-node)

(defn- model-map-schema [props entries path]
  (form :map props
    (mapv (fn [{:keys [original-key wire-name alias? entry node original-path]}]
            (let [entry (cond-> entry
                          alias? (assoc :description
                                        (description (or (:description entry)
                                                         (own-description (:original node)))
                                                     original-key (or original-path (conj path original-key)))))]
              (cond-> [wire-name] (seq entry) (conj entry) true (conj (:model-schema node))))) entries)))

(defn- wire-literal [value]
  (if (keyword? value) (subs (str value) 1) value))

(defn- key-type [value path]
  (cond (nil? value) "null" (keyword? value) "keyword" (string? value) "string"
        (boolean? value) "boolean" (integer? value) "integer"
        (instance? Double value) "double" (instance? Float value) "float"
        (instance? java.math.BigDecimal value) "decimal"
        :else (reject! :unsupported-input-key-type path)))

(defn- key-variants [schema path]
  (let [s (m/schema schema) kind (m/type s) props (m/properties s) children (m/children s)]
    (case kind
      :or (vec (mapcat #(key-variants (m/form %) path) children))
      :maybe (into [{:type "null" :schema :nil}] (key-variants (m/form (first children)) path))
      (:enum :=)
      (mapv (fn [type]
              (let [members (filterv #(= type (key-type % path)) children)
                    meanings (when (:descriptions props)
                               (into {} (keep (fn [member]
                                                (when-let [meaning (get (:descriptions props) member)]
                                                  [(wire-literal member) meaning]))) members))
                    props (cond-> (dissoc props :descriptions) (seq meanings) (assoc :descriptions meanings
                       :description (str/join "\n" (remove str/blank?
                         [(:description props) (str "Allowed original key meanings: "
                           (str/join "; " (map (fn [member] (str (pr-str member) " = " (get (:descriptions props) member)))
                             (filter #(contains? (:descriptions props) %) members))))]))))]
                {:type type :schema (form :enum props (mapv wire-literal members))}))
            (distinct (map #(key-type % path) children)))
      :keyword [{:type "keyword" :schema [:string {:description
                                      (str (or (:description props) "")
                                           " Literal keyword spelling, including its namespace, without the leading colon.")}]}]
      :string [{:type "string" :schema schema}]
      :int [{:type "integer" :schema schema}]
      :nil [{:type "null" :schema schema}]
      :boolean [{:type "boolean" :schema schema}]
      (:double :float :number)
      (mapv (fn [type] {:type type :schema schema}) ["integer" "double" "float" "decimal"])
      (reject! :unsupported-input-key-schema path))))

(defn- typed-key-schema [variants]
  (let [branches
        (mapv (fn [type]
                (let [schemas (vec (distinct (map :schema (filter #(= type (:type %)) variants))))]
                  [:map {:closed true}
                   ["type" {:description "Exact original host key type; do not merge keys of different types."} [:enum type]]
                   ["value" (if (= 1 (count schemas)) (first schemas) (into [:or] schemas))]]))
              (distinct (map :type variants)))]
    (if (= 1 (count branches)) (first branches) (into [:or] branches))))

(defn- compile-node [schema path]
  ;; Recurse over compiled children so inherited/local registries survive.
  ;; Emit ordinary forms for legacy scalar rendering; named nodes keep their
  ;; compiled context for the request guide's separate declared-contract view.
  (let [s (m/schema schema) kind (m/type s) props (m/properties s) children (m/children s)]
    (case kind
      (inst? :uuid)
      {:kind :json-string :original schema
       :model-schema [:string (update props :description
                               #(str/join "\n" (remove str/blank?
                                   [% (if (= kind :uuid) "Canonical UUID string."
                                          "Canonical ISO-8601 timestamp string.")])))]}
      :map
      (let [names (allocate (mapv first children) json-name "__sio_input_key_" path)
            entries (mapv (fn [[key entry child] identity]
                            (assoc identity :original-path (conj path key) :entry entry :node (compile-node child (conj path key))))
                          children names)]
        {:kind :map :original schema :entries entries
         :model-schema (model-map-schema props entries path)})
      (:enum :=)
      (let [wire-values (mapv wire-literal children)
            _ (when-not (= (count wire-values) (count (set wire-values)))
                (reject! :ambiguous-input-enum path))
            props (cond-> props (:descriptions props)
                    (update :descriptions #(into {} (map (fn [[key value]] [(wire-literal key) value])) %)))]
        {:kind :identity :original schema :model-schema (form kind props wire-values)})
      :multi
      (let [dispatch (:dispatch props)
            _ (when-not (keyword? dispatch) (reject! :unsupported-input-dispatch path))
            branches (mapv (fn [[tag entry child]]
                             (when (= :malli.core/default tag) (reject! :unsupported-input-dispatch path))
                             {:tag tag :wire-tag (wire-literal tag) :entry entry
                              :node (compile-node child (conj path tag))}) children)
            tags (mapv :wire-tag branches)
            _ (when-not (= (count tags) (count (set tags))) (reject! :ambiguous-input-dispatch path))
            dispatch-entry (fn [branch] (some #(when (= dispatch (:original-key %)) %) (get-in branch [:node :entries])))
            _ (when-not (every? dispatch-entry branches) (reject! :unsupported-input-dispatch path))
            wires (set (map (comp :wire-name dispatch-entry) branches))
            reserved (set (mapcat #(map :wire-name (get-in % [:node :entries])) branches))
            wire (if (= 1 (count wires)) (first wires)
                   (first (remove reserved (map #(str "__sio_input_dispatch_" %) (range)))))
            branches (mapv (fn [{:keys [node] :as branch}]
                             (let [entries (mapv #(if (= dispatch (:original-key %))
                                                    (cond-> (assoc % :wire-name wire)
                                                      (not= wire (:wire-name %)) (assoc :alias? true)) %) (:entries node))]
                               (assoc branch :node
                                      (assoc node :entries entries
                                             :model-schema (model-map-schema (m/properties (m/schema (:original node))) entries path))))) branches)
            model-dispatch (with-meta (fn [value] (get value wire)) {:sio.input-identity/dispatch-key wire})]
        {:kind :multi :original schema :dispatch dispatch :branches branches
         :model-schema (form :multi (assoc props :dispatch model-dispatch)
                             (mapv (fn [{:keys [wire-tag entry node]}]
                                     (cond-> [wire-tag] (seq entry) (conj entry) true (conj (:model-schema node)))) branches))})
      :map-of
      (let [[key-schema value-schema] children
            key-form (m/form key-schema)
            variants (key-variants key-form (conj path :keys))
            typed? (not= #{"string"} (set (map :type variants)))
            value-node (compile-node value-schema (conj path :values))
            key-meaning (own-description key-form)
            props (update props :description
                          #(str/join "\n" (remove str/blank?
                               [% (when key-meaning (str "Dictionary key meaning: " key-meaning))
                                (when typed? "Typed dictionary entries preserve key identity; entry order is not meaningful.")])))]
        {:kind :map-of :original schema :typed-keys? typed? :key-schema key-form
         :value-node value-node :key-variants variants
         :model-schema (if typed?
                         (form :vector props
                           [[:map {:closed true}
                             ["key" (typed-key-schema variants)]
                             ["value" (:model-schema value-node)]]])
                         (form :map-of props [key-form (:model-schema value-node)]))})
      :or
      (let [branches (mapv #(compile-node % (conj path :branch %2)) children (range))]
        {:kind :or :original schema :branches branches
         :model-schema (form :or props (mapv :model-schema branches))})
      (:maybe :vector :sequential :set)
      (let [child (compile-node (first children) (conj path :items))]
        {:kind kind :original schema :child child
         :model-schema (form kind props [(:model-schema child)])})
      ;; Unknown shapes retain their schema, but encoding still refuses raw
      ;; object-key collisions instead of silently losing an actual value.
      {:kind :identity :original schema
       :model-schema (if (#{:schema :ref :malli.core/schema} kind) s (m/form s))})))

(defn- encode-generic [value path]
  (cond
    (map? value)
    (let [pairs (mapv (fn [[key child]] [(json-name key path) (encode-generic child (conj path key))]) value)]
      (when-not (= (count pairs) (count (set (map first pairs))))
        (reject! :unplanned-object-key-collision path))
      (into {} pairs))
    (coll? value) (mapv #(encode-generic %2 (conj path %1)) (range) value)
    (keyword? value) (subs (str value) 1)
    :else value))

(defn- encode-node [plan value path]
  (case (:kind plan)
    :json-string (json/read-str (json/write-str value))
    :map
    (if-not (map? value) (reject! :native-input-required path)
      (let [declared (into {} (map (juxt :original-key identity) (:entries plan)))
            pairs (mapv (fn [[key child]]
                          (if-let [entry (get declared key)]
                            [(:wire-name entry) (encode-node (:node entry) child (conj path key))]
                            [(json-name key path) (encode-generic child (conj path key))])) value)]
        ;; Open objects may contain undeclared keys that occupy an alias.
        ;; Do not silently overwrite them or pretend they were declared.
        (when-not (= (count pairs) (count (set (map first pairs))))
          (reject! :unplanned-object-key-collision path))
        (into {} pairs)))
    :map-of
    (do
      (when-not (and (map? value) (m/validate (:original plan) value))
        (reject! :invalid-input-dictionary path))
      (if (:typed-keys? plan)
        (mapv (fn [[key child]]
                {"key" {"type" (key-type key (conj path :keys)) "value" (wire-literal key)}
                 "value" (encode-node (:value-node plan) child (conj path key))}) value)
        (into {} (map (fn [[key child]]
                        [key (encode-node (:value-node plan) child (conj path key))])) value)))
    :multi
    (let [tag (get value (:dispatch plan))
          branch (some #(when (= tag (:tag %)) %) (:branches plan))]
      (when-not (and branch (m/validate (:original plan) value))
        (reject! :no-matching-input-branch path))
      (encode-node (:node branch) value path))
    :or
    (let [indices (into [] (keep-indexed #(when (m/validate (:original %2) value) %1)) (:branches plan))
          outputs (mapv #(encode-node (nth (:branches plan) %) value path) indices)]
      (when (empty? indices) (reject! :no-matching-input-branch path))
      (when-not (apply = outputs) (reject! :ambiguous-input-branch path))
      (if (:tagged? plan) {"branch" (pr-str indices) "value" (first outputs)} (first outputs)))
    :maybe (when-not (nil? value) (encode-node (:child plan) value path))
    (:vector :sequential :set)
    (if (coll? value)
      (mapv #(encode-node (:child plan) %2 (conj path %1)) (range) value)
      (encode-generic value path))
    (encode-generic value path)))

(defn- matching-indices [branches value]
  (into [] (keep-indexed #(when (m/validate (:original %2) value) %1)) branches))

(defn- specialize-plan [plan observed path]
  (let [props (m/properties (m/schema (:model-schema plan)))
        specialize #(specialize-plan %1 %2 path)]
    (case (:kind plan)
      :map
      (let [entries (mapv (fn [{:keys [original-key node] :as entry}]
                            (assoc entry :node (specialize node (map #(get % original-key)
                                                               (filter #(and (map? %) (contains? % original-key)) observed))))) (:entries plan))]
        (assoc plan :entries entries :model-schema (model-map-schema props entries path)))
      (:vector :sequential :set :maybe)
      (let [values (if (= :maybe (:kind plan)) (remove nil? observed) (mapcat #(if (coll? %) % []) observed))
            child (specialize (:child plan) values)]
        (assoc plan :child child :model-schema (form (:kind plan) props [(:model-schema child)])))
      :map-of
      (let [child (specialize (:value-node plan) (mapcat vals (filter map? observed)))
            schema (if (:typed-keys? plan)
                     (form :vector props [[:map {:closed true} ["key" (typed-key-schema (:key-variants plan))]
                                           ["value" (:model-schema child)]]])
                     (form :map-of props [(:key-schema plan) (:model-schema child)]))]
        (assoc plan :value-node child :model-schema schema))
      :multi
      (let [branches (mapv (fn [branch]
                            (update branch :node specialize
                                    (filter #(= (:tag branch) (get % (:dispatch plan))) observed))) (:branches plan))]
        (assoc plan :branches branches :model-schema
               (form :multi props (mapv (fn [{:keys [wire-tag entry node]}]
                                         (cond-> [wire-tag] (seq entry) (conj entry) true (conj (:model-schema node)))) branches))))
      :or
      (let [branches (mapv (fn [branch] (specialize branch (filter #(m/validate (:original branch) %) observed))) (:branches plan))
            selections (mapv #(matching-indices branches %) observed)
            encoded (mapv (fn [value indices]
                            (when (empty? indices) (reject! :no-matching-input-branch path))
                            (let [outputs (mapv #(encode-node (nth branches %) value path) indices)]
                              (when-not (apply = outputs) (reject! :ambiguous-input-branch path))
                              (first outputs))) observed selections)
            collision? (some true? (map (fn [value indices]
                                         (some #(and (not (contains? (set indices) %))
                                                     (m/validate (:model-schema (nth branches %)) value))
                                               (range (count branches)))) encoded selections))
            meanings (distinct (keep #(own-description (:original (nth branches %))) (mapcat identity selections)))
            props (cond-> props (seq meanings) (update :description #(str/join "\n" (remove str/blank? (cons % meanings)))))
            schema (if collision?
                     (form :or props
                       (mapv (fn [indices]
                               [:map {:closed true}
                                ["branch" {:description "Original native union branches applicable to this value."} [:enum (pr-str indices)]]
                                ["value" (if (= 1 (count indices)) (:model-schema (nth branches (first indices)))
                                             (into [:or] (map #(:model-schema (nth branches %)) indices)))]] )
                             (distinct selections)))
                     (form :or props (mapv :model-schema branches)))]
        (assoc plan :branches branches :tagged? (boolean collision?) :model-schema schema))
      plan)))

(defn prepare-inputs
  "Prepare one chat/model-display input view from original fields and NATIVE
  values, before any JSON serialization. Returns :inputs, rendered :values,
  transformed :native-values, and opaque versioned :identity-plan metadata.

  Feed :inputs and :values together to SIO's existing input-field-guide and
  request builders. Original domain schemas/values are not mutated or replaced.
  Validate domain values against original schemas before this presentation step.
  Do not pass already-stringified complex values: lost key identity cannot be
  recovered. Images remain untouched. Optional input absence remains absence;
  no output presence codec is introduced. Top-level nil renders as JSON null.

  Handles fixed maps, nullable collections, native unions, finite keyword-key
  dispatch, and scalar-key dictionaries. Actual union presentation collisions
  receive branch tags; compatible overlaps retain every applicable meaning.
  Compound dictionary keys and arbitrary dispatchers fail explicitly.
  Plans are value-specific and must be prepared again when values change.
  This is not native-decision protocol integration."
  [fields values]
  (let [names (mapv :name fields)
        _ (when-not (= (count names) (count (set names)))
            (reject! :duplicate-input-name []))
        identities (allocate names (fn [key path]
                                     (if (or (keyword? key) (string? key)) (json-name key path)
                                         (reject! :unsupported-input-name path)))
                             "__sio_input_field_" [])
        plans (mapv (fn [field identity]
                      (assoc identity :field field :model-name (if (or (:alias? identity) (and (keyword? (:name field)) (namespace (:name field))))
                                                        (:wire-name identity) (:name field))
                             :image? (= :image (:type field))
                             :node (when-not (= :image (:type field))
                                     (specialize-plan (compile-node (:spec field) [(:name field)])
                                                      (if (contains? values (:name field)) [(get values (:name field))] [])
                                                      [(:name field)])))) fields identities)
        model-fields (mapv (fn [{:keys [field model-name node alias? image?]}]
                             (cond-> (assoc field :name model-name)
                               (not image?) (assoc :spec (:model-schema node) :sio.input-identity/json-keys? true)
                               (or alias? (and (keyword? (:name field)) (namespace (:name field)))) (assoc :description (description (or (:description field) (own-description (:spec field)))
                                                                      (:name field) [(:name field)])))) plans)
        native-values (into {} (keep (fn [{:keys [original-key model-name node image?]}]
                                       (when (contains? values original-key)
                                         [model-name (if image? (get values original-key)
                                                         (encode-node node (get values original-key) [original-key]))]))) plans)
        rendered (into {} (keep (fn [{:keys [model-name image? node]}]
                                  (when (contains? native-values model-name)
                                    (let [value (get native-values model-name)]
                                      [model-name (cond image? value
                                                        (or (= :json-string (:kind node)) (coll? value) (nil? value)
                                                            (instance? java.util.Date value)
                                                            (instance? java.time.Instant value)
                                                            (instance? java.util.UUID value)) (json/write-str value)
                                                        :else value)])))) plans)]
    {:inputs model-fields :values rendered :native-values native-values
     :identity-plan {:version 1 :fields plans}}))
