(ns sio.input-identity-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [malli.core :as m]
            [sio.core :as sio]
            [sio.input-identity :as input]))

(deftest mixed-typed-fixed-keys-preserve-values-and-meanings
  (let [fields [{:name :packet :spec [:map
                                     [:same {:optional true :description "Keyword-owned label."} :string]
                                     ["same" {:optional true :description "String-owned count."} :int]]}]
        native {:packet {:same "keyword" "same" 0}}
        prepared (input/prepare-inputs fields native)
        entries (drop 1 (:spec (first (:inputs prepared))))
        names (mapv first entries)
        rendered (json/read-str (get-in prepared [:values :packet]))
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (= 2 (count (set (map name names)))))
    (is (= #{"keyword" 0} (set (vals rendered))))
    (is (= rendered (get-in prepared [:native-values :packet])))
    (is (re-find #"Keyword-owned label" guide))
    (is (re-find #"String-owned count" guide))
    (is (re-find #"Original input key.*keyword" guide))
    (is (re-find #"Original input key.*string" guide))
    (is (= [:same "same"] (mapv first (drop 1 (:spec (first fields))))))
    (is (= {:packet {:same "keyword" "same" 0}} native))))

(deftest only-present-collision-key-is-shown
  (let [fields [{:name :packet :spec [:map
                                     [:same {:optional true :description "Only keyword meaning."} [:maybe :string]]
                                     ["same" {:optional true :description "Only string meaning."} :int]]}]]
    (doseq [[value expected absent] [[{:same nil} "Only keyword meaning" "Only string meaning"]
                                     [{"same" 0} "Only string meaning" "Only keyword meaning"]]]
      (let [prepared (input/prepare-inputs fields {:packet value})
            rendered (json/read-str (get-in prepared [:values :packet]))
            guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
        (is (= 1 (count rendered)))
        (is (= (set (vals value)) (set (vals rendered))))
        (is (.contains guide expected))
        (is (not (.contains guide absent)))))))

(deftest top-level-collisions-and-authored-aliases-are-paired
  (let [fields [{:name :a/name :spec :string :description "First qualified field."}
                {:name :b/name :spec :int :description "Second qualified field."}
                {:name "name" :spec :boolean :description "String named field."}
                {:name :__sio_input_field_0 :spec :string}]
        values {:a/name "one" :b/name 0 "name" false :__sio_input_field_0 "reserved"}
        prepared (input/prepare-inputs fields values)
        names (mapv :name (:inputs prepared))
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (= 4 (count (set (map name names)))))
    (is (= :__sio_input_field_0 (last names)))
    (is (= #{"one" 0 false "reserved"} (set (vals (:values prepared)))))
    (is (.contains guide ":a/name"))
    (is (.contains guide ":b/name"))
    (is (.contains guide "First qualified field"))
    (is (= (:values prepared) (:values (input/prepare-inputs fields values))))))

(deftest nested-nullable-vectors-reserve-keys-and-preserve-namespaces
  (let [schema [:maybe [:vector [:map
                                 [:same [:maybe :string]] ["same" :boolean]
                                 ["__sio_input_key_0" :string]
                                 [:a/id :int] [:b/id :int]]]]
        fields [{:name :packet :spec schema}]
        values {:packet [{:same nil "same" false "__sio_input_key_0" "literal" :a/id 0 :b/id 1}]}
        prepared (input/prepare-inputs fields values)
        row (first (json/read-str (get-in prepared [:values :packet])))]
    (is (= 5 (count row)))
    (is (= "literal" (get row "__sio_input_key_0")))
    (is (= 0 (get row "a/id")))
    (is (= 1 (get row "b/id")))
    (is (= #{nil false "literal" 0 1} (set (vals row))))
    (is (= "null" (get-in (input/prepare-inputs fields {:packet nil}) [:values :packet])))
    (is (= {} (:values (input/prepare-inputs fields {}))))))

(deftest ordinary-json-and-images-preserve-behavior
  (let [picture (Object.)
        fields [{:name :packet :spec [:map [:a/id :int] ["note" :string] [:items [:vector :boolean]]]}
                {:name :photo :type :image :spec :string}
                {:name :literal :spec :string}]
        values {:packet {:a/id 0 "note" "" :items [false]}
                :photo picture :literal "{\"same\":\"literal\"}"}
        prepared (input/prepare-inputs fields values)]
    (is (= {"a/id" 0 "note" "" "items" [false]}
           (json/read-str (get-in prepared [:values :packet]))))
    (is (identical? picture (get-in prepared [:values :photo])))
    (is (identical? picture (get-in prepared [:native-values :photo])))
    (is (= (second fields) (second (:inputs prepared))))
    (is (= (:literal values) (get-in prepared [:values :literal])))
    (is (= fields [{:name :packet :spec [:map [:a/id :int] ["note" :string] [:items [:vector :boolean]]]}
                  {:name :photo :type :image :spec :string} {:name :literal :spec :string}]))))

(deftest missing-and-explicit-null-have-different-guide-visibility
  (let [fields [{:name :packet :spec [:map
                                     [:same {:optional true :description "Null-capable keyword label."} [:maybe :string]]
                                     ["same" {:optional true :description "String neighbor."} :string]]}]
        absent (input/prepare-inputs fields {:packet {}})
        present (input/prepare-inputs fields {:packet {:same nil}})]
    (is (not (.contains (sio/input-field-guide (:inputs absent) (:values absent)) "Null-capable keyword label")))
    (is (.contains (sio/input-field-guide (:inputs present) (:values present)) "Null-capable keyword label"))
    (is (not (.contains (sio/input-field-guide (:inputs present) (:values present)) "String neighbor")))))

(deftest unknown-open-object-alias-does-not-overwrite-a-declared-value
  (let [fields [{:name :packet :spec [:map [:same :string] ["same" :int]]}]]
    (is (= :unplanned-object-key-collision
           (try (input/prepare-inputs fields {:packet {:same "kept" "same" 0 "__sio_input_key_0" "unknown"}})
                nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))))))

(deftest native-union-selection-keeps-mixed-key-branch-meanings
  (let [branch [:map [:kind [:enum :mixed]]
                [:same {:optional true :description "Union keyword field."} :string]
                ["same" {:optional true :description "Union string field."} :int]]
        fields [{:name :packet :spec [:vector [:or branch
                                             [:map [:kind [:enum :other]] [:note {:description "Other branch note."} :string]]]]}]
        prepared (input/prepare-inputs fields {:packet [{:kind :mixed "same" 0}
                                                        {:kind :other :note ""}]})
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (.contains guide "Union string field"))
    (is (not (.contains guide "Union keyword field")))
    (is (.contains guide "Other branch note"))
    (is (= 0 (get-in prepared [:native-values :packet 0 "__sio_input_key_1"])))
    (is (= "" (get-in prepared [:native-values :packet 1 "note"]))))
  (let [fields [{:name :packet :spec [:or [:map [:a {:optional true} :string]]
                                       [:map [:b {:optional true} :string]]]}]]
    (is (= {} (get-in (input/prepare-inputs fields {:packet {}}) [:native-values :packet])))))

(deftest concrete-null-input-has-null-type-and-value
  (let [prepared (input/prepare-inputs [{:name :nothing :spec :nil :description "Explicit absent measurement."}]
                                      {:nothing nil})
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (= "null" (sio/spec->type-str :nil)))
    (is (= "null" (get-in prepared [:values :nothing])))
    (is (.contains guide "(null)"))
    (is (.contains guide "Explicit absent measurement"))))

(deftest native-multi-dispatch-follows-aliased-field
  (let [branch (fn [tag meaning]
                 [:map [:kind [:enum tag]] ["kind" {:description "Literal string-key neighbor."} :string]
                  [:value {:description meaning} :int]])
        fields [{:name :packet :spec [:multi {:dispatch :kind}
                                     [:a (branch :a "Branch A value.")]
                                     [:b (branch :b "Branch B value.")]]}]
        prepared (input/prepare-inputs fields {:packet {:kind :b "kind" "literal" :value 0}})
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (.contains guide "Branch B value"))
    (is (not (.contains guide "Branch A value")))
    (is (.contains guide "Literal string-key neighbor"))
    (is (= #{"b" "literal" 0} (set (vals (get-in prepared [:native-values :packet])))))
    (is (= :kind (get-in fields [0 :spec 1 :dispatch])))))

(deftest mixed-dictionary-keys-keep-identity-even-one-key-present
  (let [fields [{:name :packet :spec [:map-of {:description "Exact typed dictionary."}
                                    [:or [:keyword {:description "Keyword key meaning."}]
                                         [:string {:description "String key meaning."}]]
                                    [:map [:count :int] [:note {:optional true} [:maybe :string]]]]}]]
    (doseq [native [{:same {:count 0 :note nil} "same" {:count 1}}
                    {:same {:count 0}} {"same" {:count 0}}]]
      (let [prepared (input/prepare-inputs fields {:packet native})
            entries (get-in prepared [:native-values :packet])
            guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
        (is (vector? entries))
        (is (= (count native) (count entries)))
        (is (= (set (map #(if (keyword? %) "keyword" "string") (keys native)))
               (set (map #(get-in % ["key" "type"]) entries))))
        (is (= #{"same"} (set (map #(get-in % ["key" "value"]) entries))))
        (is (.contains guide "Exact typed dictionary"))
        (is (.contains guide (if (contains? native :same) "Keyword key meaning" "String key meaning")))))))

(deftest string-dictionaries-retain-object-format-with-described-values
  (let [fields [{:name :packet :spec [:map-of
                                    [:string {:description "Exact external source ID."}]
                                    [:or [:map [:kind [:enum :a]] [:text {:description "Text branch meaning."} :string]]
                                         [:map [:kind [:enum :b]] [:count {:description "Count branch meaning."} :int]]]]}]
        prepared (input/prepare-inputs fields {:packet {"a/b" {:kind :a :text ""} "b.c" {:kind :b :count 0}}})
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (= {"a/b" {"kind" "a" "text" ""} "b.c" {"kind" "b" "count" 0}}
           (json/read-str (get-in prepared [:values :packet]))))
    (is (.contains guide "Exact external source ID"))
    (is (.contains guide "Text branch meaning"))
    (is (.contains guide "Count branch meaning"))))

(deftest typed-enum-dictionary-retains-key-meanings-and-native-types
  (let [fields [{:name :packet :spec [:map-of
                                    [:enum {:descriptions {:same "Keyword option meaning." "same" "String option meaning."}}
                                     :same "same" nil false 0 "0"]
                                    [:maybe :string]]}]
        source {:same "" "same" nil nil "nil" false "false" 0 "zero" "0" "text-zero"}
        prepared (input/prepare-inputs fields {:packet source})
        entries (get-in prepared [:native-values :packet])
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (= 6 (count entries)))
    (is (= #{["keyword" "same"] ["string" "same"] ["null" nil]
             ["boolean" false] ["integer" 0] ["string" "0"]}
           (set (map #(vector (get-in % ["key" "type"]) (get-in % ["key" "value"])) entries))))
    (is (.contains guide "Keyword option meaning"))
    (is (.contains guide "String option meaning"))
    (is (= (set (vals source)) (set (map #(get % "value") entries))))))

(deftest variant-dispatch-is-harmonized-when-only-one-branch-collides
  (let [fields [{:name :packet :spec [:multi {:dispatch :kind}
                                    [:a [:map [:kind [:enum :a]] ["kind" :string] [:n {:description "A number."} :int]]]
                                    [:b [:map [:kind [:enum :b]] [:n {:description "B number."} :int]]]]}]]
    (doseq [[value wanted unwanted] [[{:kind :a "kind" "neighbor" :n 0} "A number" "B number"]
                                      [{:kind :b :n 1} "B number" "A number"]]]
      (let [prepared (input/prepare-inputs fields {:packet value})
            guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
        (is (.contains guide wanted))
        (is (not (.contains guide unwanted)))
        (is (contains? (get-in prepared [:native-values :packet]) "__sio_input_dispatch_0"))))))

(deftest unsupported-input-ambiguities-are-explicit
  (doseq [[schema value reason]
          [[[:enum :same "same"] :same :ambiguous-input-enum]
           [[:or [:map [:same :int] ["same" {:optional true} :int]] [:map [:same :int]]]
            {:same 0} :ambiguous-input-branch]
           [[:multi {:dispatch :kind}
             [:same [:map [:kind [:enum :same]]]]
             ["same" [:map [:kind [:enum "same"]]]]] {:kind :same} :ambiguous-input-dispatch]
           [[:multi {:dispatch (fn [v] (:kind v))} [:a [:map [:kind [:enum :a]]]]]
            {:kind :a} :unsupported-input-dispatch]
           [[:map-of :string :int] {"a" "wrong"} :invalid-input-dictionary]
           [[:map-of [:vector :string] :int] {["a"] 1} :unsupported-input-key-schema]]]
    (is (= reason
           (try (input/prepare-inputs [{:name :packet :spec schema}] {:packet value}) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(deftest supplied-false-and-null-are-validated-without-requiring-absent-inputs
  (doseq [value [false nil]]
    (is (thrown? clojure.lang.ExceptionInfo
          (sio/validate-inputs [{:name :x :spec :string}] {:x value}))))
  (is (= {} (sio/validate-inputs [{:name :x :spec :string}] {})))
  (is (= {:x false} (sio/validate-inputs [{:name :x :spec :boolean}] {:x false})))
  (is (= {:x nil} (sio/validate-inputs [{:name :x :spec [:maybe :string]}] {:x nil})))
  (is (= {:x nil} (sio/validate-inputs [{:name :x :spec :string :type :image}] {:x nil}))))

(deftest stringified-mixed-map-cannot-recover-native-identity
  (is (= :native-input-required
         (try (input/prepare-inputs [{:name :packet :spec [:map [:same :string] ["same" :int]]}]
                                   {:packet "{\"same\":0}"}) nil
              (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))

(deftest colliding-union-literals-do-not-select-the-wrong-guide-branch
  (let [schema [:vector [:or
                         [:map [:kind [:enum :ready]] [:value {:optional true :description "Keyword branch value."} :int]]
                         [:map [:kind [:enum "ready"]] [:value {:optional true :description "String branch value."} :int]]]]]
    (doseq [[value wanted unwanted] [[{:kind :ready :value 0} "Keyword branch value" "String branch value"]
                                      [{:kind "ready" :value 0} "String branch value" "Keyword branch value"]]]
      (let [prepared (input/prepare-inputs [{:name :packet :spec schema}] {:packet [value]})
            guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
        (is (.contains guide wanted))
        (is (not (.contains guide unwanted)))))))

(deftest harmless-overlapping-unions-retain-all-applicable-meanings
  (let [prepared (input/prepare-inputs
                  [{:name :packet :spec [:or [:string {:description "General text."}]
                                            [:enum {:description "Literal x meaning."} "x"]]}]
                  {:packet "x"})
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (= "x" (get-in prepared [:native-values :packet])))
    (is (.contains guide "General text"))
    (is (.contains guide "Literal x meaning"))))

(deftest present-null-retains-its-applicable-union-meaning
  (let [fields [{:name :packet :spec [:map
                                     [:measurement {:optional true}
                                      [:or [:nil {:description "Explicit null measurement."}]
                                           [:enum {:description "Empty measurement label."} ""]]]]}]
        present (input/prepare-inputs fields {:packet {:measurement nil}})
        absent (input/prepare-inputs fields {:packet {}})
        guide (sio/input-field-guide (:inputs present) (:values present))]
    (is (.contains guide "Explicit null measurement"))
    (is (not (.contains guide "Empty measurement label")))
    (is (not (.contains (sio/input-field-guide (:inputs absent) (:values absent)) "Explicit null measurement")))
    (is (= {"measurement" nil} (get-in present [:native-values :packet])))))

(deftest nullable-union-retains-present-null-meaning
  (let [prepared (input/prepare-inputs
                   [{:name :packet :spec [:map [:reading [:or
                                                          [:maybe {:description "Nullable numeric reading."} :int]
                                                          [:enum {:description "Unavailable marker."} ""]]]]}]
                   {:packet {:reading nil}})
        guide (sio/input-field-guide (:inputs prepared) (:values prepared))]
    (is (.contains guide "Nullable numeric reading"))
    (is (not (.contains guide "Unavailable marker")))
    (is (= {"reading" nil} (get-in prepared [:native-values :packet])))))

(deftest inferred-host-scalars-retain-domain-values-and-use-faithful-model-types
  (doseq [[schema value type-text]
          [['integer? 900719925474099312345N "int"]
           ['integer? (java.math.BigInteger. "900719925474099312345") "int"]
           ['float? (float 1.25) "float"]
           ['decimal? 12345678901234567890.12345678901234567890M "decimal"]
           ['inst? (java.util.Date. 0) "ISO-8601 timestamp"]
           ['inst? (java.time.Instant/ofEpochMilli 0) "ISO-8601 timestamp"]
           [:uuid (java.util.UUID/fromString "123e4567-e89b-12d3-a456-426614174000") "UUID string"]]]
    (let [values {:scalar value}
          prepared (input/prepare-inputs [{:name :scalar :spec schema}] values)
          presented (get-in prepared [:native-values :scalar])]
      (is (if (number? value)
            (identical? value presented)
            (= (json/read-str (json/write-str value)) presented)))
      (is (identical? value (:scalar values)))
      (is (m/validate (:spec (first (:inputs prepared))) presented))
      (is (= type-text (sio/spec->type-str schema)))
      (is (.contains (sio/input-field-guide (:inputs prepared) (:values prepared)) type-text))
      (is (= (json/write-str value)
             (if (number? value) (str (get-in prepared [:values :scalar]))
               (get-in prepared [:values :scalar])))))))
