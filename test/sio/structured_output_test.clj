(ns sio.structured-output-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [sio.structured-output :as structured]))

(deftest enum-option-meanings-survive-ordinary-field-description
  (let [compiled (structured/compile-contract
                  {:outputs [{:name :assessment :description "Joined acceptance assessment"
                              :spec [:enum {:description "Evidence-based choice"
                                            :descriptions {"supported" "All criteria have evidence."
                                                           "repair" "An owned artifact requires revision."}}
                                     "supported" "repair"]}]})
        schema (get-in compiled [:schema :properties "assessment"])]
    (is (= ["supported" "repair"] (:enum schema)))
    (doseq [meaning ["Joined acceptance assessment" "Evidence-based choice"
                     "All criteria have evidence." "An owned artifact requires revision."]]
      (is (str/includes? (:description schema) meaning))
      (is (str/includes? (structured/output-guide compiled) meaning)))
    (is (not (contains? schema :descriptions)))
    (is (= {:assessment "repair"}
           (structured/parse-output compiled "{\"assessment\":\"repair\"}")))))

(deftest ordered-band-enums-preserve-order-meanings-and-exact-decoding
  (let [compiled (structured/compile-contract
                  {:outputs [{:name :band :description "Assessment against the rubric"
                              :spec [:enum {:ordered-bands true
                                            :description "Select the applicable band"
                                            :descriptions {"2" "All essential criteria established."
                                                           "30" "Criteria established with supporting detail."
                                                           "10" "Material criteria remain unestablished."}}
                                     "10" "2" "30"]}]})
        schema (get-in compiled [:schema :properties "band"])
        ordered-guide "Ordered bands in declared order: [\"10\",\"2\",\"30\"]."]
    (is (= ["10" "2" "30"] (:enum schema)))
    (doseq [meaning ["Assessment against the rubric" "Select the applicable band"
                     "Material criteria remain unestablished."
                     "All essential criteria established."
                     "Criteria established with supporting detail." ordered-guide]]
      (is (str/includes? (:description schema) meaning))
      (is (str/includes? (structured/output-guide compiled)
                         (subs (json/write-str meaning) 1 (dec (count (json/write-str meaning)))))))
    (is (not (contains? schema :ordered-bands)))
    (is (not (contains? schema :descriptions)))
    (doseq [option ["10" "2" "30"]]
      (is (= {:band option}
             (structured/parse-output compiled (json/write-str {:band option})))))
    (doseq [value ["20" 2]]
      (is (= :schema-validation-failed
             (:failure-kind
              (try (structured/parse-output compiled (json/write-str {:band value}))
                   nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))

(deftest described-contract-round-trips-presence
  (let [compiled (structured/compile-contract
                  {:outputs [{:name :source-id :spec :string :description "Exact source identity"}
                             {:name :coolant-status :spec [:maybe :string]}
                             {:name :reference :spec :string :optional true}]})
        schema (:schema compiled)]
    (is (= "Exact source identity" (get-in schema [:properties "source-id" :description])))
    (is (= #{"source-id" "coolant-status" "reference"} (set (:required schema))))
    (is (false? (:additionalProperties schema)))
    (is (= {:source-id "A-1" :coolant-status nil}
           (structured/parse-output compiled "{\"source-id\":\"A-1\",\"coolant-status\":null,\"reference\":null}")))
    (is (= {:source-id "A-1" :coolant-status "cold" :reference "R-1"}
           (structured/parse-output compiled "{\"source-id\":\"A-1\",\"coolant-status\":\"cold\",\"reference\":\"R-1\"}")))))

(defn rejection [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest ordered-band-annotation-requires-a-boolean-at-its-schema-path
  (doseq [annotation [nil "true" 1 :true [] {}]]
    (is (= {:failure-kind :unsupported-output-schema :phase :compile
            :reason :invalid-ordered-bands :path ["assessments" :items :band]}
           (rejection #(structured/compile-contract
                         {:outputs [{:name :assessments
                                     :spec [:vector [:map {:closed true}
                                                     [:band [:enum {:ordered-bands annotation}
                                                             "low" "high"]]]]}]})))
        (pr-str annotation))))

(deftest false-ordered-band-annotation-retains-ordinary-enum-contract
  (let [props {:description "Available result"
               :descriptions {:result/ready "Ready for use." :result/repair "Needs revision."}}
        compile-choice (fn [properties]
                         (structured/compile-contract
                           {:outputs [{:name :choice
                                       :spec [:enum properties :result/ready :result/repair]}]}))
        ordinary (compile-choice props)
        annotated (compile-choice (assoc props :ordered-bands false))]
    (is (= (:schema ordinary) (:schema annotated)))
    (is (= (structured/output-guide ordinary) (structured/output-guide annotated)))
    (doseq [choice [:result/ready :result/repair]]
      (is (= {:choice choice}
             (structured/parse-output annotated
                                      (json/write-str {:choice (subs (str choice) 1)})))))
    (is (= :schema-validation-failed
           (:failure-kind (rejection #(structured/parse-output annotated
                                                              "{\"choice\":\"ready\"}")))))
    (doseq [unknown [{:unknown true} {:json-schema/enum ["anything"]}]]
      (is (= :unsupported-properties
             (:reason (rejection #(compile-choice (merge props {:ordered-bands true} unknown))))))))
  (is (= :unsupported-properties
         (:reason (rejection #(structured/compile-contract
                               {:outputs [{:name :choice :spec [:string {:ordered-bands true}]}]}))))))

(deftest nested-described-enums-preserve-entry-meaning-and-keyword-identities
  (let [choice [:enum {:description "Selection meaning"
                       :descriptions {:route/ready "Evidence is sufficient."
                                      "route/repair" "Revision is needed."}}
                :route/ready :route/repair]
        compiled (structured/compile-contract
                  {:outputs [{:name :results :description "Actual results"
                              :spec [:vector [:map {:closed true}
                                              [:route {:description "Owned route"} choice]
                                              [:optional-route {:optional true :description "Optional owned route"} choice]
                                              [:nullable-route {:description "Nullable owned route"} [:maybe choice]]]]}]})
        properties (get-in compiled [:schema :properties "results" :items :properties])]
    (is (= ["route/ready" "route/repair"] (get-in properties ["route" :enum])))
    (doseq [meaning ["Owned route" "Selection meaning" "Evidence is sufficient." "Revision is needed."]]
      (is (str/includes? (get-in properties ["route" :description]) meaning)))
    (doseq [field ["optional-route" "nullable-route"]]
      (is (str/includes? (get-in properties [field :description]) "owned route"))
      (is (str/includes? (get-in properties [field :anyOf 0 :description]) "Evidence is sufficient.")))
    (is (= {:results [{:route :route/ready :nullable-route :route/repair}]}
           (structured/parse-output compiled
             "{\"results\":[{\"route\":\"route/ready\",\"optional-route\":null,\"nullable-route\":\"route/repair\"}]}")))
    (is (= :schema-validation-failed
           (:failure-kind (rejection #(structured/parse-output compiled
             "{\"results\":[{\"route\":\"ready\",\"optional-route\":null,\"nullable-route\":null}]}")))))))

(deftest enum-description-metadata-is-narrow-and-fail-closed
  (doseq [annotation [nil [] "meaning" {"unknown" "Unknown choice"}
                      {:ready "Ambiguous" "ready" "Also ambiguous"}
                      {[:ready] "Invalid ID"} {:ready nil} {:ready 1} {:ready " "}]]
    (is (= {:failure-kind :unsupported-output-schema :phase :compile
            :reason :invalid-enum-descriptions :path ["assessment"]}
           (rejection #(structured/compile-contract
                         {:outputs [{:name :assessment
                                     :spec [:enum {:descriptions annotation} :ready :repair]}]})))))
  (doseq [props [{:descriptions {:ready "Ready"} :unknown true}
                {:descriptions {:ready "Ready"} :json-schema/enum ["anything"]}]]
    (is (= :unsupported-properties
           (:reason (rejection #(structured/compile-contract
                                  {:outputs [{:name :assessment :spec [:enum props :ready :repair]}]}))))))
  (let [compiled (structured/compile-contract
                  {:outputs [{:name :assessment :spec [:enum {:descriptions {:ready "Ready"}} :ready :repair]}]})]
    (is (= {:assessment :repair} (structured/parse-output compiled "{\"assessment\":\"repair\"}")))))

(deftest strict-response-is-one-complete-object
  (let [compiled (structured/compile-contract {:outputs [{:name :answer :spec [:maybe :string]}]})]
    (doseq [content ["{}" "{\"answer\":null,\"extra\":1}" "[]" "null"
                     "{\"answer\":null} {}" "{\"answer\":null} trailing"
                     "{\"answer\":" "```json\n{\"answer\":null}\n```" "{\"answer\":42}"]]
      (is (= :schema-validation-failed (:failure-kind (rejection #(structured/parse-output compiled content)))) content))
    (is (= {:answer nil} (structured/parse-output compiled " \n{\"answer\":null}\t\n")))))

(deftest ambiguous-and-unsupported-contracts-fail-before-dispatch
  (doseq [outputs [[{:name :answer :spec [:maybe :string] :optional true}]
                   [{:name :answer :spec [:map [:a :string]]}]
                   [{:name :answer :spec [:map-of :string :string]}]
                   [{:name :answer :spec :any}]
                   [{:name :answer :spec [:or :string :int]}]
                   [{:name :answer :spec [:string {:json-schema/type "integer"}]}]
                   [{:name :same :spec :string} {:name "same" :spec :string}]
                   [{:name :answer :spec [:map {:closed true} [:x {:optional true} [:maybe :int]]]}]]]
    (is (= :unsupported-output-schema
           (:failure-kind (rejection #(structured/compile-contract {:outputs outputs})))) (pr-str outputs))))

(deftest recursive-contract-retains-constraints-and-decodes-optionals
  (let [compiled (structured/compile-contract
                  {:outputs [{:name :items :spec
                              [:vector {:min 1 :max 2}
                               [:map {:closed true :description "One task"}
                                [:source-id [:string {:min 2 :description "Exact identity"}]]
                                [:route [:enum :ready :repair]]
                                [:note {:optional true :description "Optional note"} :string]
                                [:count [:maybe :int]]]]}]})
        item-schema (get-in compiled [:schema :properties "items" :items])]
    (is (= 1 (get-in compiled [:schema :properties "items" :minItems])))
    (is (= "Exact identity" (get-in item-schema [:properties "source-id" :description])))
    (is (= "Optional note" (get-in item-schema [:properties "note" :description])))
    (is (= #{"source-id" "route" "note" "count"} (set (:required item-schema))))
    (is (= [{:type "string"} {:type "null"}] (get-in item-schema [:properties "note" :anyOf])))
    (is (= {:items [{:source-id "A1" :route :ready :count nil}]}
           (structured/parse-output compiled "{\"items\":[{\"source-id\":\"A1\",\"route\":\"ready\",\"note\":null,\"count\":null}]}")))
    (doseq [content ["{\"items\":[]}"
                     "{\"items\":[{\"source-id\":\"A\",\"route\":\"ready\",\"note\":null,\"count\":null}]}"
                     "{\"items\":[{\"source-id\":\"A1\",\"route\":\"ready\",\"count\":null}]}"
                     "{\"items\":[{\"source-id\":\"A1\",\"route\":\"ready\",\"note\":null,\"count\":null,\"extra\":0}]}"
                     "{\"items\":{\"source-id\":\"A1\",\"route\":\"ready\",\"note\":null,\"count\":null}}"]]
      (is (= :schema-validation-failed (:failure-kind (rejection #(structured/parse-output compiled content)))))) ))

(deftest compiler-rejects-ambiguous-enums-and-invalid-field-identities
  (doseq [outputs [[{:name :value :spec [:enum :ready "ready"]}]
                   [{:name :value :spec [:enum {:not "a JSON scalar"}]}]
                   [{:name nil :spec :string}]
                   [{:name :x/value :spec :string} {:name :value :spec :string}]]]
    (is (= :unsupported-output-schema (:failure-kind (rejection #(structured/compile-contract {:outputs outputs})))))))

(deftest numeric-json-values-match-the-original-numeric-contract
  (doseq [kind [:double :float]]
    (let [compiled (structured/compile-contract {:outputs [{:name :score :spec [kind {:min 0 :max 2}]}]})]
      (is (= 1.0 (:score (structured/parse-output compiled "{\"score\":1}"))))
      (doseq [content ["{\"score\":\"1\"}" "{\"score\":3}"]]
        (is (= :schema-validation-failed (:failure-kind (rejection #(structured/parse-output compiled content))))))))
  (doseq [value [Double/NaN Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY]]
    (is (= :unsupported-output-schema
           (:failure-kind (rejection #(structured/compile-contract {:outputs [{:name :score :spec [:enum value]}]})))))))

(deftest integer-contract-accepts-only-integral-json-numbers
  (let [compiled (structured/compile-contract {:outputs [{:name :count :spec [:int {:min 0 :max 2}]}]})]
    (is (= {:count 1} (structured/parse-output compiled "{\"count\":1.0}")))
    (doseq [content ["{\"count\":1.5}" "{\"count\":\"1\"}" "{\"count\":3.0}"]]
      (is (= :schema-validation-failed (:failure-kind (rejection #(structured/parse-output compiled content))))))))

(deftest integer-decoding-preserves-the-original-host-range
  (let [compiled (structured/compile-contract {:outputs [{:name :count :spec :int}]})]
    (is (= :schema-validation-failed
           (:failure-kind (rejection #(structured/parse-output compiled "{\"count\":1.00000000000000000001}")))))
    (is (= :schema-validation-failed
           (:failure-kind (rejection #(structured/parse-output compiled "{\"count\":9223372036854775808}")))))))

(deftest enum-numeric-equivalence-cannot-collapse-distinct-host-options
  (is (= :ambiguous-enum-values
         (:reason (rejection #(structured/compile-contract {:outputs [{:name :choice :spec [:enum 1 1.0]}]})))))
  (let [compiled (structured/compile-contract {:outputs [{:name :choice :spec [:enum true false]}]})]
    (is (= {:choice true} (structured/parse-output compiled "{\"choice\":true}")))
    (is (= {:choice false} (structured/parse-output compiled "{\"choice\":false}")))))

(deftest enum-numbers-match-exact-canonical-json-values
  (doseq [[options rejected] [[[:enum 1.0] "1.00000000000000000001"]
                             [[:enum 9007199254740992.0] "9007199254740993"]]]
    (let [compiled (structured/compile-contract {:outputs [{:name :choice :spec options}]})]
      (is (= :schema-validation-failed
             (:failure-kind (rejection #(structured/parse-output compiled (str "{\"choice\":" rejected "}"))))))))
  (let [compiled (structured/compile-contract {:outputs [{:name :choice :spec [:enum 9007199254740992.0 9007199254740993]}]})]
    (is (= {:choice 9007199254740992.0} (structured/parse-output compiled "{\"choice\":9.007199254740992e15}")))
    (is (= {:choice 9007199254740993} (structured/parse-output compiled "{\"choice\":9007199254740993}"))))
  (doseq [[option json-value] [[1 "1.0"] [1.0 "1"] [0.1 "0.1000"]]]
    (let [compiled (structured/compile-contract {:outputs [{:name :choice :spec [:enum option]}]})]
      (is (= {:choice option} (structured/parse-output compiled (str "{\"choice\":" json-value "}"))))))
  (is (= :ambiguous-enum-values
         (:reason (rejection #(structured/compile-contract {:outputs [{:name :choice :spec [:enum 1.0 1.00M]}]}))))))


(deftest integer-wire-contract-emits-only-authored-bounds
  (doseq [[form expected] [[:int {:type "integer"}]
                         [[:int {:min 0}] {:type "integer" :minimum 0}]
                         [[:int {:min 2 :max 100}] {:type "integer" :minimum 2 :maximum 100}]
                         [[:int {:max Long/MAX_VALUE}] {:type "integer" :maximum Long/MAX_VALUE}]]]
    (let [compiled (structured/compile-contract {:outputs [{:name :count :spec form}]})]
      (is (= expected (get-in compiled [:schema :properties "count"]))))))


(deftest authored-property-order-survives-wide-and-nested-contracts
  (let [names ["rationale" "invocation" "obligation" "quantity" "unit-price"
               "delivery-charge" "total" "text" "evidence"]
        entries (mapv (fn [n] [(keyword n) :string]) names)
        compiled (structured/compile-contract
                  {:outputs (conj (mapv (fn [n] {:name (keyword n) :spec :string}) names)
                                  {:name :nested :spec (into [:map {:closed true}] entries)})})
        schema (:schema compiled)
        serialized (json/write-str schema)
        guide (structured/output-guide compiled)
        ;; Inspect the actual serialized property sequence. Required-array order
        ;; alone does not establish model-visible property order.
        ordered? (fn [text]
                   (let [positions (mapv #(.indexOf text (str (json/write-str %) ":")) names)]
                     (and (every? #(<= 0 %) positions) (apply < positions))))]
    (is (= (conj names "nested") (vec (keys (:properties schema)))))
    (is (= names (vec (keys (get-in schema [:properties "nested" :properties])))))
    (is (ordered? serialized))
    (is (ordered? guide))))
