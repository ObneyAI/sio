(ns sio.structured-output-variants-test
  (:require [clojure.test :refer [deftest is testing]]
            [sio.structured-output :as so]))

(defn contract [schema]
  (so/compile-contract {:outputs [{:name :answer :spec schema}]}))

(deftest disjoint-scalar-union-roundtrip
  (let [compiled (contract [:or {:description "Exact supplied text or count."} :string :int])]
    (is (= {:answer ""} (so/parse-output compiled "{\"answer\":\"\"}")))
    (is (= {:answer 0} (so/parse-output compiled "{\"answer\":0}")))
    (is (= {:anyOf [{:type "string"} {:type "integer"}]
            :description "Exact supplied text or count."}
           (get-in compiled [:schema :properties "answer"])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (so/parse-output compiled "{\"answer\":false}")))))

(def ready-record
  [:map {:closed true :description "Completed record."}
   [:kind [:enum {:descriptions {:ready "Completed, even if no items exist."}} :ready]]
   [:items [:vector [:map {:closed true} [:enabled :boolean] [:count :int]]]]])
(def pending-record
  [:map {:closed true :description "Pending record."}
   [:kind [:enum :pending]]
   [:note [:maybe :string]]])

(deftest disjoint-record-union-roundtrip
  (let [compiled (contract [:vector [:or ready-record pending-record]])]
    (is (= {:answer [{:kind :ready :items [{:enabled false :count 0}]}
                     {:kind :pending :note nil}]}
           (so/parse-output compiled
             "{\"answer\":[{\"kind\":\"ready\",\"items\":[{\"enabled\":false,\"count\":0}]},{\"kind\":\"pending\",\"note\":null}]}")))
    (is (re-find #"Completed, even if no items exist" (so/output-guide compiled)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (so/parse-output compiled "{\"answer\":[{\"kind\":\"unknown\",\"note\":null}]}")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (so/parse-output compiled "{\"answer\":[{\"kind\":\"ready\",\"items\":[],\"extra\":true}]}")))))

(deftest finite-literal-multi-roundtrip
  (let [schema [:multi {:dispatch :kind :description "The supplied tagged variant."}
                [:ready [:map {:closed true} [:kind [:= :ready]] [:value :boolean]]]
                [:pending [:map {:closed true} [:kind [:= :pending]] [:value [:maybe :string]]]]]
        compiled (contract schema)]
    (is (= {:answer {:kind :ready :value false}}
           (so/parse-output compiled "{\"answer\":{\"kind\":\"ready\",\"value\":false}}")))
    (is (= {:answer {:kind :pending :value ""}}
           (so/parse-output compiled "{\"answer\":{\"kind\":\"pending\",\"value\":\"\"}}")))
    (is (= 2 (count (get-in compiled [:schema :properties "answer" :anyOf]))))
    (is (= {:enum ["ready"]} (get-in compiled [:schema :properties "answer" :anyOf 0 :properties "kind"])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (so/parse-output compiled "{\"answer\":{\"kind\":\"unknown\",\"value\":false}}")))))

(defn rejection [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest ambiguous-json-unions-reject-before-decoding
  (doseq [schema [[:or [:enum :ready] :string]
                  [:or [:enum :ready] [:enum "ready"]]
                  [:or :int :double]
                  [:or [:enum 1] [:enum 1.0]]
                  [:or [:map {:closed true} [:kind {:optional true} [:enum :a]]]
                       [:map {:closed true} [:kind {:optional true} [:enum :b]]]]
                  [:multi {:dispatch :kind}
                   [:ready [:map {:closed true} [:kind [:= :ready]]]]
                   ["ready" [:map {:closed true} [:kind [:= "ready"]]]]]]]
    (is (= {:failure-kind :unsupported-output-schema :phase :compile
            :reason :ambiguous-union :path ["answer"]}
           (rejection #(contract schema))))))

(deftest literal-dispatch-contracts-reject-unsupported-forms
  (doseq [[schema reason]
          [[[:multi {:dispatch (fn [x] (:kind x))}
              [:ready [:map {:closed true} [:kind [:= :ready]]]]]
            :unsupported-dispatch]
           [[:multi {:dispatch :kind}
              [:ready [:map {:closed true} [:kind :string]]]]
            :nonliteral-dispatch]
           [[:multi {:dispatch :kind}
              [:ready [:map {:closed true} [:kind {:optional true} [:= :ready]]]]]
            :nonliteral-dispatch]
           [[:multi {:dispatch :kind}
              [:ready [:map {:closed true} [:kind [:= :other]]]]]
            :nonliteral-dispatch]
           [[:multi {:dispatch :kind}
              [:malli.core/default [:map {:closed true} [:kind :string]]]]
            :unsupported-dispatch]]]
    (is (= reason (:reason (rejection #(contract schema)))))))

(deftest nested-unions-preserve-null-empty-and-original-constraints
  (let [compiled (contract [:or [:maybe [:string {:min 1}]] [:vector :boolean]])]
    (is (= {:answer nil} (so/parse-output compiled "{\"answer\":null}")))
    (is (= {:answer []} (so/parse-output compiled "{\"answer\":[]}")))
    (is (= {:answer [false]} (so/parse-output compiled "{\"answer\":[false]}")))
    (is (= :no-matching-branch (:reason (rejection #(so/parse-output compiled "{\"answer\":\"\"}")))))))

(deftest optional-union-does-not-invoke-unsupported-dispatch
  (let [calls (atom 0)
        schema [:or :string
                [:multi {:dispatch (fn [_] (swap! calls inc) :ready)}
                 [:ready [:map {:closed true} [:kind [:= :ready]]]]]]
        error (rejection #(so/compile-contract {:outputs [{:name :answer :optional true :spec schema}]}))]
    (is (= :unsupported-dispatch (:reason error)))
    (is (zero? @calls))))

(deftest optional-domain-fields-still-have-required-wire-slots
  (let [compiled (contract
                   [:or [:map {:closed true} [:a {:optional true} :string]]
                        [:map {:closed true} [:b {:optional true} :string]]])
        branches (get-in compiled [:schema :properties "answer" :anyOf])]
    (is (= [["a"] ["b"]] (mapv :required branches)))
    (is (= [false false] (mapv :additionalProperties branches)))
    (is (= {:answer {}} (so/parse-output compiled "{\"answer\":{\"a\":null}}")))
    (is (= {:answer {}} (so/parse-output compiled "{\"answer\":{\"b\":null}}")))
    ;; The original domain maps overlap at {}, but no wire object can match
    ;; both closed, fully required encodings. Neither missing nor combined
    ;; sentinel slots may sneak through branch selection.
    (doseq [content ["{\"answer\":{}}" "{\"answer\":{\"a\":null,\"b\":null}}"]]
      (is (= :no-matching-branch (:reason (rejection #(so/parse-output compiled content))))))))
