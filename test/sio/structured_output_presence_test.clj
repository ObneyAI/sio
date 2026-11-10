(ns sio.structured-output-presence-test
  (:require [clojure.test :refer [deftest is]]
            [sio.structured-output :as so]))

(defn rejection [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest optional-nullable-output-preserves-three-states
  (let [compiled (so/compile-contract
                   {:outputs [{:name :answer :optional true :spec [:maybe :string]
                               :description "Literal supplied text, or explicit null."}]})]
    (is (= {} (so/parse-output compiled "{\"answer\":{\"presence\":\"absent\"}}")))
    (is (= {:answer nil} (so/parse-output compiled "{\"answer\":{\"presence\":\"present\",\"value\":null}}")))
    (is (= {:answer ""} (so/parse-output compiled "{\"answer\":{\"presence\":\"present\",\"value\":\"\"}}")))
    (is (= {:answer "{\"a\":true}"}
           (so/parse-output compiled "{\"answer\":{\"presence\":\"present\",\"value\":\"{\\\"a\\\":true}\"}}")))
    (is (re-find #"absent" (so/output-guide compiled)))
    (is (re-find #"Literal supplied text" (so/output-guide compiled)))))

(deftest nested-presence-preserves-values-in-collections
  (let [compiled (so/compile-contract
                   {:outputs [{:name :rows :spec
                               [:vector [:map {:closed true}
                                         [:note {:optional true :description "Optional source note."} [:maybe :string]]
                                         [:ready {:optional true} [:maybe :boolean]]
                                         [:count {:optional true} [:maybe :int]]
                                         [:tags {:optional true} [:maybe [:vector :string]]]]]}]})]
    (is (= {:rows [{:note nil :ready false :count 0 :tags []} {}]}
           (so/parse-output compiled
             "{\"rows\":[{\"note\":{\"presence\":\"present\",\"value\":null},\"ready\":{\"presence\":\"present\",\"value\":false},\"count\":{\"presence\":\"present\",\"value\":0},\"tags\":{\"presence\":\"present\",\"value\":[]}},{\"note\":{\"presence\":\"absent\"},\"ready\":{\"presence\":\"absent\"},\"count\":{\"presence\":\"absent\"},\"tags\":{\"presence\":\"absent\"}}]}")))
    (is (re-find #"Optional source note" (so/output-guide compiled)))))

(deftest presence-rejects-missing-extra-and-invalid-values
  (let [compiled (so/compile-contract {:outputs [{:name :answer :optional true :spec [:maybe [:int {:min 0}]]}]})]
    (doseq [body ["{}" "{\"answer\":null}" "{\"answer\":{}}"
                  "{\"answer\":{\"presence\":\"unknown\"}}"
                  "{\"answer\":{\"presence\":\"present\"}}"
                  "{\"answer\":{\"presence\":\"absent\",\"value\":null}}"
                  "{\"answer\":{\"presence\":\"present\",\"value\":0,\"extra\":false}}"
                  "{\"answer\":{\"presence\":\"present\",\"value\":-1}}"
                  "{\"answer\":{\"presence\":\"present\",\"value\":\"0\"}}"]]
      (is (= :schema-validation-failed (:failure-kind (rejection #(so/parse-output compiled body)))) body))))

(deftest existing-presence-protocols-stay-wire-compatible
  (let [compiled (so/compile-contract
                   {:outputs [{:name :optional :optional true :spec :string}
                              {:name :nullable :spec [:maybe :string]}]})]
    (is (= {"optional" {:anyOf [{:type "string"} {:type "null"}]}
            "nullable" {:anyOf [{:type "string"} {:type "null"}]}}
           (get-in compiled [:schema :properties])))
    (is (every? #(not (contains? % :presence-encoding)) (:fields compiled)))
    (is (= {:nullable nil} (so/parse-output compiled "{\"optional\":null,\"nullable\":null}")))
    (is (= {:optional "" :nullable ""} (so/parse-output compiled "{\"optional\":\"\",\"nullable\":\"\"}")))))

(deftest nullable-union-and-null-literal-use-same-presence-codec
  (doseq [schema [[:or :nil :string] [:enum {} nil :ready] :nil]]
    (let [compiled (so/compile-contract {:outputs [{:name :answer :optional true :spec schema}]})]
      (is (= :tagged-v1 (:presence-encoding (first (:fields compiled)))))
      (is (= {} (so/parse-output compiled "{\"answer\":{\"presence\":\"absent\"}}")))
      (is (= {:answer nil} (so/parse-output compiled "{\"answer\":{\"presence\":\"present\",\"value\":null}}"))))))
