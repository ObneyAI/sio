(ns sio.output-integrity-test
  (:require [clojure.test :refer [deftest is testing]]
            [sio.core :as sio]))

(def nullable-fields [{:name :answer :spec [:maybe :string]}])

(defn tool-response [arguments]
  {:choices [{:message {:tool-calls [{:function {:arguments arguments}}]}}]})

(deftest tool-preserves-required-null-presence-test
  (let [absent (sio/parse-tool-call-response (tool-response "{}") nullable-fields)
        explicit (sio/parse-tool-call-response (tool-response "{\"answer\":null}") nullable-fields)]
    (is (= {} absent))
    (is (thrown? clojure.lang.ExceptionInfo (sio/validate-outputs nullable-fields absent)))
    (is (= {:answer nil} explicit))
    (is (= explicit (sio/validate-outputs nullable-fields explicit)))))

(deftest final-marker-presence-is-not-partial-streaming-test
  (let [spec {:outputs [{:name :answer :spec [:maybe :string]}
                        {:name :note :spec :string}]}
        partial "[[ ## note ## ]]\nworking"
        finished (str partial "\n[[ ## completed ## ]]")]
    (is (= {:note "working"} (sio/parse-output finished spec)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (sio/validate-outputs (:outputs spec) (sio/parse-output finished spec))))
    (is (= {:answer nil :note "working"} (sio/parse-streaming-output partial spec)))
    (is (= {:note "working"} (sio/parse-output partial spec)))
    (is (= {} (sio/parse-output nil spec)))
    (is (= {:answer nil :note nil} (sio/parse-streaming-output nil spec)))
    (is (= {:answer "" :note "done"}
           (sio/parse-output "[[ ## answer ## ]]\n\n[[ ## note ## ]]\ndone" spec)))))

(deftest nullable-json-scalars-decode-once-test
  (doseq [[schema encoded expected]
          [[[:maybe :string] "\"REF-24\"" "REF-24"]
           [[:maybe :string] "\"He said \\\"yes\\\"\"" "He said \"yes\""]
           [[:maybe :string] "\"null\"" "null"]
           [[:maybe :string] "null" nil]
           [[:maybe :boolean] "false" false]
           [[:maybe :int] "42" 42]
           [[:maybe :double] "0.5" 0.5]]]
    (let [fields [{:name :answer :spec schema}]
          parsed (sio/parse-output (str "[[ ## answer ## ]]\n" encoded) {:outputs fields})]
      (is (= {:answer expected} parsed))
      (is (= parsed (sio/validate-outputs fields parsed)))))
  (testing "plain and quoted ordinary strings are not JSON protocol values"
    (doseq [value ["REF-24" "\"REF-24\"" "null" "He said \"yes\""]]
      (is (= {:answer value}
             (sio/parse-output (str "[[ ## answer ## ]]\n" value)
                               {:outputs [{:name :answer :spec :string}]})))))
  (testing "malformed nullable JSON remains visible for validation"
    (is (= {:answer "falsely"}
           (sio/parse-output "[[ ## answer ## ]]\nfalsely"
                             {:outputs [{:name :answer :spec [:maybe :boolean]}]})))))

(deftest nullable-descriptions-distinguish-presence-test
  (is (= "int or null" (sio/spec->type-str [:maybe {:description "Nullable count"} :int])))
  (let [spec {:inputs [] :outputs [{:name :count :spec [:maybe {:description "Nullable count"} :int]
                                    :description "Known count or null"}]}
        prompt (sio/spec->prompt spec)]
    (is (clojure.string/includes? prompt "`count` (int or null): Known count or null"))
    (is (not (clojure.string/includes? prompt "optional")))))

(deftest completed-marker-is-not-an-empty-answer-test
  (let [spec {:outputs [{:name :answer :spec :string}]}]
    (is (= {} (sio/parse-output "[[ ## completed ## ]]" spec)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (sio/validate-outputs (:outputs spec)
                                       (sio/parse-output "[[ ## completed ## ]]" spec))))
    (is (= {:answer ""} (sio/parse-output "[[ ## answer ## ]]\n[[ ## completed ## ]]" spec)))
    (is (= {:answer "ordinary prose"} (sio/parse-output "ordinary prose" spec)))
    (is (= {:answer nil} (sio/parse-streaming-output "[[ ## completed ## ]]" spec)))))
