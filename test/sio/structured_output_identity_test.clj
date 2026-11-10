(ns sio.structured-output-identity-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [sio.structured-output :as so]))
(defn contract [schema]
  (so/compile-contract {:outputs [{:name :answer :spec schema}]}))
(defn rejection [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest colliding-typed-keys-get-paired-deterministic-aliases
  (let [schema [:map {:closed true}
                [:same [:string {:description "Keyword-owned text."}]]
                ["same" [:int {:description "String-owned count."}]]
                [:other :boolean]]
        compiled (contract schema)
        entries (get-in compiled [:fields 0 :node :entries])
        names (into {} (map (juxt :key :wire-name) entries))]
    (is (= "other" (names :other)))
    (is (= 3 (count (set (vals names)))))
    (is (not= "same" (names :same)))
    (is (not= "same" (names "same")))
    (is (= names (into {} (map (juxt :key :wire-name) (get-in (contract schema) [:fields 0 :node :entries])))))
    (is (= {:answer {:same "literal" "same" 0 :other false}}
           (so/parse-output compiled
             (json/write-str {"answer" {(names :same) "literal" (names "same") 0 "other" false}}))))
    (is (re-find #"Keyword-owned text" (so/output-guide compiled)))
    (is (re-find #"String-owned count" (so/output-guide compiled)))
    (is (re-find #"keyword" (so/output-guide compiled)))
    (is (re-find #"Original map key" (so/output-guide compiled)))))

(deftest aliases-reserve-authored-names-and-preserve-namespaces
  (let [compiled (contract [:map {:closed true}
                            [:same :string] ["same" :int]
                            ["__sio_key_0" :boolean]
                            [:left/same :string] [:right/same :string]
                            [:app/id :string] ["app/id" :int]])
        names (into {} (map (juxt :key :wire-name) (get-in compiled [:fields 0 :node :entries])))]
    (is (= "__sio_key_0" (names "__sio_key_0")))
    (is (= "left/same" (names :left/same)))
    (is (= "right/same" (names :right/same)))
    (is (= 7 (count (set (vals names)))))
    (is (= {:answer {:same "" "same" 0 "__sio_key_0" false
                     :left/same "left" :right/same "right" :app/id "keyword" "app/id" 7}}
           (so/parse-output compiled
             (json/write-str {"answer" {(names :same) "" (names "same") 0 "__sio_key_0" false
                                       "left/same" "left" "right/same" "right"
                                       (names :app/id) "keyword" (names "app/id") 7}}))))))

(deftest alias-plan-composes-with-presence-and-variant-selection
  (let [variant (fn [kind]
                  [:map {:closed true} [:kind [:= kind]]
                   [:same {:optional true} [:maybe :boolean]]
                   ["same" {:optional true} :string]])
        compiled (contract [:multi {:dispatch :kind} [:ready (variant :ready)] [:pending (variant :pending)]])
        branches (get-in compiled [:fields 0 :node :branches])]
    (doseq [[branch kind] (map vector branches [:ready :pending])]
      (let [names (into {} (map (juxt :key :wire-name) (:entries branch)))]
        (is (= :aliases-v1 (:key-encoding branch)))
        (is (= {:answer {:kind kind :same nil}}
               (so/parse-output compiled
                 (json/write-str {"answer" {"kind" (name kind)
                                           (names :same) {"presence" "present" "value" nil}
                                           (names "same") nil}}))))
        (is (= {:answer {:kind kind "same" ""}}
               (so/parse-output compiled
                 (json/write-str {"answer" {"kind" (name kind)
                                           (names :same) {"presence" "absent"}
                                           (names "same") ""}}))))))))

(deftest unknown-original-spelling-is-not-an-alias-fallback
  (let [compiled (contract [:map {:closed true} [:same :string] ["same" :int]])]
    (is (= :undeclared-property
           (:reason (rejection #(so/parse-output compiled "{\"answer\":{\"same\":1}}")))))))

(deftest duplicate-json-aliases-reject-before-map-overwrite
  (let [compiled (contract [:map {:closed true} [:same :string] ["same" :int]])
        names (into {} (map (juxt :key :wire-name) (get-in compiled [:fields 0 :node :entries])))
        alias (json/write-str (names :same))
        content (str "{\"answer\":{" alias ":\"first\"," alias ":\"second\","
                     (json/write-str (names "same")) ":0}}")]
    (is (= {:failure-kind :schema-validation-failed :phase :parse
            :reason :duplicate-json-property :path ["answer"]}
           (rejection #(so/parse-output compiled content))))))

(deftest noncolliding-property-plans-remain-unchanged
  (let [compiled (contract [:map {:closed true} [:left/name :string] ["literal" :boolean]])
        node (get-in compiled [:fields 0 :node])]
    (is (not (contains? node :key-encoding)))
    (is (= ["left/name" "literal"] (mapv :wire-name (:entries node))))
    (is (every? #(not (contains? % :wire-identity-description)) (:entries node)))
    (is (= {:answer {:left/name "" "literal" false}}
           (so/parse-output compiled "{\"answer\":{\"left/name\":\"\",\"literal\":false}}")))))

(deftest duplicate-property-detection-is-local-and-unicode-aware
  (let [compiled (contract [:map {:closed true}
                            [:left [:map {:closed true} [:a :int]]]
                            [:right [:map {:closed true} [:a :int]]]])]
    (is (= {:answer {:left {:a 0} :right {:a 1}}}
           (so/parse-output compiled "{\"answer\":{\"left\":{\"a\":0},\"right\":{\"a\":1}}}")))
    (is (= ["answer" "left"]
           (:path (rejection #(so/parse-output compiled
                                "{\"answer\":{\"left\":{\"a\":0,\"\\u0061\":1},\"right\":{\"a\":1}}}")))))
    (is (= :duplicate-json-property
           (:reason (rejection #(so/parse-output compiled
                                  "{\"answer\":{},\"answer\":{\"left\":{\"a\":0},\"right\":{\"a\":1}}}")))))))
