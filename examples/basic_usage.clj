;; Runnable, provider-free tour of sio.
;;
;;   clojure -M examples/basic_usage.clj
;;
;; sio never calls an LLM. To keep this example self-contained and deterministic,
;; the "model responses" below are hard-coded strings — exactly the shape a real
;; provider would return for the given spec.

(require '[sio.core :as sio]
         '[clojure.pprint :refer [pprint]])

(defn section [title] (println (str "\n=== " title " ===")))

;; -----------------------------------------------------------------------------
;; 1. Define a spec — the typed input/output contract.
;; -----------------------------------------------------------------------------

(def qa
  {:inputs  [{:name :question :spec :string  :description "The question to answer"}]
   :outputs [{:name :answer    :spec :string  :description "The answer"}
             {:name :confident :spec :boolean :description "Are you confident?"}]
   :instructions "Answer concisely and accurately."})

(section "1. spec->prompt — render the contract into a prompt template")
(println (sio/spec->prompt qa))

;; -----------------------------------------------------------------------------
;; 2. Send the prompt with YOUR client; parse the reply back into typed data.
;; -----------------------------------------------------------------------------

(section "2. parse-output — model text -> Clojure data")
(def qa-reply "[[ ## answer ## ]]\nParis\n[[ ## confident ## ]]\nTrue")
(pprint (sio/parse-output qa-reply qa))
;; => {:answer "Paris", :confident true}

(section "2b. validate-outputs — enforce the Malli schemas")
(pprint (sio/validate-outputs (:outputs qa) (sio/parse-output qa-reply qa)))

;; -----------------------------------------------------------------------------
;; 3. Complex, typed outputs (enum / double / vector / map) parse as JSON.
;; -----------------------------------------------------------------------------

(def analysis
  {:inputs  [{:name :text :spec :string :description "Text to analyze"}]
   :outputs [{:name :sentiment :spec [:enum "positive" "neutral" "negative"]}
             {:name :score     :spec [:double {:min 0 :max 1}]}
             {:name :keywords  :spec [:vector :string]}
             {:name :meta      :spec [:map [:lang :string] [:reviewed {:optional true} :boolean]]}]})

(section "3. Complex fields — prompt gets JSON hints, parse-output reads JSON")
(println (sio/spec->prompt analysis))

(def analysis-reply
  (str "[[ ## sentiment ## ]]\npositive\n"
       "[[ ## score ## ]]\n0.87\n"
       "[[ ## keywords ## ]]\n[\"fast\", \"cheap\"]\n"
       "[[ ## meta ## ]]\n```json\n{\"lang\": \"en\", \"reviewed\": true}\n```"))
(println "\nparsed:")
(pprint (sio/parse-output analysis-reply analysis))

;; -----------------------------------------------------------------------------
;; 4. Function calling — tool definition out, arguments back in.
;; -----------------------------------------------------------------------------

(section "4. outputs->tool-definition")
(pprint (sio/outputs->tool-definition qa))

(section "4b. parse-tool-call-response")
(def provider-response
  {:choices [{:message {:tool-calls [{:function {:name "submit_response"
                                                 :arguments "{\"answer\": \"Paris\", \"confident\": true}"}}]}}]})
(pprint (sio/parse-tool-call-response provider-response (:outputs qa)))

;; -----------------------------------------------------------------------------
;; 5. Multimodal — image inputs become content parts, not prompt text.
;; -----------------------------------------------------------------------------

(def caption
  {:inputs  [{:name :instruction :spec :string}
             {:name :photo :type :image :description "Photo to caption"}]
   :outputs [{:name :caption :spec :string}]})

(section "5. build-message-content — text + image content parts")
(pprint (sio/build-message-content caption
                                   "Write a caption."
                                   {:instruction "Write a caption."
                                    :photo "data:image/png;base64,iVBORw0KG..."}))

;; -----------------------------------------------------------------------------
;; 6. Validation catches bad data.
;; -----------------------------------------------------------------------------

(section "6. validate-inputs — throws on a type mismatch")
(try
  (sio/validate-inputs (:inputs qa) {:question 123})
  (catch Exception e
    (println "Rejected as expected:" (ex-message e))))

;; -----------------------------------------------------------------------------
;; 7. Streaming — progressive parse over accumulated text.
;; -----------------------------------------------------------------------------

(def streamer {:outputs [{:name :answer :spec :string}]})

(section "7. parse-streaming-output — progressive parse")
(doseq [partial ["[[ ## answer ## ]]\nThe"
                 "[[ ## answer ## ]]\nThe capital"
                 "[[ ## answer ## ]]\nThe capital is Paris."]]
  (println (pr-str (sio/parse-streaming-output partial streamer))))

(println "\nDone.")
