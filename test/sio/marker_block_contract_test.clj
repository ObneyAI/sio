(ns sio.marker-block-contract-test
  "What the READ side does with what the model actually sent.

   Every fixture below is a REAL shape taken from captured model responses,
   not an invented one. Provenance is cited per test.

   The marker block is the protocol's only contract, and it has four
   independent questions:

     1. what counts as the block's DELIMITERS   (`[ [` / `] ]` drift)
     2. what counts as the block's START        (indented? mid-line?)
     3. where the block's value BEGINS          (next line? same line?)
     4. WHICH occurrence is the answer          (the model's scratchpad
                                                 contains the others)

   and one question about the value inside it:

     5. what a value that cannot be interpreted as the declared type becomes
        (never a fabricated one)"
  (:require [clojure.test :refer [deftest is testing]]
            [sio.core :as sio]))

(def two-strings
  {:outputs [{:name :a :spec :string} {:name :b :spec :string}]})

;; ===========================================================================
;; 1. DELIMITERS — the two brackets drift apart
;; ===========================================================================

(deftest closing-bracket-drift-is-tolerated-test
  ;; PROVENANCE: 23 of 23 captured :unparseable-output events from orc's
  ;; `reflect` node wrote `[[ ## avoid-when ## ] ]`.
  ;; evidence/cc33/raw/reflect-unparseable-rows.json
  (testing "`] ]` — the exact production drift — still extracts"
    (is (= {:a "first" :b "second"}
           (sio/parse-output "[[ ## a ## ]]\nfirst\n\n[[ ## b ## ] ]\nsecond\n"
                             two-strings))))
  (testing "a verbatim production block still extracts"
    (let [real (str "[[ ## representative-uses ## ]]\n"
                    "{\n  \"representative-uses\": [\n    \"Ranking retrieved behavioral subtrees\"\n  ]\n}\n\n"
                    "[[ ## avoid-when ## ] ]\n"
                    "{\n  \"avoid-when\": [\n    \"Strict low-latency batch reranking requirements\"\n  ]\n}\n\n"
                    "[[ ## summary ## ]]\nThis node reranks candidates.\n[[ ## completed ## ]]")
          parsed (sio/parse-output real
                                   {:outputs [{:name :representative-uses :spec :string}
                                              {:name :avoid-when :spec :string}
                                              {:name :summary :spec :string}]})]
      (is (some? (:avoid-when parsed))
          "the field orc logged as :nil-keys [:avoid-when]")
      (is (= "This node reranks candidates." (:summary parsed))))))

(deftest opening-bracket-drift-is-tolerated-test
  ;; PROVENANCE: SIO-1 audit row D1' — the same two characters, worse
  ;; consequence: today the PREVIOUS field silently swallows the remainder.
  (testing "`[ [` still extracts, and does not contaminate the previous field"
    (is (= {:a "first" :b "second"}
           (sio/parse-output "[[ ## a ## ]]\nfirst\n\n[ [ ## b ## ]]\nsecond\n"
                             two-strings)))))

;; ===========================================================================
;; 2. START — an indented marker is still a marker
;; ===========================================================================

(deftest indented-marker-starts-and-terminates-a-block-test
  ;; PROVENANCE: 9 of 15 qwen/qwen3.5-flash-02-23 captures indent their
  ;; markers. evidence/sio1/raw/live-qwen_*.txt
  ;; DSPy strips each line before matching, so this is upstream parity.
  (testing "an indented marker terminates the previous field"
    (is (= {:a "first" :b "second"}
           (sio/parse-output "[[ ## a ## ]]\nfirst\n   [[ ## b ## ]]\nsecond\n"
                             two-strings)))))

(deftest a-marker-quoted-mid-line-does-not-start-a-block-test
  ;; PROVENANCE: evidence/sio1/raw/live-qwen_qwen3_5-flash-02-23-13.txt:
  ;;   "    *   Must follow the exact delimiters: `[[ ## capabilities ## ]]`, etc."
  ;; Widening the block's START must not widen it to prose that MENTIONS a
  ;; marker. A block begins a line (leading whitespace allowed) or it is not
  ;; a block.
  (testing "a marker inside a sentence is part of the value, not a new block"
    (is (= {:a "see `[[ ## b ## ]]` for the format" :b "real"}
           (sio/parse-output
             "[[ ## a ## ]]\nsee `[[ ## b ## ]]` for the format\n[[ ## b ## ]]\nreal\n"
             two-strings)))))

;; ===========================================================================
;; 3. VALUE POSITION — the value may sit on the marker line
;; ===========================================================================

(deftest value-on-the-marker-line-is-extracted-test
  ;; PROVENANCE: 11 of 15 qwen captures put a value on the marker line.
  ;; DSPy supports this explicitly (`remaining_content = line[match.end():]`),
  ;; so this too is upstream parity — sio regressed it.
  (testing "`[[ ## a ## ]] value` extracts, and does not nil the whole response"
    (is (= {:a "first" :b "second"}
           (sio/parse-output "[[ ## a ## ]] first\n[[ ## b ## ]] second\n"
                             two-strings))))
  (testing "the newline form is unchanged"
    (is (= {:a "first" :b "second"}
           (sio/parse-output "[[ ## a ## ]]\nfirst\n[[ ## b ## ]]\nsecond\n"
                             two-strings)))))

;; ===========================================================================
;; 4. WHICH OCCURRENCE — the answer, not the scratchpad
;; ===========================================================================

(deftest the-answer-block-wins-over-a-scratchpad-echo-test
  ;; PROVENANCE: 15 of 15 qwen captures contain a duplicated field marker;
  ;; 10 of 30 captures returned the model's SCRATCHPAD content rather than its
  ;; answer, silently. evidence/sio1/raw/, replayed in
  ;; evidence/sio2/logs/replay-BEFORE.log.
  ;; DSPy assembles sections in order into a dict, so the LAST section for a
  ;; field wins — upstream parity again. Reasoning precedes the answer; a
  ;; model drafts, revises, then writes the real block last.
  (testing "a later block for the same field replaces an earlier draft"
    (is (= {:a "REAL" :b "b"}
           (sio/parse-output "[[ ## a ## ]]\nDRAFT\n\n[[ ## a ## ]]\nREAL\n\n[[ ## b ## ]]\nb\n"
                             two-strings))))
  (testing "an INDENTED draft block loses to the answer block (the real shape)"
    ;; Condensed from live-qwen_qwen3_5-flash-02-23-05.txt: the model drafts
    ;; the whole template indented inside its reasoning, then emits the real
    ;; one at column 0.
    (is (= {:a "the revised answer" :b "the revised note"}
           (sio/parse-output
             (str "Thinking Process:\n\n"
                  "    [[ ## a ## ]]\n    the draft answer\n\n"
                  "    [[ ## b ## ]]\n    the draft note\n\n"
                  "    Okay, ready to generate.\n</think>\n\n"
                  "[[ ## a ## ]]\nthe revised answer\n\n"
                  "[[ ## b ## ]]\nthe revised note\n"
                  "[[ ## completed ## ]]")
             two-strings)))))

;; ===========================================================================
;; 5. THE VALUE INSIDE — a boolean is never fabricated
;; ===========================================================================

(def one-bool {:outputs [{:name :ok :spec :boolean} {:name :note :spec :string}]})

(defn- bool-of [v]
  (:ok (sio/parse-output (str "[[ ## ok ## ]]\n" v "\n\n[[ ## note ## ]]\nn\n") one-bool)))

(deftest boolean-coercion-never-fabricates-a-value-test
  ;; PROVENANCE: SIO-1 measured a model writing `true` and sio writing
  ;; `false` to the blackboard, with no error anywhere — 2 of 15 qwen calls
  ;; (evidence/sio1/logs/bool-truth-check.log rows 05, 06).
  ;; `false` is a VALID value, so a fabricated `false` validates and is
  ;; reported :success. That is the defect: not strictness, but invention.
  (testing "the spellings the rendered prompt teaches (`True`/`False`) are honoured"
    (is (true? (bool-of "True")))
    (is (false? (bool-of "False"))))
  (testing "JSON's spellings are honoured — the same prompt says `respond with valid JSON`"
    (is (true? (bool-of "true")))
    (is (false? (bool-of "false"))))
  (testing "the upper-case pair is symmetric"
    (is (true? (bool-of "TRUE")))
    (is (false? (bool-of "FALSE"))))
  (testing "ANY other value is nil — never a fabricated boolean"
    (doseq [v ["Yes" "no" "1" "0" "**True**" "true." "True (the transcript suffices)"
               "```true```" "maybe" "" "I think so"]]
      (is (nil? (bool-of v))
          (str "an uninterpretable boolean must not become a value the model "
               "never wrote — got a boolean for " (pr-str v)))))
  (testing "surrounding whitespace is not a different answer"
    (is (true? (bool-of "  true  ")))))

;; ===========================================================================
;; The regression net: well-formed responses must parse IDENTICALLY.
;; This change deliberately WIDENS acceptance, so the guard is that nothing
;; already accepted moves. (SIO-5 banked the same corpus; this is its
;; in-repo, per-row form — the md5 lives in evidence/sio2.)
;; ===========================================================================

(def well-formed-corpus
  [["two string fields"
    "[[ ## answer ## ]]\nParis\n\n[[ ## reason ## ]]\nIt is the capital.\n"
    {:outputs [{:name :answer :spec :string} {:name :reason :spec :string}]}
    {:answer "Paris" :reason "It is the capital."}]
   ["boolean + string"
    "[[ ## is_true ## ]]\nTrue\n\n[[ ## note ## ]]\nbecause\n"
    {:outputs [{:name :is_true :spec :boolean} {:name :note :spec :string}]}
    {:is_true true :note "because"}]
   ["int and double"
    "[[ ## count ## ]]\n42\n\n[[ ## score ## ]]\n0.75\n"
    {:outputs [{:name :count :spec :int} {:name :score :spec :double}]}
    {:count 42 :score 0.75}]
   ["int with prose stays loud"
    "[[ ## count ## ]]\nabout seven\n\n[[ ## x ## ]]\ny\n"
    {:outputs [{:name :count :spec :int} {:name :x :spec :string}]}
    {:count "about seven" :x "y"}]
   ["enum stays a plain string"
    "[[ ## op ## ]]\nadd\n\n[[ ## why ## ]]\nreason\n"
    {:outputs [{:name :op :spec [:enum :add :support]} {:name :why :spec :string}]}
    {:op "add" :why "reason"}]
   ["vector of string (JSON)"
    "[[ ## items ## ]]\n[\"a\", \"b\"]\n\n[[ ## note ## ]]\nn\n"
    {:outputs [{:name :items :spec [:vector :string]} {:name :note :spec :string}]}
    {:items ["a" "b"] :note "n"}]
   ["vector cardinality repair (bare scalar)"
    "[[ ## items ## ]]\n\"only-one\"\n\n[[ ## note ## ]]\nn\n"
    {:outputs [{:name :items :spec [:vector :string]} {:name :note :spec :string}]}
    {:items ["\"only-one\""] :note "n"}]
   ["vector of map (JSON)"
    "[[ ## rows ## ]]\n[{\"k\": 1}]\n\n[[ ## note ## ]]\nn\n"
    {:outputs [{:name :rows :spec [:vector [:map [:k :int]]]} {:name :note :spec :string}]}
    {:rows [{:k 1}] :note "n"}]
   ["maybe string still yields the string \"null\" (D4, untouched here)"
    "[[ ## maybe_v ## ]]\nnull\n\n[[ ## note ## ]]\nn\n"
    {:outputs [{:name :maybe_v :spec [:maybe :string]} {:name :note :spec :string}]}
    {:maybe_v "null" :note "n"}]
   ["completed marker is stripped"
    "[[ ## answer ## ]]\nParis\n\n[[ ## reason ## ]]\ncapital\n\n[[ ## completed ## ]]\n"
    {:outputs [{:name :answer :spec :string} {:name :reason :spec :string}]}
    {:answer "Paris" :reason "capital"}]
   ["tight markers [[##f##]]"
    "[[##answer##]]\nParis\n\n[[##reason##]]\ncapital\n"
    {:outputs [{:name :answer :spec :string} {:name :reason :spec :string}]}
    {:answer "Paris" :reason "capital"}]
   ["extra whitespace inside markers"
    "[[   ##   answer   ##   ]]\nParis\n\n[[ ## reason ## ]]\ncapital\n"
    {:outputs [{:name :answer :spec :string} {:name :reason :spec :string}]}
    {:answer "Paris" :reason "capital"}]
   ["single string field, markers present"
    "[[ ## answer ## ]]\nParis\n"
    {:outputs [{:name :answer :spec :string}]}
    {:answer "Paris"}]
   ["single string field, whole-text fallback"
    "Paris is the capital of France."
    {:outputs [{:name :answer :spec :string}]}
    {:answer "Paris is the capital of France."}]
   ["single string field, fallback strips completed"
    "Paris.\n[[ ## completed ## ]]\ntrailing"
    {:outputs [{:name :answer :spec :string}]}
    {:answer "Paris.\n[[ ## completed ## ]]\ntrailing"}]
   ["missing field is nil"
    "[[ ## answer ## ]]\nParis\n"
    {:outputs [{:name :answer :spec :string} {:name :absent :spec :string}]}
    {:answer "Paris" :absent nil}]
   ["snake_case and kebab-case names"
    "[[ ## snake_name ## ]]\ns\n\n[[ ## kebab-name ## ]]\nk\n"
    {:outputs [{:name :snake_name :spec :string} {:name :kebab-name :spec :string}]}
    {:snake_name "s" :kebab-name "k"}]
   ["streaming, partial"
    "[[ ## a ## ]]\npar"
    {:outputs [{:name :a :spec :string} {:name :b :spec :string}]}
    {:a "par" :b nil}]
   ["empty response"
    ""
    {:outputs [{:name :a :spec :string} {:name :b :spec :string}]}
    {:a nil :b nil}]])

(deftest a-nil-response-is-nil-fields-not-a-nullpointerexception-test
  ;; Disclosed micro-change that rides with the extraction rewrite. A provider
  ;; can return a nil message content, and previously that threw NPE out of
  ;; `parse-output` — an unclassified exception that loses the usage, the model
  ;; and the field-level detail a caller needs. Nothing was read, so every
  ;; declared field is nil: the same shape as any other unreadable response.
  (testing "no content at all is every declared field nil"
    (is (= {:a nil :b nil} (sio/parse-output nil two-strings)))))

(deftest well-formed-responses-parse-unchanged-test
  (doseq [[label response spec expected] well-formed-corpus]
    (testing label
      (is (= expected (sio/parse-output response spec))))))
