(ns sio.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sio.core :as sio]))

;; =============================================================================
;; Prompt Rendering
;; =============================================================================

(deftest spec->prompt-test
  (testing "Basic prompt generation with inputs and outputs"
    (let [spec {:inputs [{:name :question
                          :spec :string
                          :description "The question to answer"}]
                :outputs [{:name :answer
                           :spec :string
                           :description "The answer"}]
                :instructions "Answer questions accurately."}
          prompt (sio/spec->prompt spec)]
      (is (string? prompt))
      (is (re-find #"Your input fields are:" prompt))
      (is (re-find #"Your output fields are:" prompt))
      (is (re-find #"question" prompt))
      (is (re-find #"answer" prompt))
      (is (re-find #"Answer questions accurately\." prompt))))

  (testing "Prompt generation with instructions including rules"
    (let [spec {:inputs [{:name :text :spec :string :description "Input text"}]
                :outputs [{:name :result :spec :string :description "Result"}]
                :instructions "Process text.\n\nSTRICT RULES:\n1. Be concise\n2. Be accurate"}
          prompt (sio/spec->prompt spec)]
      (is (re-find #"STRICT RULES:" prompt))
      (is (re-find #"Be concise" prompt))
      (is (re-find #"Process text\." prompt))))

  (testing "Prompt generation with boolean output type"
    (let [spec {:inputs [{:name :statement :spec :string :description "A statement"}]
                :outputs [{:name :is_true :spec :boolean :description "Is it true?"}]
                :instructions "Verify statements."}
          prompt (sio/spec->prompt spec)]
      (is (re-find #"True or False" prompt))))

  (testing "Prompt generation without instructions"
    (let [spec {:inputs [{:name :x :spec :string :description "Input"}]
                :outputs [{:name :y :spec :string :description "Output"}]}
          prompt (sio/spec->prompt spec)]
      (is (string? prompt))
      (is (not (re-find #"instructions" prompt))))))

;; =============================================================================
;; Output Parsing
;; =============================================================================

(deftest parse-output-test
  (testing "Parse string output"
    (let [spec {:outputs [{:name :answer :spec :string}]}
          response "[[ ## answer ## ]]\nParis"
          result (sio/parse-output response spec)]
      (is (= "Paris" (:answer result)))))

  (testing "Parse boolean output - True"
    (let [spec {:outputs [{:name :is_valid :spec :boolean}]}
          response "[[ ## is_valid ## ]]\nTrue"
          result (sio/parse-output response spec)]
      (is (true? (:is_valid result)))))

  (testing "Parse boolean output - true (lowercase)"
    (let [spec {:outputs [{:name :is_valid :spec :boolean}]}
          response "[[ ## is_valid ## ]]\ntrue"
          result (sio/parse-output response spec)]
      (is (true? (:is_valid result)))))

  (testing "Parse boolean output - False"
    (let [spec {:outputs [{:name :is_valid :spec :boolean}]}
          response "[[ ## is_valid ## ]]\nFalse"
          result (sio/parse-output response spec)]
      (is (false? (:is_valid result)))))

  (testing "Parse integer output"
    (let [spec {:outputs [{:name :count :spec :int}]}
          response "[[ ## count ## ]]\n42"
          result (sio/parse-output response spec)]
      (is (= 42 (:count result)))))

  (testing "Parse float output"
    (let [spec {:outputs [{:name :score :spec :double}]}
          response "[[ ## score ## ]]\n0.95"
          result (sio/parse-output response spec)]
      (is (= 0.95 (:score result)))))

  (testing "Parse multiple outputs"
    (let [spec {:outputs [{:name :answer :spec :string}
                          {:name :confidence :spec :double}
                          {:name :is_confident :spec :boolean}]}
          response (str "[[ ## answer ## ]]\nParis\n"
                        "[[ ## confidence ## ]]\n0.98\n"
                        "[[ ## is_confident ## ]]\nTrue")
          result (sio/parse-output response spec)]
      (is (= "Paris" (:answer result)))
      (is (= 0.98 (:confidence result)))
      (is (true? (:is_confident result)))))

  (testing "Parse output with extra whitespace"
    (let [spec {:outputs [{:name :answer :spec :string}]}
          response "[[ ## answer ## ]]\n  Paris  \n"
          result (sio/parse-output response spec)]
      (is (= "Paris" (:answer result)))))

  (testing "Parse output with multiline content"
    (let [spec {:outputs [{:name :explanation :spec :string}]}
          response "[[ ## explanation ## ]]\nLine 1\nLine 2\nLine 3"
          result (sio/parse-output response spec)]
      (is (= "Line 1\nLine 2\nLine 3" (:explanation result)))))

  (testing "Invalid integer returns original string"
    (let [spec {:outputs [{:name :count :spec :int}]}
          response "[[ ## count ## ]]\nnot-a-number"
          result (sio/parse-output response spec)]
      (is (= "not-a-number" (:count result)))))

  (testing "Invalid float returns original string"
    (let [spec {:outputs [{:name :score :spec :double}]}
          response "[[ ## score ## ]]\nnot-a-number"
          result (sio/parse-output response spec)]
      (is (= "not-a-number" (:score result)))))

  (testing "Strips a same-line [[ ## completed ## ]] marker from an extracted field"
    (let [spec {:outputs [{:name :answer :spec :string}]}
          response "[[ ## answer ## ]]\nParis [[ ## completed ## ]] trailer"
          result (sio/parse-output response spec)]
      (is (= "Paris" (:answer result)))))

  (testing "Strips a trailing [[ ## completed ## ]] marker in the single-field prose fallback"
    ;; The marker (and anything after it on the same line) is dropped; the
    ;; preceding prose becomes the field value.
    (let [spec {:outputs [{:name :answer :spec :string}]}
          response "Paris\n[[ ## completed ## ]] and same-line trailer"
          result (sio/parse-output response spec)]
      (is (= "Paris" (:answer result))))))

(deftest single-string-field-whole-text-fallback-test
  (testing "marker-less prose becomes the single string field's value"
    (is (= {:answer "Hello there!"}
           (sio/parse-output "Hello there!"
                             {:outputs [{:name :answer :spec :string}]}))))
  (testing "marker output still parses normally"
    (is (= {:answer "Hi."}
           (sio/parse-output "[[ ## answer ## ]]\nHi."
                             {:outputs [{:name :answer :spec :string}]}))))
  (testing "multi-field specs stay strict"
    (is (= {:a nil :b nil}
           (sio/parse-output "plain text"
                             {:outputs [{:name :a :spec :string}
                                        {:name :b :spec :string}]}))))
  (testing "non-string single fields stay strict"
    (is (= {:flag nil}
           (sio/parse-output "plain text"
                             {:outputs [{:name :flag :spec :boolean}]})))))

(deftest field-extraction-test
  (testing "Field name with underscores"
    (let [spec {:outputs [{:name :my_field :spec :string}]}
          response "[[ ## my_field ## ]]\nValue"
          result (sio/parse-output response spec)]
      (is (= "Value" (:my_field result)))))

  (testing "Field name with hyphens (keyword style)"
    (let [spec {:outputs [{:name :my-field :spec :string}]}
          response "[[ ## my-field ## ]]\nValue"
          result (sio/parse-output response spec)]
      (is (= "Value" (:my-field result))))))

;; =============================================================================
;; Malli Spec Tests
;; =============================================================================

(deftest spec->type-str-test
  (testing "Convert Malli specs to string types"
    (is (= "str" (sio/spec->type-str :string)))
    (is (= "int" (sio/spec->type-str :int)))
    (is (= "float" (sio/spec->type-str :double)))
    (is (= "float" (sio/spec->type-str :float)))
    (is (= "bool" (sio/spec->type-str :boolean))))

  (testing "Convert Malli predicate symbols to string types"
    (is (= "str" (sio/spec->type-str 'string?)))
    (is (= "int" (sio/spec->type-str 'int?)))
    (is (= "float" (sio/spec->type-str 'double?)))
    (is (= "bool" (sio/spec->type-str 'boolean?))))

  (testing "Convert vector specs"
    (is (= "str" (sio/spec->type-str [:string {:description "A string"}]))))

  (testing "Fallback for unknown types"
    (is (= "str" (sio/spec->type-str :unknown)))))

(deftest validation-test
  (testing "Valid field passes validation"
    (let [field {:name :age :spec :int :description "Age"}
          value 25]
      (is (= value (sio/validate-field field value)))))

  (testing "Invalid field throws exception"
    (let [field {:name :age :spec :int :description "Age"}
          value "not-a-number"]
      (is (thrown? Exception (sio/validate-field field value)))))

  (testing "Valid inputs pass validation"
    (let [fields [{:name :question :spec :string}
                  {:name :age :spec :int}]
          input {:question "What is 2+2?" :age 25}]
      (is (= input (sio/validate-inputs fields input)))))

  (testing "Invalid input throws exception"
    (let [fields [{:name :age :spec :int}]
          input {:age "not-a-number"}]
      (is (thrown? Exception (sio/validate-inputs fields input)))))

  (testing "Valid outputs pass validation"
    (let [fields [{:name :answer :spec :string}
                  {:name :score :spec :double}]
          output {:answer "Paris" :score 0.9}]
      (is (= output (sio/validate-outputs fields output)))))

  (testing "Invalid output throws exception"
    (let [fields [{:name :score :spec :double}]
          output {:score "high"}]
      (is (thrown? Exception (sio/validate-outputs fields output))))))

(deftest spec-with-malli-test
  (testing "Spec with Malli specs generates correct prompt"
    (let [spec {:inputs [{:name :question
                          :spec [:string {:description "The question"}]}]
                :outputs [{:name :answer
                           :spec [:string {:description "The answer"}]}]
                :instructions "Answer accurately."}
          prompt (sio/spec->prompt spec)]
      (is (string? prompt))
      (is (re-find #"Your input fields are:" prompt))
      (is (re-find #"Your output fields are:" prompt))
      (is (re-find #"question" prompt))
      (is (re-find #"answer" prompt))
      (is (re-find #"Answer accurately\." prompt))))

  (testing "Spec with multiple field types"
    (let [spec {:inputs [{:name :text :spec :string :description "Input text"}
                         {:name :count :spec :int :description "Number of items"}]
                :outputs [{:name :result :spec :string :description "Result"}
                          {:name :is_valid :spec :boolean :description "Is valid?"}
                          {:name :score :spec :double :description "Score"}]}
          prompt (sio/spec->prompt spec)]
      ;; Check that fields are present in the prompt
      (is (re-find #"text" prompt))
      (is (re-find #"count" prompt))
      (is (re-find #"result" prompt))
      (is (re-find #"is_valid" prompt))
      (is (re-find #"score" prompt))
      ;; Check that types are correctly converted
      (is (re-find #"str" prompt))
      (is (re-find #"int" prompt))
      (is (re-find #"bool" prompt))
      (is (re-find #"float" prompt)))))

;; =============================================================================
;; Complex Schema Tests
;; =============================================================================

(deftest complex-spec-test
  (testing "complex-spec? detects complex types"
    (is (sio/complex-spec? [:map [:x :string]]))
    (is (sio/complex-spec? [:vector :int]))
    (is (sio/complex-spec? [:sequential :string]))
    (is (sio/complex-spec? [:maybe :string]))
    (is (not (sio/complex-spec? [:enum "a" "b" "c"]))) ;; enums are plain values, not JSON
    (is (not (sio/complex-spec? :string)))
    (is (not (sio/complex-spec? :int)))
    (is (not (sio/complex-spec? [:string {:min 1}]))))

  (testing "complex-spec? detects bare-keyword complex types"
    (is (sio/complex-spec? :map))
    (is (sio/complex-spec? :map-of))
    (is (sio/complex-spec? :vector))
    (is (sio/complex-spec? :sequential))
    (is (sio/complex-spec? :set))
    (is (sio/complex-spec? :tuple))
    (is (not (sio/complex-spec? :enum))) ;; bare :enum is not treated as complex
    (is (not (sio/complex-spec? :string))))

  (testing "complex-spec? detects vector-head :map-of/:set/:tuple/:or/:and"
    (is (sio/complex-spec? [:map-of :string :string]))
    (is (sio/complex-spec? [:set :string]))
    (is (sio/complex-spec? [:tuple :string :int]))
    (is (sio/complex-spec? [:or :string :int]))
    (is (sio/complex-spec? [:and :string :string])))

  (testing "spec->type-str handles map schemas"
    (is (= "json {one: str, two: str}"
           (sio/spec->type-str [:map [:one :string] [:two :string]])))
    (is (= "json {name: str, age: int}"
           (sio/spec->type-str [:map [:name :string] [:age :int]]))))

  (testing "spec->type-str handles nested maps"
    (is (= "json {user: json {name: str}}"
           (sio/spec->type-str [:map [:user [:map [:name :string]]]]))))

  (testing "spec->type-str handles optional fields"
    (is (= "json {name: str, email?: str}"
           (sio/spec->type-str [:map [:name :string] [:email {:optional true} :string]]))))

  (testing "spec->type-str handles vector schemas"
    (is (= "json array of str"
           (sio/spec->type-str [:vector :string])))
    (is (= "json array of int"
           (sio/spec->type-str [:vector :int]))))

  (testing "spec->type-str handles enum schemas"
    (is (= "one of: a, b, c"
           (sio/spec->type-str [:enum "a" "b" "c"]))))

  (testing "spec->type-str handles maybe schemas"
    (is (= "str or null"
           (sio/spec->type-str [:maybe :string]))))

  (testing "spec->type-str handles map-of vector schemas"
    (is (= "json object with str keys and str values"
           (sio/spec->type-str [:map-of :string :string]))))

  (testing "spec->type-str: set/tuple vector forms fall through to head keyword"
    ;; Unlike malli-spec->json-schema, spec->type-str has no dedicated [:set ...]
    ;; or [:tuple ...] branch, so element info is dropped and only the head
    ;; keyword's description is used.
    (is (= "json array (unique items)" (sio/spec->type-str [:set :string])))
    (is (= "json array" (sio/spec->type-str [:tuple :string :int]))))

  (testing "spec->type-str handles bare complex keywords and :any"
    (is (= "json object" (sio/spec->type-str :map)))
    (is (= "json object" (sio/spec->type-str :map-of)))
    (is (= "json array" (sio/spec->type-str :vector)))
    (is (= "json array" (sio/spec->type-str :sequential)))
    (is (= "json array (unique items)" (sio/spec->type-str :set)))
    (is (= "json array" (sio/spec->type-str :tuple)))
    (is (= "any" (sio/spec->type-str :any)))))

(deftest parse-json-output-test
  (testing "Parse JSON object output"
    (let [spec {:outputs [{:name :data
                           :spec [:map [:name :string] [:age :int]]}]}
          response "[[ ## data ## ]]\n{\"name\": \"John\", \"age\": 30}"
          result (sio/parse-output response spec)]
      (is (= {:name "John" :age 30} (:data result)))))

  (testing "Parse JSON array output"
    (let [spec {:outputs [{:name :items :spec [:vector :string]}]}
          response "[[ ## items ## ]]\n[\"a\", \"b\", \"c\"]"
          result (sio/parse-output response spec)]
      (is (= ["a" "b" "c"] (:items result)))))

  (testing "A scalar singleton preserves a declared vector output type"
    (doseq [field-spec [[:vector :string]
                        [:maybe [:vector :string]]]]
      (let [spec {:outputs [{:name :mentions :spec field-spec}]}
            response "[[ ## mentions ## ]]\nCameron approved the proposal."
            result (sio/parse-output response spec)]
        (is (= ["Cameron approved the proposal."] (:mentions result))))))

  (testing "Parse nested JSON output"
    (let [spec {:outputs [{:name :user
                           :spec [:map [:profile [:map [:name :string]]]]}]}
          response "[[ ## user ## ]]\n{\"profile\": {\"name\": \"Alice\"}}"
          result (sio/parse-output response spec)]
      (is (= {:profile {:name "Alice"}} (:user result)))))

  (testing "Parse JSON wrapped in markdown code block"
    (let [spec {:outputs [{:name :data :spec [:map [:x :string]]}]}
          response "[[ ## data ## ]]\n```json\n{\"x\": \"hi\"}\n```"
          result (sio/parse-output response spec)]
      (is (= {:x "hi"} (:data result)))))

  (testing "Parse enum as plain string"
    (let [spec {:outputs [{:name :choice :spec [:enum "a" "b" "c"]}]}
          response "[[ ## choice ## ]]\nb"
          result (sio/parse-output response spec)]
      (is (= "b" (:choice result)))))

  (testing "Invalid JSON falls back to string"
    (let [spec {:outputs [{:name :data :spec [:map [:x :string]]}]}
          response "[[ ## data ## ]]\nnot valid json"
          result (sio/parse-output response spec)]
      (is (= "not valid json" (:data result)))))

  (testing "Mixed complex and simple outputs"
    (let [spec {:outputs [{:name :items :spec [:vector :string]}
                          {:name :count :spec :int}
                          {:name :valid :spec :boolean}]}
          response (str "[[ ## items ## ]]\n[\"x\", \"y\"]\n"
                        "[[ ## count ## ]]\n2\n"
                        "[[ ## valid ## ]]\nTrue")
          result (sio/parse-output response spec)]
      (is (= ["x" "y"] (:items result)))
      (is (= 2 (:count result)))
      (is (true? (:valid result))))))

(deftest spec-prompt-json-hint-test
  (testing "Prompt includes JSON hint for map outputs"
    (let [spec {:outputs [{:name :data
                           :spec [:map [:x :string]]
                           :description "Data object"}]}
          prompt (sio/spec->prompt spec)]
      (is (re-find #"respond with valid JSON" prompt))))

  (testing "Prompt includes JSON hint for vector outputs"
    (let [spec {:outputs [{:name :items
                           :spec [:vector :string]
                           :description "List of items"}]}
          prompt (sio/spec->prompt spec)]
      (is (re-find #"respond with valid JSON" prompt))))

  (testing "Prompt does not include JSON hint for simple outputs"
    (let [spec {:outputs [{:name :answer
                           :spec :string
                           :description "The answer"}]}
          prompt (sio/spec->prompt spec)]
      (is (not (re-find #"respond with valid JSON" prompt)))))

  (testing "Prompt includes 'no quotes' note for enum outputs"
    (let [spec {:outputs [{:name :choice
                           :spec [:enum "a" "b" "c"]
                           :description "Pick one"}]}
          prompt (sio/spec->prompt spec)]
      (is (re-find #"respond with just the value, no quotes" prompt))
      ;; enum is not a complex spec, so it must NOT get the JSON hint
      (is (not (re-find #"respond with valid JSON" prompt))))))

;; =============================================================================
;; JSON Schema Conversion Tests (for function calling)
;; =============================================================================

(deftest malli-spec->json-schema-test
  (testing "Primitive types convert to JSON Schema"
    (is (= {:type "string"} (sio/malli-spec->json-schema :string)))
    (is (= {:type "integer"} (sio/malli-spec->json-schema :int)))
    (is (= {:type "number"} (sio/malli-spec->json-schema :double)))
    (is (= {:type "number"} (sio/malli-spec->json-schema :float)))
    (is (= {:type "boolean"} (sio/malli-spec->json-schema :boolean)))
    (is (= {} (sio/malli-spec->json-schema :any))))

  (testing "Predicate symbols convert to JSON Schema"
    (is (= {:type "string"} (sio/malli-spec->json-schema 'string?)))
    (is (= {:type "integer"} (sio/malli-spec->json-schema 'int?)))
    (is (= {:type "number"} (sio/malli-spec->json-schema 'double?)))
    (is (= {:type "boolean"} (sio/malli-spec->json-schema 'boolean?))))

  (testing "Enum converts to JSON Schema with allowed values"
    (is (= {:type "string" :enum ["a" "b" "c"]}
           (sio/malli-spec->json-schema [:enum "a" "b" "c"])))
    (is (= {:type "string" :enum ["unchanged" "changed"]}
           (sio/malli-spec->json-schema [:enum :unchanged :changed])))
    (is (= {:type "string" :enum ["decision/unchanged" "decision/changed"]}
           (sio/malli-spec->json-schema
            [:enum :decision/unchanged :decision/changed])))
    (is (= {:type "string" :enum [":unchanged" "changed"]}
           (sio/malli-spec->json-schema [:enum ":unchanged" "changed"]))))

  (testing "Literal values convert to JSON Schema constants"
    (is (= {:const "invoke"}
           (sio/malli-spec->json-schema [:= :invoke])))
    (is (= {:const "decision/invoke"}
           (sio/malli-spec->json-schema [:= :decision/invoke])))
    (is (= {:const ":invoke"}
           (sio/malli-spec->json-schema [:= ":invoke"])))
    (is (= {:const false}
           (sio/malli-spec->json-schema [:= false]))))

  (testing "Or converts every structured alternative"
    (is (= {:anyOf
            [{:type "object"
              :properties {"action" {:const "invoke"}
                           "capability" {:type "string"}}
              :required ["action" "capability"]}
             {:type "object"
              :properties {"action" {:const "respond"}
                           "message" {:type "string"}}
              :required ["action" "message"]}]}
           (sio/malli-spec->json-schema
            [:or
             [:map [:action [:= :invoke]] [:capability :string]]
             [:map [:action [:= :respond]] [:message :string]]]))))

  (testing "Maybe converts to a JSON Schema null union"
    (is (= {:oneOf [{:type "string"} {:type "null"}]}
           (sio/malli-spec->json-schema [:maybe :string]))))

  (testing "Map converts to object with properties"
    (let [schema (sio/malli-spec->json-schema [:map [:name :string] [:age :int]])]
      (is (= "object" (:type schema)))
      (is (= {:type "string"} (get-in schema [:properties "name"])))
      (is (= {:type "integer"} (get-in schema [:properties "age"])))
      (is (= ["name" "age"] (:required schema)))))

  (testing "Map with optional fields"
    (let [schema (sio/malli-spec->json-schema [:map
                                               [:name :string]
                                               [:email {:optional true} :string]])]
      (is (= ["name"] (:required schema)))
      (is (some? (get-in schema [:properties "email"])))))

  (testing "Vector converts to array"
    (is (= {:type "array" :items {:type "string"}}
           (sio/malli-spec->json-schema [:vector :string])))
    (is (= {:type "array" :items {:type "integer"}}
           (sio/malli-spec->json-schema [:vector :int]))))

  (testing "Sequential converts to array"
    (is (= {:type "array" :items {:type "string"}}
           (sio/malli-spec->json-schema [:sequential :string]))))

  (testing "Set converts to array with uniqueItems"
    (is (= {:type "array" :items {:type "string"} :uniqueItems true}
           (sio/malli-spec->json-schema [:set :string]))))

  (testing "Map-of converts to object with additionalProperties"
    (is (= {:type "object" :additionalProperties {:type "string"}}
           (sio/malli-spec->json-schema [:map-of :keyword :string]))))

  (testing "Nested structures"
    (let [schema (sio/malli-spec->json-schema
                  [:map [:user [:map [:name :string]]]])]
      (is (= "object" (:type schema)))
      (is (= "object" (get-in schema [:properties "user" :type])))
      (is (= {:type "string"} (get-in schema [:properties "user" :properties "name"])))))

  (testing "Schema constraints are preserved"
    (is (= {:type "string" :minLength 1}
           (sio/malli-spec->json-schema [:string {:min 1}]))))

  (testing "Tuple converts to array with positional items"
    (is (= {:type "array"
            :items [{:type "string"} {:type "integer"}]
            :additionalItems false}
           (sio/malli-spec->json-schema [:tuple :string :int]))))

  (testing "Bare schemas follow Malli's arity requirements"
    (is (= {:type "object" :properties {}}
           (sio/malli-spec->json-schema :map)))
    (is (= {:type "array" :items [] :additionalItems false}
           (sio/malli-spec->json-schema :tuple)))
    (is (thrown? Exception (sio/malli-spec->json-schema :map-of)))
    (is (thrown? Exception (sio/malli-spec->json-schema :vector)))
    (is (thrown? Exception (sio/malli-spec->json-schema :sequential)))
    (is (thrown? Exception (sio/malli-spec->json-schema :set))))

  (testing "Composite schemas tolerate a leading Malli properties map (spec-children)"
    ;; Without the spec-children fix, the props map is mistaken for the child
    ;; schema and the element type collapses to string / leaks into the output.
    ;; The point of these cases is the CHILD TYPE (items/additionalProperties)
    ;; surviving the leading props map. The props map's :description is now also
    ;; carried onto the emitted schema — see
    ;; malli-description-properties-reach-the-json-schema-test.
    (is (= {:type "array" :items {:type "integer"} :description "x"}
           (sio/malli-spec->json-schema [:vector {:description "x"} :int])))
    (is (= {:type "array" :items {:type "integer"} :description "x"}
           (sio/malli-spec->json-schema [:sequential {:description "x"} :int])))
    (is (= {:type "array" :items {:type "integer"} :uniqueItems true :description "x"}
           (sio/malli-spec->json-schema [:set {:description "x"} :int])))
    (is (= {:type "object" :additionalProperties {:type "integer"} :description "x"}
           (sio/malli-spec->json-schema [:map-of {:description "x"} :string :int])))
    (is (= {:type "string" :enum ["a" "b"] :description "x"}
           (sio/malli-spec->json-schema [:enum {:description "x"} "a" "b"])))
    (is (= {:oneOf [{:type "integer"} {:type "null"}] :description "x"}
           (sio/malli-spec->json-schema [:maybe {:description "x"} :int])))
    (is (= {:type "array" :items [{:type "string"} {:type "integer"}]
            :additionalItems false :description "x"}
           (sio/malli-spec->json-schema [:tuple {:description "x"} :string :int])))))

(deftest outputs->tool-definition-test
  (testing "Creates valid tool definition from simple output"
    (let [spec {:outputs [{:name :answer :spec :string :description "The answer"}]
                :instructions "Answer the question"}
          tool-def (sio/outputs->tool-definition spec)]
      (is (= "function" (:type tool-def)))
      (is (= "submit_response" (get-in tool-def [:function :name])))
      (is (= "Answer the question" (get-in tool-def [:function :description])))
      (is (= "object" (get-in tool-def [:function :parameters :type])))
      (is (= {:type "string" :description "The answer"}
             (get-in tool-def [:function :parameters :properties "answer"])))
      (is (= ["answer"] (get-in tool-def [:function :parameters :required])))))

  (testing "Creates tool definition with multiple outputs"
    (let [spec {:outputs [{:name :score :spec :double :description "Score 0-1"}
                          {:name :valid :spec :boolean :description "Is valid"}]
                :instructions "Evaluate"}
          tool-def (sio/outputs->tool-definition spec)]
      (is (= {:type "number" :description "Score 0-1"}
             (get-in tool-def [:function :parameters :properties "score"])))
      (is (= {:type "boolean" :description "Is valid"}
             (get-in tool-def [:function :parameters :properties "valid"])))
      (is (= ["score" "valid"] (get-in tool-def [:function :parameters :required])))))

  (testing "Creates tool definition with complex output"
    (let [spec {:outputs [{:name :data
                           :spec [:map [:items [:vector :string]] [:count :int]]
                           :description "Result data"}]}
          tool-def (sio/outputs->tool-definition spec)]
      (is (= "object" (get-in tool-def [:function :parameters :properties "data" :type])))
      (is (= {:type "array" :items {:type "string"}}
             (get-in tool-def [:function :parameters :properties "data" :properties "items"])))))

  (testing "Uses canonical JSON spellings for keyword enum outputs"
    (let [spec {:outputs [{:name :outcome
                           :spec [:enum :unchanged :changed]
                           :description "Learning outcome"}]}
          tool-def (sio/outputs->tool-definition spec)]
      (is (= {:type "string"
              :enum ["unchanged" "changed"]
              :description "Learning outcome"}
             (get-in tool-def
                     [:function :parameters :properties "outcome"])))))

  (testing "Keeps a root union of maps structured in the tool definition"
    (let [spec {:outputs
                [{:name :decision
                  :spec [:or
                         [:map
                          [:action [:= :invoke]]
                          [:capability :string]
                          [:world-changing? :boolean]]
                         [:map
                          [:action [:= :respond]]
                          [:message :string]]]}]}
          decision-schema
          (get-in (sio/outputs->tool-definition spec)
                  [:function :parameters :properties "decision"])]
      (is (nil? (:type decision-schema)))
      (is (= 2 (count (:anyOf decision-schema))))
      (is (every? #(= "object" (:type %)) (:anyOf decision-schema)))
      (is (= {:const "invoke"}
             (get-in decision-schema
                     [:anyOf 0 :properties "action"])))))

  (testing "Uses default description when instructions not provided"
    (let [spec {:outputs [{:name :x :spec :string}]}
          tool-def (sio/outputs->tool-definition spec)]
      (is (= "Submit the structured response"
             (get-in tool-def [:function :description]))))))

(deftest parse-tool-call-response-test
  (testing "Parses arguments from a function-calling response"
    (let [outputs [{:name :answer :spec :string}
                   {:name :score :spec :double}]
          response {:choices [{:message {:tool-calls [{:function {:name "submit_response"
                                                                  :arguments "{\"answer\": \"Paris\", \"score\": 0.9}"}}]}}]}
          result (sio/parse-tool-call-response response outputs)]
      (is (= {:answer "Paris" :score 0.9} result))))

  (testing "Returns nil when there is no tool call"
    (let [outputs [{:name :answer :spec :string}]
          response {:choices [{:message {:content "no tool call here"}}]}]
      (is (nil? (sio/parse-tool-call-response response outputs)))))

  (testing "Keys map onto output field names"
    (let [outputs [{:name :label :spec :string}]
          response {:choices [{:message {:tool-calls [{:function {:arguments "{\"label\": \"spam\"}"}}]}}]}]
      (is (= {:label "spam"} (sio/parse-tool-call-response response outputs)))))

  (testing "Malformed JSON arguments yield nil (parse failure is swallowed)"
    (let [outputs [{:name :answer :spec :string}]
          response {:choices [{:message {:tool-calls [{:function {:arguments "{not json"}}]}}]}]
      (is (nil? (sio/parse-tool-call-response response outputs))))))

;; =============================================================================
;; Multimodal Image Input Tests
;; =============================================================================

(deftest build-message-content-test
  (testing "Text-only spec returns plain string"
    (let [spec {:inputs [{:name :question :spec :string :description "Q"}]
                :outputs [{:name :answer :spec :string :description "A"}]}
          result (sio/build-message-content spec "Hello" {:question "Hi"})]
      (is (string? result))
      (is (= "Hello" result))))

  (testing "Spec with image input returns vector of content parts"
    (let [spec {:inputs [{:name :question :spec :string :description "Q"}
                         {:name :photo :type :image :description "An image"}]
                :outputs [{:name :answer :spec :string :description "A"}]}
          result (sio/build-message-content spec "Describe this"
                                            {:question "What is this?" :photo "data:image/png;base64,abc123"})]
      (is (vector? result))
      (is (= 2 (count result)))
      (is (= {:type "text" :text "Describe this"} (first result)))
      (is (= {:type "image_url" :image_url {:url "data:image/png;base64,abc123"}}
             (second result)))))

  (testing "Single image value (not wrapped in seq) works"
    (let [spec {:inputs [{:name :img :type :image :description "Image"}]
                :outputs [{:name :out :spec :string}]}
          result (sio/build-message-content spec "prompt" {:img "https://example.com/img.png"})]
      (is (vector? result))
      (is (= 2 (count result)))
      (is (= "https://example.com/img.png"
             (get-in (second result) [:image_url :url])))))

  (testing "Multiple images from a lazy seq work"
    (let [spec {:inputs [{:name :photos :type :image :description "Photos"}]
                :outputs [{:name :out :spec :string}]}
          urls (map #(str "https://example.com/" % ".png") (range 3))
          result (sio/build-message-content spec "prompt" {:photos urls})]
      (is (vector? result))
      ;; 1 text part + 3 image parts
      (is (= 4 (count result)))
      (is (= "text" (:type (first result))))
      (is (every? #(= "image_url" (:type %)) (rest result)))))

  (testing "Nil image value is excluded"
    (let [spec {:inputs [{:name :img :type :image :description "Image"}]
                :outputs [{:name :out :spec :string}]}
          result (sio/build-message-content spec "prompt" {:img nil})]
      (is (vector? result))
      (is (= 1 (count result)))
      (is (= {:type "text" :text "prompt"} (first result))))))

(deftest validate-inputs-skips-image-fields-test
  (testing "Image fields are skipped during validation"
    (let [fields [{:name :question :spec :string :description "Q"}
                  {:name :photo :type :image :spec :string :description "Photo"}]
          ;; photo value is not a string — would fail :string validation if not skipped
          input {:question "What is this?" :photo ["data:image/png;base64,abc"]}]
      (is (= input (sio/validate-inputs fields input)))))

  (testing "Non-image fields still validate"
    (let [fields [{:name :count :spec :int :description "Count"}
                  {:name :photo :type :image :description "Photo"}]
          input {:count "not-a-number" :photo "data:image/png;base64,abc"}]
      (is (thrown? Exception (sio/validate-inputs fields input))))))

(deftest spec-prompt-excludes-image-inputs-test
  (testing "Image inputs are excluded from prompt template"
    (let [spec {:inputs [{:name :question :spec :string :description "The question"}
                         {:name :photo :type :image :description "A photo"}]
                :outputs [{:name :answer :spec :string :description "The answer"}]
                :instructions "Describe the image."}
          prompt (sio/spec->prompt spec)]
      ;; question should appear in the prompt
      (is (re-find #"question" prompt))
      ;; photo should NOT appear in field listing or interaction format
      (is (not (re-find #"photo" prompt))))))

;; =============================================================================
;; Streaming Parse Helpers
;; =============================================================================

(deftest parse-streaming-output-test
  (testing "Progressive parse of a single string field yields text-so-far"
    (let [spec {:outputs [{:name :answer :spec :string}]}]
      (is (= {:answer "Par"}
             (sio/parse-streaming-output "[[ ## answer ## ]]\nPar" spec)))
      (is (= {:answer "Paris"}
             (sio/parse-streaming-output "[[ ## answer ## ]]\nParis" spec)))))

  (testing "Delegates to parse-output (marker-less prose fallback)"
    (is (= {:answer "partial text"}
           (sio/parse-streaming-output "partial text"
                                       {:outputs [{:name :answer :spec :string}]})))))

(deftest parse-streaming-json-array-test
  (testing "Extracts complete scalar items from a JSON array"
    (is (= [1 2 3] (sio/parse-streaming-json-array "[1, 2, 3]"))))

  (testing "Returns empty vector when no array bracket present"
    (is (= [] (sio/parse-streaming-json-array "no array here"))))

  (testing "Decodes JSON objects (not EDN) with keyword keys"
    (is (= [{:a 1} {:b 2}]
           (sio/parse-streaming-json-array "[{\"a\": 1}, {\"b\": 2}]"))))

  (testing "Handles nested arrays/objects correctly"
    (is (= [[1 2] [3 4]]
           (sio/parse-streaming-json-array "[[1,2],[3,4]]"))))

  (testing "Ignores brackets and commas inside strings"
    (is (= ["a],b" "c"]
           (sio/parse-streaming-json-array "[\"a],b\", \"c\"]"))))

  (testing "Omits a still-incomplete trailing element"
    (is (= [{:a 1}]
           (sio/parse-streaming-json-array "[{\"a\": 1}, {\"b\":")))
    (is (= [1 2]
           (sio/parse-streaming-json-array "[1, 2, "))))

  (testing "Is safe on untrusted text — a #= reader macro is NOT evaluated"
    (let [marker (java.io.File. "/tmp/sio-rce-should-not-exist")]
      (.delete marker)
      (is (= [] (sio/parse-streaming-json-array
                 "[#=(spit \"/tmp/sio-rce-should-not-exist\" \"x\")]")))
      (is (not (.exists marker)) "reader macro must not have executed"))))

;; ===========================================================================
;; Malli :description properties must survive into the provider-facing schema.
;;
;; The hand-rolled converter previously ignored a schema's properties map, so
;; [:string {:description "..."}] fell through the "wrapped spec — recurse on
;; the first element" branch and emitted a bare {:type "string"}. Any guidance
;; authored below the top level of a function-calling output silently vanished
;; from what the model was shown, with no error. malli's own json-schema
;; transformer emits these, so this was also a divergence from Malli semantics.
;; ===========================================================================

(deftest malli-description-properties-reach-the-json-schema-test
  (testing "A primitive carries its :description"
    (is (= {:type "string" :description "The student's verbatim quote."}
           (sio/malli-spec->json-schema
            [:string {:description "The student's verbatim quote."}]))))

  (testing "A collection carries its :description (and keeps its item type)"
    (is (= {:type "array" :items {:type "string"}
            :description "3-5 specific factors that influenced this score."}
           (sio/malli-spec->json-schema
            [:vector {:description "3-5 specific factors that influenced this score."}
             :string]))))

  (testing "A union carries its :description"
    (is (= "Score from 0.0 to 1.0"
           (:description (sio/malli-spec->json-schema
                          [:or {:description "Score from 0.0 to 1.0"} :int :double])))))

  (testing "A :maybe carries its :description alongside its null union"
    (let [s (sio/malli-spec->json-schema [:maybe {:description "ACT, or null."} :int])]
      (is (= [{:type "integer"} {:type "null"}] (:oneOf s)))
      (is (= "ACT, or null." (:description s)))))

  (testing "NESTED descriptions survive — the actual regression"
    (let [s (sio/malli-spec->json-schema
             [:map {:description "Multi-dimension scoring for ONE program."}
              [:reasoning [:string {:description "2-3 sentences, produced BEFORE the score."}]]
              [:score [:or {:description "Financial fit 0.0-1.0; use net tuition, NOT raw tuition."}
                       :int :double]]
              [:key-factors [:vector {:description "3-5 specific factors."} :string]]])]
      (is (= "Multi-dimension scoring for ONE program." (:description s)))
      (is (= "2-3 sentences, produced BEFORE the score."
             (get-in s [:properties "reasoning" :description])))
      (is (= "Financial fit 0.0-1.0; use net tuition, NOT raw tuition."
             (get-in s [:properties "score" :description])))
      (is (= "3-5 specific factors."
             (get-in s [:properties "key-factors" :description])))))

  (testing "Descriptions on map entries survive"
    (is (= "Confidence from 0.0 to 1.0"
           (get-in (sio/malli-spec->json-schema
                    [:map
                     [:score {:description "Confidence from 0.0 to 1.0"}
                      :double]])
                   [:properties "score" :description]))))

  (testing "Nested descriptions reach the function-calling tool definition"
    (let [tool (sio/outputs->tool-definition
                {:instructions "Score this program."
                 :outputs [{:name :financial
                            :spec [:map [:score [:or {:description "Use net tuition, NOT raw tuition."}
                                                 :int :double]]]}]})
          score (get-in tool [:function :parameters :properties "financial"
                              :properties "score"])]
      (is (= "Use net tuition, NOT raw tuition." (:description score)))))

  (testing "An explicit field :description still wins over the schema's own"
    (let [tool (sio/outputs->tool-definition
                {:outputs [{:name :answer
                            :spec [:string {:description "from schema"}]
                            :description "from field"}]})]
      (is (= "from field"
             (get-in tool [:function :parameters :properties "answer" :description])))))

  (testing "Other Malli properties are preserved without adding a description"
    (is (= {:type "string" :minLength 1}
           (sio/malli-spec->json-schema [:string {:min 1}])))
    (is (= {:type "array" :items {:type "string"} :minItems 1}
           (sio/malli-spec->json-schema [:vector {:min 1} :string])))
    (is (= {:type "string" :enum ["a" "b"]}
           (sio/malli-spec->json-schema [:enum "a" "b"])))
    (is (= {:const false} (sio/malli-spec->json-schema [:= false])))))

;; ===========================================================================
;; Registry refs must be INLINED, not emitted as $ref/definitions.
;;
;; Malli's transformer emits {"$ref" "#/definitions/foo" "definitions" {...}}
;; for a registry reference. That pointer addresses the DOCUMENT ROOT, but a
;; field's schema is nested at parameters.properties.<field>, so the definitions
;; map lands nested too and the pointer cannot resolve. Consumers pass the tool
;; schema through untouched (litellm's transform-tools moves :parameters by
;; identity), so an unresolvable pointer reaches the provider verbatim — and
;; Gemini's function-declaration subset does not reliably support $ref at all.
;;
;; Inlining also has to be RECURSIVE: resolving only the top level leaves nested
;; refs behind, which looks correct on the first field you inspect.
;; ===========================================================================

(def ^:private flat-ref-spec
  [:schema {:registry {:t/addr [:map [:street :string]]}} :t/addr])

(def ^:private nested-ref-spec
  [:schema {:registry {:t/addr [:map [:street :string]]
                       :t/person [:map [:name :string] [:address :t/addr]]}}
   :t/person])

(deftest registry-refs-are-inlined-test
  (testing "a $ref's SIBLING keys survive inlining, and win over the definition (issue #2)"
    ;; Malli renders `[:schema {:description "..."} ::number]` as
    ;; {:$ref "#/definitions/..." :description "..."} — the annotation rides BESIDE the
    ;; pointer. Replacing the whole map with the resolved definition silently dropped
    ;; the authored guidance: the exact failure mode this branch set out to fix,
    ;; reintroduced one layer down.
    (let [spec [:schema {:registry {:t/num [:or :int :double]}}
                [:map [:weight [:schema {:description "Strength of preference, 0.0-1.0."}
                                :t/num]]]]
          weight (get-in (sio/malli-spec->json-schema spec) [:properties "weight"])]
      (is (= "Strength of preference, 0.0-1.0." (:description weight))
          "the sibling :description must survive the $ref resolution")
      (is (= "number" (:type weight))
          "…without losing the resolved definition itself")))

  (testing "a top-level registry ref is replaced by its target"
    (is (= {:type "object"
            :properties {"street" {:type "string"}}
            :required ["street"]}
           (sio/malli-spec->json-schema flat-ref-spec))))

  (testing "a NESTED registry ref is inlined too (one level of deref is not enough)"
    (let [s (sio/malli-spec->json-schema nested-ref-spec)]
      (is (= {:type "object" :properties {"street" {:type "string"}} :required ["street"]}
             (get-in s [:properties "address"]))
          "the nested ref must be the real object, not a dangling pointer")
      (is (= {:type "string"} (get-in s [:properties "name"])))))

  (testing "no $ref or definitions survives anywhere in the emitted schema"
    (doseq [[label spec] [["flat" flat-ref-spec] ["nested" nested-ref-spec]]]
      (let [rendered (pr-str (sio/malli-spec->json-schema spec))]
        (is (not (clojure.string/includes? rendered ":$ref"))
            (str label ": a $ref pointer cannot resolve once nested under a property"))
        (is (not (clojure.string/includes? rendered ":definitions"))
            (str label ": definitions must not be emitted inside a field schema")))))

  (testing "inlining does not disturb a ref-free schema"
    (is (= {:type "object"
            :properties {"a" {:type "string"}}
            :required ["a"]}
           (sio/malli-spec->json-schema [:map [:a :string]])))))

;; ===========================================================================
;; A union of integer|number is redundant — JSON Schema's `number` already
;; admits integers. Malli renders [:or :int :double] as that union, and wrapping
;; it in :maybe nests a union INSIDE a union:
;;
;;   {"oneOf":[{"anyOf":[{"type":"integer"},{"type":"number"}]},{"type":"null"}]}
;;
;; That is noise in the contract the model reads, and models comply with it
;; worse than with a plain type. Collapsing it changes nothing semantically.
;; ===========================================================================

(deftest redundant-numeric-unions-are-collapsed-test
  (testing "integer|number collapses to number"
    (is (= {:type "number"} (sio/malli-spec->json-schema [:or :int :double]))))

  (testing "the collapse survives :maybe, leaving a single level of nesting"
    (is (= {:oneOf [{:type "number"} {:type "null"}]}
           (sio/malli-spec->json-schema [:maybe [:or :int :double]]))))

  (testing "a description on the union is preserved"
    (is (= {:type "number" :description "Score from 0.0 to 1.0"}
           (sio/malli-spec->json-schema [:or {:description "Score from 0.0 to 1.0"} :int :double]))))

  (testing "collapsed inside a map property"
    (is (= {:type "number"}
           (get-in (sio/malli-spec->json-schema [:map [:gpa [:or :int :double]]])
                   [:properties "gpa"]))))

  (testing "a union that is NOT all-numeric is left alone"
    (let [s (sio/malli-spec->json-schema [:or :string :int])]
      (is (nil? (:type s)) "a string|integer union must stay a union")
      (is (= 2 (count (:anyOf s))))))

  (testing "a plain integer stays an integer — collapsing must not widen a lone type"
    (is (= {:type "integer"} (sio/malli-spec->json-schema :int)))
    (is (= {:type "number"} (sio/malli-spec->json-schema :double)))))

;; =============================================================================
;; Type rendering — DSCloj parity + union descriptions
;; =============================================================================

(deftest dscloj-prompt-parity-golden-test
  (testing "the marker prompt is a 1:1 port of DSCloj's module->prompt — verified
            byte-identical against DSCloj 0c76d42 for this module; this golden pins it"
    ;; If this fails after an intentional renderer change, update the golden AND
    ;; re-run the DSCloj side-by-side so the divergence is a decision, not drift.
    (let [module {:instructions "Extract financial context from the transcript."
                  :inputs  [{:name :transcript-corrected :spec :string
                             :description "The corrected transcript."}]
                  :outputs [{:name :reasoning :spec :string :description "Brief analysis first."}
                            {:name :efc-mentioned :spec [:maybe :string]
                             :description "EFC if mentioned; '' if not."}
                            {:name :financial-concerns :spec [:maybe [:vector :string]]
                             :description "Concerns; [] if none."}
                            {:name :knows-tops :spec [:maybe :boolean] :description "Tri-state."}]}
          prompt (sio/spec->prompt module)]
      (is (str/includes? prompt "`efc-mentioned` (str or null): EFC if mentioned; '' if not."))
      (is (str/includes? prompt "`financial-concerns` (json array of str or null): Concerns; [] if none."))
      (is (str/includes? prompt "`knows-tops` (bool or null): Tri-state.")))))

(deftest union-types-render-their-branches-test
  (testing "[:or ...] renders each branch instead of collapsing to \"str\" — a deliberate
            improvement over the inherited DSCloj behavior (its spec->type-str had no
            :or case and fell through to \"str\", mis-describing every union field)"
    (let [render (fn [spec]
                   (let [p (sio/spec->prompt {:inputs [] :instructions "x"
                                              :outputs [{:name :f :spec spec}]})]
                     (second (re-find #"`f` \(([^)]*)\)" p))))]
      (is (= "int or float" (render [:or :int :double]))
          "the rec pipeline's score fields — previously described to the model as str")
      (is (= "bool or str" (render [:or :boolean :string])))
      (is (= "json array of str or str" (render [:or [:vector :string] :string])))
      (is (= "int or float, or null" (render [:maybe [:or :int :double]]))
          ":maybe wraps the WHOLE union — comma disambiguates the null from the branches")
      (is (= "str" (render :string))
          "non-union rendering unchanged — parity with DSCloj holds everywhere else"))))
