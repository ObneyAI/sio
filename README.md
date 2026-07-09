# sio

**S**tructured **I**nput/**O**utput for LLM prompts, in Clojure.

`sio` manages the boundary between your program's data and a language model. You
declare a typed **spec** — the input/output contract — and `sio`:

- **renders** it into a prompt template (with typed field markers and per-type hints),
- **injects** your input values (including multimodal image parts),
- **parses** the model's response back into Clojure data (markers → JSON → type coercion),
- **validates** everything against [Malli](https://github.com/metosin/malli) schemas, and
- **emits** function-calling tool definitions for providers that support them.

`sio` is deliberately **provider-agnostic**: it never makes an LLM call. Bring
your own client — [litellm-clj](https://github.com/ObneyAI/litellm-clj), a raw
HTTP request, anything. `sio` gives you the prompt to send and turns whatever
comes back into validated data.

> `sio` follows the [DSPy](https://github.com/stanfordnlp/dspy) signature model —
> a declarative, typed I/O contract — but stays deliberately small: just the
> structured-I/O core, with no provider/router or LLM-calling machinery.

## Installation

`sio` is consumed as a git dependency. Add it to your `deps.edn`:

```clojure
{:deps {io.github.obneyai/sio
        {:git/url "https://github.com/ObneyAI/sio.git"
         :git/sha "<sha>"}}}
```

Its only runtime dependencies are `org.clojure/data.json` and `metosin/malli`.

## Concepts

A **spec** is the I/O contract — a plain map:

```clojure
{:inputs       [<field> ...]   ; fields you fill in
 :outputs      [<field> ...]   ; fields you want the model to produce
 :instructions "..."}          ; optional task description / rules
```

A **field** is a map:

```clojure
{:name        :answer        ; keyword identifier
 :spec        :string        ; a Malli schema — the field's type
 :description "The answer"   ; human-readable hint (optional)
 :type        :image}        ; optional; :image marks a multimodal input
```

Note the deliberate overlap: the *container* is a spec, and each *field* also
carries a `:spec` — its Malli schema. Everything typed in `sio` is Malli.

## Quickstart

```clojure
(require '[sio.core :as sio])

(def qa
  {:inputs  [{:name :question :spec :string  :description "The question to answer"}]
   :outputs [{:name :answer   :spec :string  :description "The answer"}
             {:name :confident :spec :boolean :description "Are you confident?"}]
   :instructions "Answer concisely and accurately."})

;; 1. Render the contract into a prompt template.
(sio/spec->prompt qa)
;; =>
;; "Your input fields are:
;;  1. `question` (str): The question to answer
;;  Your output fields are:
;;  1. `answer` (str): The answer
;;  2. `confident` (bool): Are you confident?
;;  All interactions will be structured in the following way, with the appropriate values filled in.
;;
;;  [[ ## question ## ]]
;;  {question}
;;
;;  [[ ## answer ## ]]
;;  {answer}
;;
;;  [[ ## confident ## ]]
;;  {confident}        # note: the value you produce must be True or False
;;  [[ ## completed ## ]]
;;  In adhering to this structure, your instructions are: Answer concisely and accurately."

;; 2. Inject inputs + call *your* LLM client (not sio's job) with the
;;    rendered prompt as the user message. Say it replies with:
(def llm-reply
  "[[ ## answer ## ]]\nParis\n[[ ## confident ## ]]\nTrue")

;; 3. Parse the reply back into typed Clojure data.
(sio/parse-output llm-reply qa)
;; => {:answer "Paris" :confident true}

;; 4. Validate against the Malli schemas (throws on mismatch).
(sio/validate-outputs (:outputs qa) (sio/parse-output llm-reply qa))
;; => {:answer "Paris" :confident true}
```

## Typed fields

Field `:spec`s are Malli schemas, so you get validation and prompt hints for free:

```clojure
(def analysis
  {:inputs  [{:name :text :spec :string}]
   :outputs [{:name :sentiment :spec [:enum "positive" "neutral" "negative"]}
             {:name :score     :spec [:double {:min 0 :max 1}]}
             {:name :keywords  :spec [:vector :string]}
             {:name :meta      :spec [:map [:lang :string] [:reviewed {:optional true} :boolean]]}]})

;; Complex fields (maps, vectors) get a "respond with valid JSON" hint in the
;; prompt, and parse-output reads them back as Clojure data:
(sio/parse-output
  (str "[[ ## sentiment ## ]]\npositive\n"
       "[[ ## score ## ]]\n0.87\n"
       "[[ ## keywords ## ]]\n[\"fast\", \"cheap\"]\n"
       "[[ ## meta ## ]]\n{\"lang\": \"en\", \"reviewed\": true}")
  analysis)
;; => {:sentiment "positive"
;;     :score 0.87
;;     :keywords ["fast" "cheap"]
;;     :meta {:lang "en" :reviewed true}}
```

Parsing is forgiving: JSON inside ```` ```json ```` fences is unwrapped, and a
value that can't be coerced to its declared type is returned as the raw string
rather than throwing (validate explicitly when you want strictness).

## Function calling

For providers that support structured output via tool/function calling, generate
a tool definition from a spec's outputs and parse the arguments back out:

```clojure
(sio/outputs->tool-definition qa)
;; => {:type "function"
;;     :function {:name "submit_response"
;;                :description "Answer concisely and accurately."
;;                :parameters {:type "object"
;;                             :properties {"answer"    {:type "string"  :description "The answer"}
;;                                          "confident" {:type "boolean" :description "Are you confident?"}}
;;                             :required ["answer" "confident"]}}}

;; After the model calls the tool, hand sio the provider response:
(sio/parse-tool-call-response provider-response (:outputs qa))
;; => {:answer "Paris" :confident true}
```

## Multimodal inputs

Mark an input field with `:type :image`; its value is a URL or data URI (or a
sequence of them). Image fields are excluded from the text template and emitted
as OpenAI-style content parts:

```clojure
(def caption
  {:inputs  [{:name :instruction :spec :string}
             {:name :photo :type :image :description "Photo to caption"}]
   :outputs [{:name :caption :spec :string}]})

(sio/build-message-content caption
                           "Write a caption."
                           {:photo "data:image/png;base64,iVBORw0KG..."})
;; => [{:type "text" :text "Write a caption."}
;;     {:type "image_url" :image_url {:url "data:image/png;base64,iVBORw0KG..."}}]
```

## Streaming

`parse-streaming-output` is `parse-output` applied to the text accumulated so
far — drive it from your own streaming loop (e.g. over a stream of provider
chunks). For a spec with a single string-typed output field, the accumulated
text parses progressively even before the field markers arrive:

```clojure
(def streamer {:outputs [{:name :answer :spec :string}]})

(sio/parse-streaming-output "The"         streamer) ; => {:answer "The"}
(sio/parse-streaming-output "The capital" streamer) ; => {:answer "The capital"}
(sio/parse-streaming-output "[[ ## answer ## ]]\nThe capital is Paris." streamer)
;; => {:answer "The capital is Paris."}
```

For streaming a JSON array, `parse-streaming-json-array` returns the complete
items decoded so far (a still-incomplete trailing item is omitted). It is safe
on untrusted model text — it decodes with `clojure.data.json` and never uses the
Clojure reader:

```clojure
(sio/parse-streaming-json-array "[{\"n\": 1}, {\"n\":")  ; => [{:n 1}]
```

## API

All public vars live in `sio.core`:

| Function | Purpose |
|---|---|
| `spec->prompt` | Render a spec into a prompt template |
| `build-message-content` | Inject inputs; produce string or multimodal content parts |
| `parse-output` | Parse a model's text response into typed data |
| `parse-streaming-output` | Progressive `parse-output` over accumulated text |
| `parse-streaming-json-array` | Extract complete items from a partial JSON array |
| `validate-inputs` / `validate-outputs` / `validate-field` | Malli validation |
| `outputs->tool-definition` | Emit a function-calling tool definition |
| `parse-tool-call-response` | Read output values from a function-call response |
| `spec->type-str` | Malli schema → human/prompt type string |
| `malli-spec->json-schema` | Malli schema → JSON Schema |
| `complex-spec?` | Does a Malli schema serialize as JSON? |

## Development

```bash
clojure -M:test        # run the test suite (kaocha)
clojure -M:kondo --lint src test   # lint
clojure -M:nrepl       # start an nREPL on :7888
```

## License

MIT.
