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
your own client — [litellm-clj](https://github.com/unravel-team/litellm-clj), a raw
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
 :optional    true           ; field may be absent (optional)
 :type        :image}        ; optional; :image marks a multimodal input
```

Note the deliberate overlap: the *container* is a spec, and each *field* also
carries a `:spec` — its Malli schema. Everything typed in `sio` is Malli.

## Quickstart

Turn a free-text support ticket into a **validated, typed decision** your code can
route on — the whole point of `sio`.

```clojure
(require '[sio.core :as sio])

(def triage
  {:inputs  [{:name :subject :spec :string :description "The ticket subject line"}
             {:name :body    :spec :string :description "The customer's full message"}]
   :outputs [{:name :category    :spec [:enum "billing" "bug" "feature-request" "account" "other"]
              :description "Primary topic of the ticket"}
             {:name :priority    :spec [:enum "low" "medium" "high" "urgent"]
              :description "How urgently it needs attention"}
             {:name :sentiment   :spec [:enum "angry" "frustrated" "neutral" "happy"]
              :description "The customer's tone"}
             {:name :summary     :spec :string
              :description "One-sentence summary of the issue"}
             {:name :next-steps  :spec [:vector :string]
              :description "Recommended actions, most important first"}
             {:name :confidence  :spec [:double {:min 0 :max 1}]
              :description "Confidence in this triage, from 0.0 to 1.0"}
             {:name :needs-human :spec :boolean
              :description "Whether a human specialist should handle it now"}]
   :instructions "Triage the support ticket. Reserve \"urgent\" for outages, security issues, or data loss."})
```

**1 — Render the contract into a prompt.** Every output field gets a
type-appropriate hint: enums say "just the value, no quotes", collections say
"respond with valid JSON", booleans say "True or False".

```clojure
(sio/spec->prompt triage)
;; =>
;; Your input fields are:
;; 1. `subject` (str): The ticket subject line
;; 2. `body` (str): The customer's full message
;; Your output fields are:
;; 1. `category` (one of: billing, bug, feature-request, account, other): Primary topic of the ticket
;; 2. `priority` (one of: low, medium, high, urgent): How urgently it needs attention
;; 3. `sentiment` (one of: angry, frustrated, neutral, happy): The customer's tone
;; 4. `summary` (str): One-sentence summary of the issue
;; 5. `next-steps` (json array of str): Recommended actions, most important first
;; 6. `confidence` (float): Confidence in this triage, from 0.0 to 1.0
;; 7. `needs-human` (bool): Whether a human specialist should handle it now
;; All interactions will be structured in the following way ...
;;
;; [[ ## subject ## ]]
;; {subject}
;;
;; [[ ## body ## ]]
;; {body}
;;
;; [[ ## category ## ]]
;; {category}        # note: respond with just the value, no quotes
;;
;; …  (priority, sentiment, summary blocks)  …
;;
;; [[ ## next-steps ## ]]
;; {next-steps}        # note: respond with valid JSON
;;
;; …  (confidence block)  …
;;
;; [[ ## needs-human ## ]]
;; {needs-human}        # note: the value you produce must be True or False
;; [[ ## completed ## ]]
;; In adhering to this structure, your instructions are: Triage the support ticket. …
```

**2 — Send it with your own LLM client** (`sio` never calls one): the rendered
prompt plus the injected `subject`/`body`. Say the model returns this text — call
it `reply`:

```
[[ ## category ## ]]
billing
[[ ## priority ## ]]
high
[[ ## sentiment ## ]]
frustrated
[[ ## summary ## ]]
The customer was charged twice for their annual plan and wants a refund.
[[ ## next-steps ## ]]
["Confirm the duplicate charge in Stripe", "Refund the extra payment", "Reply with the refund and timeline"]
[[ ## confidence ## ]]
0.93
[[ ## needs-human ## ]]
False
```

**3 — Parse it back into typed Clojure data.** Enums stay strings, the JSON array
becomes a vector, `0.93` a `double`, `False` a `boolean`:

```clojure
(sio/parse-output reply triage)
;; => {:category    "billing"
;;     :priority    "high"
;;     :sentiment   "frustrated"
;;     :summary     "The customer was charged twice for their annual plan and wants a refund."
;;     :next-steps  ["Confirm the duplicate charge in Stripe"
;;                   "Refund the extra payment"
;;                   "Reply with the refund and timeline"]
;;     :confidence  0.93      ; a double, not "0.93"
;;     :needs-human false}    ; a boolean, not "False"
```

**4 — Enforce the contract with Malli.** A model that invents a category outside
the enum is caught before it reaches your router:

```clojure
(sio/validate-outputs (:outputs triage) (sio/parse-output reply triage))
;; => the parsed map above — every field satisfies its spec

(sio/validate-outputs (:outputs triage) {:category "refund"})
;; throws ExceptionInfo: Validation failed for field :category
```

Now `(:priority result)` and `(:needs-human result)` are values you can `case` on
with confidence — no regexing the model's prose, no guessing.

## Nested data & forgiving parsing

The triage above used enums, a vector, a bounded double, and a boolean. Fields can
also be **nested maps**, rendered as a JSON hint and read back as Clojure data:

```clojure
(sio/parse-output
  "[[ ## profile ## ]]\n{\"lang\": \"en\", \"reviewed\": true}"
  {:outputs [{:name :profile
              :spec [:map [:lang :string] [:reviewed {:optional true} :boolean]]}]})
;; => {:profile {:lang "en" :reviewed true}}
```

Parsing is forgiving, so one stray token doesn't cost you the whole response: JSON
wrapped in ```` ```json ```` fences is unwrapped, and a value that can't be coerced
to its declared type comes back as the raw string instead of throwing — call
`validate-outputs` when you want the contract enforced.

## Function calling

For providers that support structured output via tool/function calling, generate
a tool definition from a spec's outputs and parse the arguments back out:

```clojure
(sio/outputs->tool-definition triage)
;; => {:type "function"
;;     :function
;;     {:name "submit_response"
;;      :description "Triage the support ticket. Reserve \"urgent\" for outages, ..."
;;      :parameters
;;      {:type "object"
;;       :properties {"category"   {:type "string"
;;                                  :enum ["billing" "bug" "feature-request" "account" "other"]
;;                                  :description "Primary topic of the ticket"}
;;                    "next-steps" {:type "array" :items {:type "string"}
;;                                  :description "Recommended actions, most important first"}
;;                    ,,,}  ; priority, sentiment, summary, confidence, needs-human
;;       :required ["category" "priority" "sentiment" "summary"
;;                  "next-steps" "confidence" "needs-human"]}}}

;; After the model calls the tool, hand sio the provider's response:
(sio/parse-tool-call-response provider-response (:outputs triage))
;; => {:category "billing" :priority "high" :sentiment "frustrated"
;;     :summary "..." :next-steps ["Confirm the charge" "Refund it"]
;;     :confidence 0.93 :needs-human false}
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

MIT — see [LICENSE](LICENSE). Copyright (c) 2026 ObneyAI.
