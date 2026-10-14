(ns sio.input-field-guide-test
  "The input field guide: the account of each SHOWN input field -- its name, its
   type and its declared meaning -- that opens a prompt's input section.

   Every test asserts what the model is shown (the rendered text), through the
   public interface, never how the renderer walks a schema."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [malli.core :as m]
            [sio.core :as sio]))

(deftest top-level-meaning-that-lives-only-in-a-description-reaches-the-guide-test
  (testing "a shown top-level input's description appears in the guide"
    (let [inputs [{:name :author-kind
                   :spec :string
                   :description "human means a staff member, never the customer's own words"}]
          guide (sio/input-field-guide inputs {:author-kind "human"})]
      (is (string? guide))
      (is (some-> guide (str/starts-with? "Your input fields are:\n")))
      (is (some-> guide
                  (str/includes?
                   "1. `author-kind` (str): human means a staff member, never the customer's own words"))))))

(deftest a-shown-field-without-a-description-is-listed-by-name-and-type-test
  (testing "no description means no meaning clause at all -- not a dangling colon"
    (let [inputs [{:name :subject :spec :string :description "The ticket subject line"}
                  {:name :attempts :spec :int}]
          guide (sio/input-field-guide inputs {:subject "x" :attempts 3})]
      (is (= (str "Your input fields are:\n"
                  "1. `subject` (str): The ticket subject line\n"
                  "2. `attempts` (int)")
             guide)))))

(deftest optionality-is-stated-in-the-type-parenthesis-never-on-the-name-test
  (testing "an optional field says so beside its type; its name is exactly the declared name"
    (let [inputs [{:name :nickname :spec :string :optional true :description "What they like to be called"}
                  {:name :region :spec [:maybe :string] :optional true}
                  {:name :email :spec :string :description "Contact address"}]
          guide (sio/input-field-guide inputs {:nickname "Al" :region nil :email "a@b.c"})]
      (is (= (str "Your input fields are:\n"
                  "1. `nickname` (str, optional): What they like to be called\n"
                  "2. `region` (str or null, optional)\n"
                  "3. `email` (str): Contact address")
             guide))
      (is (not (str/includes? guide "nickname?")))
      (is (not (str/includes? guide "`nickname (optional)`"))))))

(deftest image-inputs-are-not-in-the-guide-test
  (testing "an image input stays a content part: it is shown, but never explained in text"
    (let [inputs [{:name :question :spec :string :description "What the customer asked"}
                  {:name :photo :type :image :description "A photo of the damage"}]
          guide (sio/input-field-guide inputs {:question "q" :photo "data:image/png;base64,abc"})]
      (is (str/includes? guide "`question`") "precondition: the text sibling is listed")
      (is (not (str/includes? guide "photo")))
      (is (not (str/includes? guide "A photo of the damage")))))

  (testing "a contract whose only shown input is an image has no guide at all"
    (is (nil? (sio/input-field-guide [{:name :photo :type :image}] {:photo "data:image/png;base64,abc"})))))

(deftest an-input-the-request-does-not-carry-is-neither-explained-nor-listed-test
  (testing "only shown top-level inputs appear; an absent optional input leaves no trace"
    (let [inputs [{:name :subject :spec :string :description "The ticket subject line"}
                  {:name :coupon :spec :string :optional true
                   :description "Promo code the customer quoted"}]
          guide (sio/input-field-guide inputs {:subject "Refund please"})]
      (is (str/includes? guide "`subject` (str): The ticket subject line")
          "precondition: the shown sibling is explained")
      (is (not (str/includes? guide "coupon")))
      (is (not (str/includes? guide "Promo code")))))

  (testing "an input present with no value (nil) IS shown"
    (let [inputs [{:name :coupon :spec [:maybe :string] :optional true
                   :description "Promo code the customer quoted"}]]
      (is (str/includes? (sio/input-field-guide inputs {:coupon nil})
                         "`coupon` (str or null, optional): Promo code the customer quoted"))))

  (testing "a request that carries no input has no guide"
    (is (nil? (sio/input-field-guide [{:name :subject :spec :string}] {})))
    (is (nil? (sio/input-field-guide [{:name :subject :spec :string}] nil)))))

;; =============================================================================
;; The marker prompt's input section IS the guide
;; =============================================================================

(def ^:private triage
  {:inputs [{:name :subject :spec :string :description "The ticket subject line"}
            {:name :photo :type :image :description "A photo of the damage"}
            {:name :history-note :spec :string :optional true
             :description "Staff note; never the customer's words"}]
   :outputs [{:name :category :spec [:enum "billing" "bug"] :description "Primary topic"}
             {:name :urgent :spec :boolean :description "Is it urgent?"}]
   :instructions "Triage the ticket."})

(defn- input-section
  "The text of a rendered prompt that sits before its output section."
  [prompt]
  (first (str/split prompt #"Your output fields are:")))

(deftest the-prompts-input-section-is-the-guide-for-this-requests-values-test
  (let [values {:subject "Refund please" :photo "data:image/png;base64,abc"}
        guide (sio/input-field-guide (:inputs triage) values)
        prompt (sio/spec->prompt triage values)]
    (testing "the prompt opens with the guide, then the unchanged output section"
      (is (str/starts-with? prompt (str guide "\nYour output fields are:\n"))))
    (testing "an input the request does not carry is not in the input section"
      (is (str/includes? (input-section prompt) "`subject`") "precondition: a shown input is listed")
      (is (not (str/includes? (input-section prompt) "history-note")))
      (is (not (str/includes? (input-section prompt) "never the customer's words"))))))

;; The sections that follow the input section -- outputs, interaction markers,
;; instructions -- were captured from the renderer BEFORE the guide existed, for
;; this fixed contract. They must not move.
(def ^:private fixed-contract
  {:inputs [{:name :subject :spec :string :description "The ticket subject line"}
            {:name :photo :type :image :description "A photo"}
            {:name :history :spec [:vector [:map [:who :string] [:text :string]]]
             :description "Prior messages"}
            {:name :note :spec :string}]
   :outputs [{:name :category :spec [:enum "billing" "bug"] :description "Primary topic"}
             {:name :urgent :spec :boolean :description "Is it urgent?"}
             {:name :steps :spec [:vector :string] :description "Next steps"}
             {:name :detail :spec [:map [:a :int] [:b {:optional true} :string]] :description "Detail"}
             {:name :score :spec [:maybe [:or :int :double]]}]
   :instructions "Triage the ticket.\nBe brief."})

(def ^:private golden-output-section
  (str "Your output fields are:\n"
       "1. `category` (one of: billing, bug): Primary topic\n"
       "2. `urgent` (bool): Is it urgent?\n"
       "3. `steps` (json array of str): Next steps\n"
       "4. `detail` (json {a: int, b?: str}): Detail\n"
       "5. `score` (int or float, or null): \n"))

(def ^:private golden-header
  "All interactions will be structured in the following way, with the appropriate values filled in.\n\n")

(def ^:private golden-input-markers
  {:subject "[[ ## subject ## ]]\n{subject}\n\n"
   :history "[[ ## history ## ]]\n{history}\n\n"
   :note "[[ ## note ## ]]\n{note}\n\n"})

(def ^:private golden-output-markers-and-instructions
  (str "[[ ## category ## ]]\n{category}        # note: respond with just the value, no quotes\n\n"
       "[[ ## urgent ## ]]\n{urgent}        # note: the value you produce must be True or False\n\n"
       "[[ ## steps ## ]]\n{steps}        # note: respond with valid JSON\n\n"
       "[[ ## detail ## ]]\n{detail}        # note: respond with valid JSON\n\n"
       "[[ ## score ## ]]\n{score}        # note: respond with valid JSON\n"
       "[[ ## completed ## ]]\n"
       "In adhering to this structure, your instructions are: Triage the ticket.\nBe brief."))

(defn- golden-tail
  "Everything after the input section, as the renderer wrote it before the guide
  existed, with the input markers of `shown-inputs` only."
  [shown-inputs]
  (str golden-output-section
       golden-header
       (apply str (map golden-input-markers shown-inputs))
       golden-output-markers-and-instructions))

(defn- after-input-section [prompt]
  (subs prompt (str/index-of prompt "Your output fields are:")))

(deftest output-markers-and-instruction-sections-are-byte-identical-to-before-test
  (testing "the one-argument form: all of it, input markers included"
    (is (= (golden-tail [:subject :history :note])
           (after-input-section (sio/spec->prompt fixed-contract)))))
  (testing "the two-argument form: the same bytes, with the input markers of the shown inputs only"
    (doseq [[values shown] [[{} []]
                            [{:subject "s"} [:subject]]
                            [{:note "n" :subject "s"} [:subject :note]]
                            [{:subject "s" :history [{:who "a" :text "b"}] :note "n"} [:subject :history :note]]
                            [{:photo "data:image/png;base64,abc"} []]]]
      (is (= (golden-tail shown)
             (after-input-section (sio/spec->prompt fixed-contract values)))
          (pr-str values)))))

(deftest without-values-every-declared-input-is-shown-test
  (testing "the one-argument form keeps a complete account of the contract"
    (let [prompt (sio/spec->prompt fixed-contract)]
      (is (str/starts-with? prompt "Your input fields are:\n1. `subject` (str): The ticket subject line\n"))
      (is (str/includes? prompt "`history`"))
      (is (str/includes? prompt "`note` (str)\n"))
      (is (not (str/includes? (input-section prompt) "photo"))))))

;; =============================================================================
;; Nested and collection inputs (IFG-2)
;; =============================================================================

(def ^:private case-input
  "A map input whose meanings live at three depths of declaration: on the field,
   on a map entry's properties, and on a schema's own properties."
  {:name :case
   :description "The case under review"
   :spec [:map
          [:status {:description "Where the case stands"} [:enum "open" "closed"]]
          [:owner {:optional true :description "Staff member who holds the case"} :string]
          [:opened-by [:string {:description "Who opened the case"}]]]})

(deftest a-shown-map-input-explains-its-shown-nested-fields-beneath-it-test
  (testing "each nested field: its name, its type and its meaning, indented under its parent"
    (is (= (str "Your input fields are:\n"
                "1. `case` (json object): The case under review\n"
                "   1. `status` (one of: open, closed): Where the case stands\n"
                "   2. `owner` (str, optional): Staff member who holds the case\n"
                "   3. `opened-by` (str): Who opened the case")
           (sio/input-field-guide
            [case-input]
            {:case {:status "open" :owner "Al" :opened-by "Bo"}}))))

  (testing "the type line no longer lists every declared field inline"
    (is (not (str/includes?
              (sio/input-field-guide [case-input] {:case {:status "open"}})
              "json {")))))

(deftest an-absent-nested-field-is-neither-explained-nor-listed-anywhere-test
  (let [values {:case {:status "open" :opened-by "Bo"}} ; no :owner
        guide (sio/input-field-guide [case-input] values)
        prompt (sio/spec->prompt {:inputs [case-input]
                                  :outputs [{:name :verdict :spec :string}]}
                                 values)]
    (testing "precondition: the shown siblings ARE explained"
      (is (str/includes? guide "`status` (one of: open, closed): Where the case stands"))
      (is (str/includes? guide "`opened-by` (str): Who opened the case")))
    (testing "the absent field's name and meaning appear nowhere in the guide"
      (is (not (str/includes? guide "owner")))
      (is (not (str/includes? guide "Staff member who holds the case"))))
    (testing "nor anywhere in the prompt's input section (the old type line listed it)"
      (is (not (str/includes? (input-section prompt) "owner"))))
    (testing "the remaining fields are numbered as a list of their own"
      (is (str/includes? guide "   2. `opened-by`")))))

(deftest a-field-present-with-no-value-is-shown-test
  (let [contract {:name :case
                  :spec [:map
                         [:owner {:optional true :description "Staff member who holds the case"}
                          [:maybe :string]]
                         [:opened-by {:description "Who opened the case"} :string]
                         [:closed-at {:optional true :description "When it was closed"} [:maybe :string]]]}]
    (testing "a nested field present as nil is explained: no value can matter"
      (let [guide (sio/input-field-guide [contract] {:case {:owner nil :opened-by "Bo"}})]
        (is (str/includes? guide "`owner` (str or null, optional): Staff member who holds the case"))
        (is (str/includes? guide "`opened-by` (str): Who opened the case") "precondition: sibling shown")
        (is (not (str/includes? guide "closed-at")) "while a truly absent field is still not")))
    (testing "a map input that is itself nil is shown, with nothing beneath it"
      (is (= (str "Your input fields are:\n"
                  "1. `case` (json object)")
             (sio/input-field-guide [contract] {:case nil}))))))

(def ^:private messages-input
  {:name :messages
   :description "The conversation so far, oldest first"
   :spec [:vector
          [:map {:description "One message in the conversation"}
           [:author-kind {:description "human means a staff member, never the customer's own words"}
            [:enum "human" "customer"]]
           [:text {:description "What was written"} :string]
           [:sent-at {:optional true :description "When it was sent"} :string]
           [:attachment {:optional true :description "Name of an attached file"} :string]]]})

(defn- occurrences [haystack needle]
  (count (re-seq (re-pattern (java.util.regex.Pattern/quote needle)) haystack)))

(deftest a-collection-of-records-explains-its-item-fields-once-test
  (testing "one 'each item:' line, then the item fields beneath it"
    (is (= (str "Your input fields are:\n"
                "1. `messages` (json array of json object): The conversation so far, oldest first\n"
                "   each item: One message in the conversation\n"
                "      1. `author-kind` (one of: human, customer): human means a staff member, never the customer's own words\n"
                "      2. `text` (str): What was written")
           (sio/input-field-guide
            [messages-input]
            {:messages [{:author-kind "human" :text "Hello"}]}))))

  (testing "the item fields are explained exactly once, for any number of items"
    (doseq [n [1 2 50]]
      (let [items (vec (repeat n {:author-kind "customer" :text "Hi"}))
            guide (sio/input-field-guide [messages-input] {:messages items})]
        (is (= 1 (occurrences guide "`author-kind`")) (str n " items"))
        (is (= 1 (occurrences guide "never the customer's own words")) (str n " items"))
        (is (= 1 (occurrences guide "each item:")) (str n " items"))
        (is (= 1 (occurrences guide "`text`")) (str n " items"))))))

(deftest a-collections-item-fields-are-the-union-of-those-its-items-carry-test
  (let [items [{:author-kind "human" :text "Hello"}
               {:text "Thanks" :sent-at "2026-01-02"}
               {:text "Bye"}]
        guide (sio/input-field-guide [messages-input] {:messages items})]
    (testing "every field any item carries is explained, once, in declared order"
      (is (= (str "Your input fields are:\n"
                  "1. `messages` (json array of json object): The conversation so far, oldest first\n"
                  "   each item: One message in the conversation\n"
                  "      1. `author-kind` (one of: human, customer): human means a staff member, never the customer's own words\n"
                  "      2. `text` (str): What was written\n"
                  "      3. `sent-at` (str, optional): When it was sent")
             guide)))
    (testing "a field no item carries is neither explained nor listed"
      (is (not (str/includes? guide "attachment")))
      (is (not (str/includes? guide "Name of an attached file")))))

  (testing "an empty collection has no item to explain"
    (is (= (str "Your input fields are:\n"
                "1. `messages` (json array of json object): The conversation so far, oldest first")
           (sio/input-field-guide [messages-input] {:messages []})))))

(def ^:private payment-input
  {:name :payment
   :description "How the customer paid"
   :spec [:or
          [:map
           [:card-last4 {:description "Last four digits of the card"} :string]
           [:network {:description "Card network"} [:enum "visa" "mastercard"]]]
          [:map
           [:iban {:description "Bank account number"} :string]
           [:reference {:description "Transfer reference"} :string]]]})

(deftest a-union-shows-only-the-branch-that-is-present-test
  (testing "the bank-transfer branch is present: its fields are explained, the card branch's are not"
    (let [guide (sio/input-field-guide [payment-input]
                                       {:payment {:iban "GB00" :reference "R-1"}})]
      (is (= (str "Your input fields are:\n"
                  "1. `payment` (json object): How the customer paid\n"
                  "   1. `iban` (str): Bank account number\n"
                  "   2. `reference` (str): Transfer reference")
             guide))
      (doseq [untaken ["card-last4" "Last four digits of the card" "network" "Card network" "visa"]]
        (is (not (str/includes? guide untaken)) untaken))))

  (testing "the card branch is present: the mirror image"
    (let [guide (sio/input-field-guide [payment-input]
                                       {:payment {:card-last4 "4242" :network "visa"}})]
      (is (str/includes? guide "`card-last4` (str): Last four digits of the card"))
      (is (str/includes? guide "`network` (one of: visa, mastercard): Card network"))
      (doseq [untaken ["iban" "Bank account number" "reference" "Transfer reference"]]
        (is (not (str/includes? guide untaken)) untaken))))

  (testing "a collection whose items take both branches explains both, once each"
    (let [guide (sio/input-field-guide
                 [{:name :payments :spec [:vector (:spec payment-input)]}]
                 {:payments [{:iban "GB00" :reference "R-1"}
                             {:card-last4 "4242" :network "visa"}
                             {:iban "GB11" :reference "R-2"}]})]
      (doseq [shown ["`iban`" "`reference`" "`card-last4`" "`network`"]]
        (is (= 1 (occurrences guide shown)) shown)))))

(deftest enum-allowed-values-stay-visible-at-every-depth-test
  (let [contract {:name :ticket
                  :spec [:map
                         [:priority {:description "How urgent it is"} [:enum "low" "high"]]
                         [:channel {:description "Where it arrived"} [:enum :email :chat]]
                         [:tags {:description "Labels"} [:vector [:enum "vip" "bug"]]]
                         [:escalation {:optional true :description "Who it went to"}
                          [:maybe [:enum "tier-1" "tier-2"]]]
                         [:history {:description "Past touches"}
                          [:vector [:map [:kind {:description "What happened"}
                                          [:enum "opened" "replied"]]]]]]}
        guide (sio/input-field-guide
               [contract]
               {:ticket {:priority "low" :channel :email :tags ["vip"] :escalation nil
                         :history [{:kind "opened"}]}})]
    (doseq [line ["`priority` (one of: low, high): How urgent it is"
                  "`channel` (one of: email, chat): Where it arrived"
                  "`tags` (json array of one of: vip, bug): Labels"
                  "`escalation` (one of: tier-1, tier-2 or null, optional): Who it went to"
                  "`kind` (one of: opened, replied): What happened"]]
      (is (str/includes? guide line) line))))

(def ^:private staff-meaning
  "human means a staff member, never the customer's own words")

(deftest a-meaning-declared-at-several-places-is-written-once-test
  (testing "later places refer to the first by its field name"
    (let [inputs [{:name :messages
                   :description "The conversation so far"
                   :spec [:vector [:map [:author-kind {:description staff-meaning} :string]]]}
                  {:name :last-author-kind :spec :string :description staff-meaning}
                  {:name :note
                   :spec [:map [:written-by {:description staff-meaning} :string]
                          [:text {:description "What the note says"} :string]]}]
          guide (sio/input-field-guide
                 inputs
                 {:messages [{:author-kind "human"}]
                  :last-author-kind "human"
                  :note {:written-by "human" :text "Called back"}})]
      (is (= (str "Your input fields are:\n"
                  "1. `messages` (json array of json object): The conversation so far\n"
                  "   each item:\n"
                  "      1. `author-kind` (str): " staff-meaning "\n"
                  "2. `last-author-kind` (str): (same meaning as `author-kind`)\n"
                  "3. `note` (json object)\n"
                  "   1. `written-by` (str): (same meaning as `author-kind`)\n"
                  "   2. `text` (str): What the note says")
             guide))
      (is (= 1 (occurrences guide staff-meaning)))))

  (testing "when two places share a field NAME, the reference names the place, not the name"
    (let [inputs [{:name :buyer :description "The buyer"
                   :spec [:map [:role {:description staff-meaning} :string]]}
                  {:name :seller :description "The seller"
                   :spec [:map [:role {:description staff-meaning} :string]]}]
          guide (sio/input-field-guide inputs {:buyer {:role "human"} :seller {:role "human"}})]
      (is (= (str "Your input fields are:\n"
                  "1. `buyer` (json object): The buyer\n"
                  "   1. `role` (str): " staff-meaning "\n"
                  "2. `seller` (json object): The seller\n"
                  "   1. `role` (str): (same meaning as `buyer.role`)")
             guide))
      (is (= 1 (occurrences guide staff-meaning)))))

  (testing "a different meaning under the same name is written in full, not referenced"
    (let [inputs [{:name :buyer :spec [:map [:role {:description "The buyer's part in the deal"} :string]]}
                  {:name :seller :spec [:map [:role {:description "The seller's part in the deal"} :string]]}]
          guide (sio/input-field-guide inputs {:buyer {:role "x"} :seller {:role "y"}})]
      (is (str/includes? guide "The buyer's part in the deal"))
      (is (str/includes? guide "The seller's part in the deal"))
      (is (not (str/includes? guide "same meaning"))))))

(deftest meaning-is-explained-at-every-depth-test
  (let [account {:name :account
                 :description "The customer's account"
                 :spec [:map
                        [:profile {:description "Who they are"}
                         [:map
                          [:contact {:description "How to reach them"}
                           [:map
                            [:phone {:description "Mobile number, digits only"} :string]
                            [:fax {:optional true :description "Fax number"} :string]
                            [:preferred {:description "Best channel"} [:enum "sms" "call"]]]]
                          [:locale {:optional true :description "Language code"} :string]]]
                        [:plan {:description "Subscription tier"} :string]]}
        guide (sio/input-field-guide
               [account]
               {:account {:profile {:contact {:phone "0123" :preferred "sms"}} :plan "pro"}})]
    (is (= (str "Your input fields are:\n"
                "1. `account` (json object): The customer's account\n"
                "   1. `profile` (json object): Who they are\n"
                "      1. `contact` (json object): How to reach them\n"
                "         1. `phone` (str): Mobile number, digits only\n"
                "         2. `preferred` (one of: sms, call): Best channel\n"
                "   2. `plan` (str): Subscription tier")
           guide))
    (testing "absent fields at depth leave no trace"
      (is (not (str/includes? guide "fax")))
      (is (not (str/includes? guide "locale"))))))

(deftest nullable-records-and-collections-are-explained-when-present-test
  (let [contract {:name :order
                  :spec [:map
                         [:shipping {:optional true :description "Where it goes"}
                          [:maybe [:map [:city {:description "Destination city"} :string]
                                   [:zip {:optional true :description "Postal code"} :string]]]]
                         [:lines {:optional true :description "What was bought"}
                          [:maybe [:vector [:map [:sku {:description "Stock code"} :string]]]]]]}]
    (testing "present: the nullable record's fields and the nullable collection's item fields"
      (is (= (str "Your input fields are:\n"
                  "1. `order` (json object)\n"
                  "   1. `shipping` (json object or null, optional): Where it goes\n"
                  "      1. `city` (str): Destination city\n"
                  "   2. `lines` (json array of json object or null, optional): What was bought\n"
                  "      each item:\n"
                  "         1. `sku` (str): Stock code")
             (sio/input-field-guide
              [contract]
              {:order {:shipping {:city "Leeds"} :lines [{:sku "A-1"}]}}))))
    (testing "present as nil: the field is shown, with nothing beneath it"
      (is (= (str "Your input fields are:\n"
                  "1. `order` (json object)\n"
                  "   1. `shipping` (json object or null, optional): Where it goes\n"
                  "   2. `lines` (json array of json object or null, optional): What was bought")
             (sio/input-field-guide [contract] {:order {:shipping nil :lines nil}}))))))

(deftest a-meaning-over-several-lines-stays-under-its-field-test
  (let [contract {:name :case
                  :spec [:map
                         [:status {:description "Where the case stands.\nOpen means staff still owe a reply.\n\nClosed means done."}
                          [:enum "open" "closed"]]
                         [:owner {:description "Who holds it"} :string]]}]
    (is (= (str "Your input fields are:\n"
                "1. `case` (json object)\n"
                "   1. `status` (one of: open, closed): Where the case stands.\n"
                "   Open means staff still owe a reply.\n"
                "\n"
                "   Closed means done.\n"
                "   2. `owner` (str): Who holds it")
           (sio/input-field-guide [contract] {:case {:status "open" :owner "Al"}})))
    (testing "at the top level a multi-line meaning is written as declared"
      (is (= (str "Your input fields are:\n"
                  "1. `note` (str): Line one.\n"
                  "Line two.")
             (sio/input-field-guide [{:name :note :spec :string :description "Line one.\nLine two."}]
                                    {:note "x"}))))))

(deftest a-meaning-is-found-wherever-the-contract-declares-it-test
  (testing "on a map entry's properties, on the entry's own schema, or through a :maybe"
    (let [contract {:name :case
                    :spec [:map
                           [:a {:description "from the entry"} :string]
                           [:b [:string {:description "from the schema"}]]
                           [:c [:maybe [:string {:description "from inside a maybe"}]]]
                           [:d [:maybe {:description "from the maybe itself"} :string]]
                           [:e {:description "entry wins"} [:string {:description "schema loses"}]]]}
          guide (sio/input-field-guide [contract] {:case {:a "1" :b "2" :c "3" :d "4" :e "5"}})]
      (is (= (str "Your input fields are:\n"
                  "1. `case` (json object)\n"
                  "   1. `a` (str): from the entry\n"
                  "   2. `b` (str): from the schema\n"
                  "   3. `c` (str or null): from inside a maybe\n"
                  "   4. `d` (str or null): from the maybe itself\n"
                  "   5. `e` (str): entry wins")
             guide))))

  (testing "a collection's own properties carry its meaning and do not hide its item type"
    (let [contract {:name :case
                    :spec [:map
                           [:tags [:vector {:description "Labels" :min 1} :int]]
                           [:touches [:vector {:description "Past touches" :min 1}
                                      [:map [:kind {:description "What happened"} :string]]]]]}
          guide (sio/input-field-guide [contract] {:case {:tags [1] :touches [{:kind "call"}]}})]
      (is (str/includes? guide "`tags` (json array of int): Labels"))
      (is (str/includes? guide "`touches` (json array of json object): Past touches"))
      (is (str/includes? guide "`kind` (str): What happened"))))

  (testing "at the top level the field's own description wins over its schema's"
    (is (str/includes?
         (sio/input-field-guide [{:name :q :spec [:string {:description "schema"}] :description "field"}]
                                {:q "x"})
         "`q` (str): field")))

  (testing "an undescribed top-level field still shows the meaning its schema carries"
    (is (str/includes?
         (sio/input-field-guide [{:name :q :spec [:string {:description "schema"}]}] {:q "x"})
         "`q` (str): schema"))))

(deftest without-values-every-declared-field-is-shown-at-every-depth-test
  (let [contract {:inputs [case-input messages-input payment-input
                           {:name :photo :type :image :description "A photo"}]
                  :outputs [{:name :verdict :spec :string}]}
        section (input-section (sio/spec->prompt contract))]
    (testing "the optional map entry no request has yet carried is explained"
      (is (str/includes? section "`owner` (str, optional): Staff member who holds the case")))
    (testing "item fields, including the optional ones, once"
      (doseq [shown ["`sent-at`" "`attachment`" "`author-kind`"]]
        (is (= 1 (occurrences section shown)) shown)))
    (testing "every branch of a union"
      (is (str/includes? section "`card-last4`"))
      (is (str/includes? section "`iban`")))
    (testing "images are still excluded"
      (is (not (str/includes? section "photo"))))))

(deftest a-map-of-records-explains-its-value-fields-once-test
  (let [contract {:name :scores
                  :description "Score per reviewer"
                  :spec [:map-of :string
                         [:map [:value {:description "0 to 10"} :int]
                          [:note {:optional true :description "Free text"} :string]
                          [:flag {:optional true :description "Raised by the reviewer"} :boolean]]]}]
    (is (= (str "Your input fields are:\n"
                "1. `scores` (json object with str keys and json object values): Score per reviewer\n"
                "   each value:\n"
                "      1. `value` (int): 0 to 10\n"
                "      2. `note` (str, optional): Free text")
           (sio/input-field-guide
            [contract]
            {:scores {"ann" {:value 7} "bo" {:value 3 :note "meh"}}})))
    (testing "a map-of scalars has nothing beneath it"
      (is (= (str "Your input fields are:\n"
                  "1. `counts` (json object with str keys and int values): Tallies")
             (sio/input-field-guide
              [{:name :counts :spec [:map-of :string :int] :description "Tallies"}]
              {:counts {"a" 1}}))))))

(def ^:private dispatched-payment
  {:name :payment
   :description "How the customer paid"
   :spec [:multi {:dispatch :kind}
          [:card [:map
                  [:kind {:description "Payment kind"} [:= :card]]
                  [:last4 {:description "Last four digits of the card"} :string]]]
          [:transfer [:map
                      [:kind {:description "Payment kind"} [:= :transfer]]
                      [:iban {:description "Bank account number"} :string]]]]})

(deftest a-dispatched-union-shows-only-the-branch-its-value-selects-test
  (testing "the transfer branch is selected by the dispatch key"
    (let [guide (sio/input-field-guide [dispatched-payment] {:payment {:kind :transfer :iban "GB00"}})]
      (is (= (str "Your input fields are:\n"
                  "1. `payment` (json object): How the customer paid\n"
                  "   1. `kind` (str): Payment kind\n"
                  "   2. `iban` (str): Bank account number")
             guide))
      (is (not (str/includes? guide "last4")))
      (is (not (str/includes? guide "Last four digits")))))

  (testing "a collection whose items select different branches explains both, once each"
    (let [guide (sio/input-field-guide
                 [{:name :payments :spec [:vector (:spec dispatched-payment)]}]
                 {:payments [{:kind :card :last4 "4242"} {:kind :transfer :iban "GB00"} {:kind :card :last4 "1"}]})]
      (is (= 1 (occurrences guide "`last4`")))
      (is (= 1 (occurrences guide "`iban`")))
      (is (= 2 (occurrences guide "`kind`")) "named for each branch's own declaration")))

  (testing "a value that selects no branch explains no branch's fields"
    (let [guide (sio/input-field-guide [dispatched-payment] {:payment {:kind :cash}})]
      (is (= (str "Your input fields are:\n"
                  "1. `payment` (json object): How the customer paid")
             guide)))))

(deftest rendering-the-guide-never-throws-test
  (testing "shapes the guide does not expand are still listed, by name and type"
    (let [inputs [{:name :pair :spec [:tuple :string :int] :description "A pair"}
                  {:name :addr :spec [:schema {:registry {:t/addr [:map [:street :string]]}} :t/addr]}
                  {:name :loose :spec [:and :string [:fn seq]]}
                  {:name :untyped}
                  {:name :anything :spec :any}
                  {:name :flag :spec 'boolean?}
                  {:name :nothing :spec [:enum]}]
          values {:pair ["a" 1] :addr {:street "x"} :loose "s" :untyped 1 :anything 2 :flag true :nothing nil}
          guide (sio/input-field-guide inputs values)]
      (is (string? guide))
      (doseq [shown ["`pair`" "`addr`" "`loose`" "`untyped`" "`anything`" "`flag`" "`nothing`"]]
        (is (str/includes? guide shown) shown))
      (is (str/includes? guide "`pair` (json array): A pair"))
      (is (string? (sio/spec->prompt {:inputs inputs :outputs [{:name :o :spec :string}]})))))

  (testing "a value that does not have the declared shape explains nothing it cannot see"
    (let [inputs [{:name :a :spec [:map [:x {:description "X"} :string]] :description "A map"}
                  {:name :b :spec [:vector [:map [:y {:description "Y"} :string]]] :description "A list"}
                  {:name :c :spec [:or [:map [:z {:description "Z"} :string]] :int] :description "A union"}
                  {:name :d :spec [:map-of :string [:map [:w {:description "W"} :string]]] :description "A dict"}
                  {:name :e :spec [:multi {:dispatch :k} [:p [:map [:v {:description "V"} :string]]]]
                   :description "A dispatched union"}]
          guide (sio/input-field-guide inputs {:a "not a map" :b {:y "not a list"} :c "neither branch"
                                               :d [1 2] :e 7})]
      (is (string? guide))
      (doseq [listed ["`a`" "`b`" "`c`" "`d`" "`e`"]]
        (is (str/includes? guide listed) listed))
      (doseq [hidden ["`x`" "`y`" "`z`" "`w`" "`v`"]]
        (is (not (str/includes? guide hidden)) hidden))))

  (testing "a set of records is a collection too"
    (let [guide (sio/input-field-guide
                 [{:name :ids :spec [:set [:map [:id {:description "Identifier"} :string]]]}]
                 {:ids #{{:id "a"} {:id "b"}}})]
      (is (= 1 (occurrences guide "`id`")))
      (is (str/includes? guide "`id` (str): Identifier")))))

(deftest no-field-name-is-ever-rendered-with-a-modifier-attached-test
  (let [inputs [{:name :evidence-sufficient? :spec :boolean :optional true :description "Whether it is enough"}
                {:name :record
                 :spec [:map
                        [:ok? {:optional true :description "Checked"} :boolean]
                        [:sent-at {:optional true :description "When"} :string]
                        [:billing/plan {:description "Plan"} :string]
                        [:items {:optional true :description "Items"}
                         [:vector [:map [:done? {:optional true :description "Done"} :boolean]]]]]}]
        guide (sio/input-field-guide
               inputs
               {:evidence-sufficient? true
                :record {:ok? true :sent-at "t" :billing/plan "pro" :items [{:done? false}]}})
        names (map second (re-seq #"(?m)^ *\d+\. `([^`]+)`" guide))]
    (is (= ["evidence-sufficient?" "record" "ok?" "sent-at" "billing/plan" "items" "done?"] names)
        "every name is exactly the declared name; optionality is only ever beside the type")
    (is (str/includes? guide "`evidence-sufficient?` (bool, optional)"))
    (is (str/includes? guide "`sent-at` (str, optional)"))
    (is (str/includes? guide "`done?` (bool, optional)"))))

(deftest a-large-partly-present-contract-explains-exactly-what-is-shown-test
  (let [rng (java.util.Random. 42)
        field-name (fn [section i] (keyword (str "s" section "-f" i)))
        ;; the closing period keeps "field 1." from also matching "field 10."
        meaning (fn [section i] (str "Meaning of section " section " field " i "."))
        ;; 12 sections of 25 described fields each: 300 declared described fields
        section-spec (fn [section]
                       (into [:map]
                             (for [i (range 25)]
                               [(field-name section i) {:optional true :description (meaning section i)}
                                :string])))
        inputs [{:name :records
                 :description "Many records"
                 :spec [:vector (into [:map {:description "One record"}]
                                      (for [section (range 6)]
                                        [(keyword (str "section-" section))
                                         {:optional true :description (str "Section " section)}
                                         (section-spec section)]))]}
                {:name :summary
                 :spec (into [:map]
                             (for [section (range 6 12)]
                               [(keyword (str "section-" section))
                                {:optional true :description (str "Section " section)}
                                (section-spec section)]))}]
        ;; each present field is chosen by the seeded RNG: roughly a third are present
        present (into {} (for [section (range 12) i (range 25)
                               :when (zero? (.nextInt rng 3))]
                           [[section i] true]))
        record-of (fn [section-ids]
                    (into {} (for [section section-ids
                                   :let [fields (into {} (for [i (range 25)
                                                               :when (present [section i])]
                                                           [(field-name section i) "v"]))]
                                   :when (seq fields)]
                               [(keyword (str "section-" section)) fields])))
        ;; the 50 records between them carry sections 0-5, one section each, round robin
        records (vec (for [r (range 50)] (record-of [(mod r 6)])))
        summary (record-of (range 6 12))
        guide (sio/input-field-guide inputs {:records records :summary summary})
        shown-fields (set (for [[[section i] _] present] [section i]))]
    (testing "preconditions: the fixture is large, and only partly present"
      (is (> (count shown-fields) 50))
      (is (< (count shown-fields) 300)))
    (testing "every present field is explained exactly once"
      (doseq [[section i] shown-fields]
        (is (= 1 (occurrences guide (meaning section i))) (str "section " section " field " i))))
    (testing "no absent field's name or meaning appears"
      (doseq [section (range 12) i (range 25)
              :when (not (present [section i]))]
        (is (not (str/includes? guide (str "`" (name (field-name section i)) "`"))))
        (is (not (str/includes? guide (meaning section i))))))
    (testing "the guide has one line per shown field plus its structure, nothing per item"
      (let [lines (str/split-lines guide)
            field-lines (filter #(re-find #"^ *\d+\. `s\d+-f\d+`" %) lines)]
        (is (= (count shown-fields) (count field-lines)))))))

;; =============================================================================
;; Added after a first round of mutations left gaps: unions inside unions, a nil
;; under a nullable union, the type of a collection of unions, references into
;; collection items, and values/forms the guide must survive.
;; =============================================================================

(deftest a-union-nested-in-a-union-narrows-its-type-too-test
  (let [contract {:name :payment
                  :spec [:or
                         [:or
                          [:map [:card-last4 {:description "Last four digits"} :string]]
                          [:map [:iban {:description "Bank account number"} :string]]]
                         :int]}
        guide (sio/input-field-guide [contract] {:payment {:iban "GB00"}})]
    (is (= (str "Your input fields are:\n"
                "1. `payment` (json object)\n"
                "   1. `iban` (str): Bank account number")
           guide))
    (is (not (str/includes? guide "card-last4")))))

(deftest a-nil-value-selects-no-branch-of-a-union-test
  (testing "the field is shown with its declared type; no branch is claimed present"
    (let [contract {:name :payment
                    :spec [:maybe [:or
                                   [:maybe [:map [:iban {:description "Bank account number"} :string]]]
                                   :string]]}
          guide (sio/input-field-guide [contract] {:payment nil})]
      (is (= (str "Your input fields are:\n"
                  "1. `payment` (json object or null or str, or null)")
             guide)))))

(deftest a-collection-of-unions-types-itself-by-the-branches-its-items-take-test
  (let [contract {:name :payments :spec [:vector (:spec payment-input)]}]
    (is (str/includes?
         (sio/input-field-guide [contract] {:payments [{:iban "A" :reference "1"} {:iban "B" :reference "2"}]})
         "1. `payments` (json array of json object)\n"))
    (testing "nullable collection of unions"
      (is (str/includes?
           (sio/input-field-guide [{:name :payments :spec [:maybe [:vector (:spec payment-input)]]}]
                                  {:payments [{:iban "A" :reference "1"}]})
           "1. `payments` (json array of json object or null)\n")))
    (testing "a map-of whose values are unions"
      (is (str/includes?
           (sio/input-field-guide [{:name :by-id :spec [:map-of :string (:spec payment-input)]}]
                                  {:by-id {"a" {:iban "A" :reference "1"}}})
           "1. `by-id` (json object with str keys and json object values)\n")))))

(deftest a-reference-into-collection-items-names-the-place-test
  (let [inputs [{:name :messages
                 :spec [:vector [:map [:author-kind {:description staff-meaning} :string]]]}
                {:name :drafts
                 :spec [:vector [:map [:author-kind {:description staff-meaning} :string]]]}]
        guide (sio/input-field-guide inputs {:messages [{:author-kind "human"}]
                                             :drafts [{:author-kind "human"}]})]
    (is (= (str "Your input fields are:\n"
                "1. `messages` (json array of json object)\n"
                "   each item:\n"
                "      1. `author-kind` (str): " staff-meaning "\n"
                "2. `drafts` (json array of json object)\n"
                "   each item:\n"
                "      1. `author-kind` (str): (same meaning as `messages[].author-kind`)")
           guide))))

(deftest a-meaning-on-the-items-themselves-is-written-once-too-test
  (let [inputs [{:name :a :spec [:vector [:map {:description "One entry"} [:x {:description "X"} :string]]]}
                {:name :b :spec [:vector [:map {:description "One entry"} [:y {:description "Y"} :string]]]}]
        guide (sio/input-field-guide inputs {:a [{:x "1"}] :b [{:y "2"}]})]
    (is (= 1 (occurrences guide "One entry")))
    (is (str/includes? guide "each item: (same meaning as `a[]`)"))))

(deftest the-guide-survives-forms-and-values-it-cannot-judge-test
  (testing "a union branch Malli cannot compile fits nothing, and nothing throws"
    (let [contract {:name :x :description "A union"
                    :spec [:or [:map [:a {:description "A"} [:ref :no/such-schema]]]
                           [:map [:b {:description "B"} :string]]]}]
      (is (str/includes? (sio/input-field-guide [contract] {:x {:b "1"}}) "`b` (str): B"))
      (is (string? (sio/input-field-guide [contract] {:x {:a 1}})))))
  (testing "a dispatch that throws selects no branch, and nothing throws"
    (let [boom (fn [_] (throw (ex-info "boom" {})))]
      (is (= (str "Your input fields are:\n"
                  "1. `x` (json object): A dispatched union")
             (sio/input-field-guide
              [{:name :x :description "A dispatched union"
                :spec [:multi {:dispatch boom} [:p [:map [:v {:description "V"} :string]]]]}]
              {:x {:v "1"}})))))
  (testing "a dispatch that is not callable selects no branch, and nothing throws"
    (is (string? (sio/input-field-guide
                  [{:name :x :spec [:multi {:dispatch [:not :callable]}
                                    [:p [:map [:v {:description "V"} :string]]]]}]
                  {:x {:v "1"}}))))
  (testing "a description that is not text is no meaning, and nothing throws"
    (is (= (str "Your input fields are:\n"
                "1. `x` (str)\n"
                "2. `y` (json object)\n"
                "   1. `z` (str)")
           (sio/input-field-guide
            [{:name :x :spec :string :description :not-text}
             {:name :y :spec [:map [:z {:description 42} :string]] :description nil}]
            {:x "1" :y {:z "2"}}))))
  (testing "a multi branch tagged :malli.core/default is selected when no other tag is"
    (let [contract {:name :x
                    :spec [:multi {:dispatch :k}
                           [:a [:map [:a-only {:description "A only"} :string]]]
                           [:malli.core/default [:map [:other {:description "Other"} :string]]]]}
          guide (sio/input-field-guide [contract] {:x {:k :zzz :other "1"}})]
      (is (str/includes? guide "`other` (str): Other"))
      (is (not (str/includes? guide "a-only"))))))

(deftest a-collection-of-nothing-has-no-item-to-explain-test
  (let [records [:map {:description "One entry"} [:x {:description "X"} :string]]]
    (testing "items that are nil carry no fields and are not an item to describe"
      (is (= (str "Your input fields are:\n"
                  "1. `a` (json array of json object)")
             (sio/input-field-guide [{:name :a :spec [:vector records]}] {:a [nil nil]}))))
    (testing "values that are nil carry no fields and are not a value to describe"
      (is (= (str "Your input fields are:\n"
                  "1. `b` (json object with str keys and json object values)")
             (sio/input-field-guide [{:name :b :spec [:map-of :string records]}] {:b {"k" nil}}))))
    (testing "precondition: the same contract does describe a real item"
      (is (str/includes?
           (sio/input-field-guide [{:name :a :spec [:vector records]}] {:a [nil {:x "1"}]})
           "each item: One entry")))))

(deftest a-schema-whose-validator-throws-fits-nothing-and-nothing-escapes-test
  (let [boom (m/-simple-schema {:type :t/boom :pred (fn [_] (throw (ex-info "boom" {})))})
        contract {:name :x
                  :spec [:or
                         [:schema {:registry {:t/boom boom}} :t/boom]
                         [:map [:b {:description "B"} :string]]]}]
    (is (str/includes? (sio/input-field-guide [contract] {:x {:b "1"}}) "`b` (str): B"))))

(deftest a-leaf-is-typed-exactly-as-the-existing-type-rendering-types-it-test
  (testing "for every leaf shape the guide writes what spec->type-str writes"
    (doseq [spec [:string :int :double :float :boolean 'string? 'int?
                  [:maybe :string] [:or :int :double] [:maybe [:or :int :double]]
                  [:or :boolean :string] [:or [:vector :string] :string]
                  [:enum "a" "b"] [:enum :a :b] [:enum {:min 1} "a" "b"]
                  [:maybe [:enum "a" "b"]] [:vector :string] [:vector [:enum "a" "b"]]
                  [:set :int] [:tuple :string :int] [:string {:min 1}] :map :vector]]
      (let [guide (sio/input-field-guide [{:name :f :spec spec}] {:f nil})]
        (is (= (str "Your input fields are:\n1. `f` (" (sio/spec->type-str spec) ")") guide)
            (pr-str spec))))))

(deftest a-description-is-written-trimmed-and-a-blank-one-is-no-meaning-test
  (let [guide (sio/input-field-guide
               [{:name :padded :spec :string :description "  Padded.\n"}
                {:name :blank :spec :string :description "  \n "}]
               {:padded "x" :blank "y"})]
    (is (= (str "Your input fields are:\n"
                "1. `padded` (str): Padded.\n"
                "2. `blank` (str)")
           guide))))

(deftest without-values-dispatched-unions-and-maps-of-are-shown-too-test
  (let [section (input-section
                 (sio/spec->prompt
                  {:inputs [dispatched-payment
                            {:name :scores
                             :spec [:map-of :string
                                    [:map [:value {:description "0 to 10"} :int]]]}]
                   :outputs [{:name :verdict :spec :string}]}))]
    (testing "every branch of a dispatched union"
      (is (str/includes? section "`last4` (str): Last four digits of the card"))
      (is (str/includes? section "`iban` (str): Bank account number")))
    (testing "the value fields of a map-of"
      (is (str/includes? section "each value:"))
      (is (str/includes? section "`value` (int): 0 to 10")))))

;; =============================================================================
;; Values as a caller really sends them: a complex input arrives as JSON TEXT.
;;
;; The text below is built by `json/generate-string` on keyword-keyed maps and
;; vectors, exactly as a caller that serializes complex input values does: a
;; keyword key becomes the string `name`, a qualified one `ns/name`, a nil
;; becomes JSON null. A nil input itself arrives as "".
;; =============================================================================

(defn- sent
  "A complex input value as it is sent to the model: JSON text."
  [value]
  (json/generate-string value))

(def ^:private billing-case
  {:name :case
   :description "The case under review"
   :spec [:map
          [:status {:description "Where the case stands"} [:enum "open" "closed"]]
          [:owner {:optional true :description "Staff member who holds the case"} :string]
          [:opened-by {:description "Who opened the case"} :string]
          [:billing/plan {:description "Subscription plan"} :string]
          [:closed-at {:optional true :description "When it was closed"} [:maybe :string]]]})

(deftest a-map-input-sent-as-json-text-has-its-shown-nested-fields-explained-test
  (let [text (sent {:status "open" :opened-by "Bo" :billing/plan "pro" :closed-at nil})
        guide (sio/input-field-guide [billing-case] {:case text})]
    (testing "precondition: the value really is the text a JSON serializer writes"
      (is (string? text))
      (is (str/includes? text "\"billing/plan\":\"pro\""))
      (is (str/includes? text "\"closed-at\":null")))
    (testing "present fields are explained, including the qualified key and the JSON null"
      (is (= (str "Your input fields are:\n"
                  "1. `case` (json object): The case under review\n"
                  "   1. `status` (one of: open, closed): Where the case stands\n"
                  "   2. `opened-by` (str): Who opened the case\n"
                  "   3. `billing/plan` (str): Subscription plan\n"
                  "   4. `closed-at` (str or null, optional): When it was closed")
             guide)))
    (testing "the absent field is neither explained nor listed"
      (is (not (str/includes? guide "owner")))
      (is (not (str/includes? guide "Staff member who holds the case"))))))

(deftest text-that-is-not-json-is-shown-with-nothing-nested-test
  (let [shown-only (str "Your input fields are:\n"
                        "1. `case` (json object): The case under review")]
    (testing "a nil input arrives as the empty string"
      (is (= shown-only (sio/input-field-guide [billing-case] {:case ""}))))
    (testing "truncated JSON"
      (is (= shown-only (sio/input-field-guide [billing-case] {:case "{\"status\":\"op"}))))
    (testing "prose"
      (is (= shown-only (sio/input-field-guide [billing-case] {:case "see the attached note"}))))
    (testing "JSON that is not an object explains no field of an object"
      (is (= shown-only (sio/input-field-guide [billing-case] {:case "[1,2,3]"}))))
    (testing "a sibling input is still explained alongside the unparsable one"
      (is (str/includes?
           (sio/input-field-guide [billing-case {:name :note :spec :string :description "A note"}]
                                  {:case "{oops" :note "x"})
           "2. `note` (str): A note")))))

(deftest a-collection-sent-as-json-text-explains-its-item-fields-once-test
  (let [items [{:author-kind "human" :text "Hello"}
               {:text "Thanks" :sent-at "2026-01-02"}
               {:text "Bye" :sent-at nil}]
        guide (sio/input-field-guide [messages-input] {:messages (sent items)})]
    (testing "the union of the fields the items carry, once, in declared order; null is shown"
      (is (= (str "Your input fields are:\n"
                  "1. `messages` (json array of json object): The conversation so far, oldest first\n"
                  "   each item: One message in the conversation\n"
                  "      1. `author-kind` (one of: human, customer): human means a staff member, never the customer's own words\n"
                  "      2. `text` (str): What was written\n"
                  "      3. `sent-at` (str, optional): When it was sent")
             guide)))
    (testing "a field no item carries is neither explained nor listed"
      (is (not (str/includes? guide "attachment")))
      (is (not (str/includes? guide "Name of an attached file"))))
    (testing "once for any number of items"
      (let [many (sio/input-field-guide [messages-input]
                                        {:messages (sent (vec (repeat 50 {:author-kind "customer" :text "Hi"})))})]
        (is (= 1 (occurrences many "`author-kind`")))
        (is (= 1 (occurrences many "never the customer's own words")))))))

(deftest a-union-sent-as-json-text-shows-only-the-branch-its-value-is-in-test
  (testing "branches told apart by a string field, as sent"
    (let [guide (sio/input-field-guide [payment-input]
                                       {:payment (sent {:iban "GB00" :reference "R-1"})})]
      (is (str/includes? guide "`iban` (str): Bank account number") "precondition: the present branch")
      (is (not (str/includes? guide "card-last4")))
      (is (not (str/includes? guide "Last four digits of the card")))))

  (testing "branches told apart by a keyword value: JSON writes :transfer as \"transfer\""
    (let [by-kind {:name :payment
                   :description "How the customer paid"
                   :spec [:or
                          [:map [:kind {:description "Payment kind"} [:= :card]]
                           [:last4 {:description "Last four digits of the card"} :string]]
                          [:map [:kind {:description "Payment kind"} [:= :transfer]]
                           [:iban {:description "Bank account number"} :string]]]}
          guide (sio/input-field-guide [by-kind] {:payment (sent {:kind :transfer :iban "GB00"})})]
      (is (= (str "Your input fields are:\n"
                  "1. `payment` (json object): How the customer paid\n"
                  "   1. `kind` (str): Payment kind\n"
                  "   2. `iban` (str): Bank account number")
             guide))
      (is (not (str/includes? guide "last4")))))

  (testing "a dispatched union whose tags are keywords: the dispatch value arrives as a string"
    (let [guide (sio/input-field-guide [dispatched-payment]
                                       {:payment (sent {:kind :transfer :iban "GB00"})})]
      (is (= (str "Your input fields are:\n"
                  "1. `payment` (json object): How the customer paid\n"
                  "   1. `kind` (str): Payment kind\n"
                  "   2. `iban` (str): Bank account number")
             guide))
      (is (not (str/includes? guide "last4"))))))

(deftest a-nested-entry-is-matched-by-the-keyword-or-by-its-json-spelling-test
  (testing "a map already parsed from JSON with string keys: ns/name for a qualified keyword"
    (let [value (json/parse-string (sent {:status "open" :opened-by "Bo" :billing/plan "pro" :closed-at nil}))
          guide (sio/input-field-guide [billing-case] {:case value})]
      (is (= {"status" "open" "opened-by" "Bo" "billing/plan" "pro" "closed-at" nil} value)
          "precondition: string keys, as a JSON parser without keyword keys returns them")
      (is (= (str "Your input fields are:\n"
                  "1. `case` (json object): The case under review\n"
                  "   1. `status` (one of: open, closed): Where the case stands\n"
                  "   2. `opened-by` (str): Who opened the case\n"
                  "   3. `billing/plan` (str): Subscription plan\n"
                  "   4. `closed-at` (str or null, optional): When it was closed")
             guide))
      (is (not (str/includes? guide "owner")))))

  (testing "a declared key that is itself a string, in a map and in JSON text"
    (let [contract {:name :labels
                    :spec [:map ["name" {:description "Display name"} :string]
                           ["colour" {:optional true :description "Hex colour"} :string]]}
          expected (str "Your input fields are:\n"
                        "1. `labels` (json object)\n"
                        "   1. `name` (str): Display name")]
      (is (= expected (sio/input-field-guide [contract] {:labels {"name" "x"}})))
      (is (= expected (sio/input-field-guide [contract] {:labels (sent {:name "x"})})))))

  (testing "a union of records whose value has string keys"
    (let [guide (sio/input-field-guide [payment-input]
                                       {:payment (json/parse-string (sent {:iban "GB00" :reference "R-1"}))})]
      (is (str/includes? guide "`iban` (str): Bank account number"))
      (is (not (str/includes? guide "card-last4")))))

  (testing "a dispatched union whose value has string keys"
    (let [guide (sio/input-field-guide [dispatched-payment]
                                       {:payment (json/parse-string (sent {:kind :transfer :iban "GB00"}))})]
      (is (str/includes? guide "`iban` (str): Bank account number"))
      (is (not (str/includes? guide "last4"))))))

;; =============================================================================
;; The interaction-format markers match the guide
;; =============================================================================

(deftest the-markers-of-a-request-name-only-the-inputs-it-shows-test
  (let [spec {:inputs [{:name :subject :spec :string :description "The ticket subject line"}
                       {:name :coupon-code :spec :string :optional true
                        :description "Promo code the customer quoted"}
                       {:name :photo :type :image :description "A photo of the damage"}
                       {:name :history :spec [:vector :string] :optional true
                        :description "Earlier messages"}]
              :outputs [{:name :verdict :spec :string :description "The decision"}]
              :instructions "Decide."}
        prompt (sio/spec->prompt spec {:subject "Refund please" :photo "data:image/png;base64,abc"})]
    (testing "precondition: the shown input is in the guide and has its marker"
      (is (str/includes? prompt "`subject` (str): The ticket subject line"))
      (is (str/includes? prompt "[[ ## subject ## ]]\n{subject}")))
    (testing "the output markers are still there"
      (is (str/includes? prompt "[[ ## verdict ## ]]\n{verdict}")))
    (testing "an input the request does not carry is named nowhere in the prompt"
      (is (not (str/includes? prompt "coupon-code")))
      (is (not (str/includes? prompt "Promo code")))
      (is (not (str/includes? prompt "history")))
      (is (not (str/includes? prompt "Earlier messages"))))
    (testing "an image input is still not in the text"
      (is (not (str/includes? prompt "photo"))))
    (testing "a request that carries every input names every input"
      (let [full (sio/spec->prompt spec {:subject "s" :coupon-code "c" :history ["h"]})]
        (doseq [shown ["[[ ## subject ## ]]" "[[ ## coupon-code ## ]]" "[[ ## history ## ]]"]]
          (is (str/includes? full shown) shown))))
    (testing "the one-argument form keeps naming every declared input"
      (let [declared (sio/spec->prompt spec)]
        (doseq [shown ["[[ ## subject ## ]]" "[[ ## coupon-code ## ]]" "[[ ## history ## ]]"]]
          (is (str/includes? declared shown) shown))))))

(deftest text-nested-too-deep-to-parse-is-shown-with-nothing-nested-test
  (let [nested (fn [depth] (str (apply str (repeat depth "[")) (apply str (repeat depth "]"))))
        shown-only (str "Your input fields are:\n"
                        "1. `case` (json object): The case under review")]
    (testing "a JSON parser overflows its stack long before this depth"
      (is (= shown-only (sio/input-field-guide [billing-case] {:case (nested 100000)}))))
    (testing "a value nested too deep to walk is shown, and nothing escapes"
      (let [deep (reduce (fn [acc _] [acc]) [] (range 50000))
            value {"iban" "GB00" "reference" "R" "extra" deep}]
        (is (str/starts-with? (sio/input-field-guide [payment-input] {:payment value})
                              "Your input fields are:\n1. `payment`"))
        (is (str/starts-with? (sio/input-field-guide [dispatched-payment] {:payment {"kind" "transfer" "extra" deep}})
                              "Your input fields are:\n1. `payment`"))))
    (testing "deep but parseable text is read, and a union of records is still judged"
      (let [deep (str "{\"iban\":\"GB00\",\"reference\":\"R\",\"x\":" (nested 1500) "}")
            guide (sio/input-field-guide [payment-input] {:payment deep})]
        (is (str/includes? guide "`iban` (str): Bank account number"))
        (is (not (str/includes? guide "card-last4")))))))

(deftest only-text-that-is-a-json-object-or-array-is-read-as-json-test
  (let [contract {:name :answer
                  :description "Either free text or a record"
                  :spec [:or :string [:map [:code {:description "Error code"} :string]]]}]
    (testing "a text that merely looks like a JSON scalar is the string branch, not parsed"
      (doseq [text ["123" "null" "true" "\"quoted\""]]
        (is (= (str "Your input fields are:\n1. `answer` (str): Either free text or a record")
               (sio/input-field-guide [contract] {:answer text}))
            text)))
    (testing "a JSON object is the record branch, and its fields are explained"
      (is (= (str "Your input fields are:\n"
                  "1. `answer` (json object): Either free text or a record\n"
                  "   1. `code` (str): Error code")
             (sio/input-field-guide [contract] {:answer (sent {:code "E1"})}))))
    (testing "leading whitespace before the JSON is allowed"
      (is (str/includes? (sio/input-field-guide [contract] {:answer (str "\n  " (sent {:code "E1"}))})
                         "`code` (str): Error code")))))

(deftest a-union-is-judged-whichever-way-its-keys-and-values-are-spelled-test
  (let [explained (fn [contract value]
                    (sio/input-field-guide [contract] {:x value}))
        has? (fn [guide s] (str/includes? guide s))]
    (testing "string-declared keys, string-keyed value (raw as it is)"
      (let [contract {:name :x :spec [:or [:map ["iban" {:description "Bank account"} :string]]
                                      [:map ["last4" {:description "Card digits"} :string]]]}
            guide (explained contract {"iban" "GB00"})]
        (is (has? guide "`iban` (str): Bank account"))
        (is (not (has? guide "last4")))))
    (testing "string-declared keys with a keyword literal: the value reads back as a string"
      (let [contract {:name :x :spec [:or [:map ["kind" [:= :card]] ["last4" {:description "Card digits"} :string]]
                                      [:map ["kind" [:= :transfer]] ["iban" {:description "Bank account"} :string]]]}
            guide (explained contract {"kind" "transfer" "iban" "GB00"})]
        (is (has? guide "`iban` (str): Bank account"))
        (is (not (has? guide "last4")))))
    (testing "keyword-declared keys with a keyword literal, string-keyed value"
      (let [contract {:name :x :spec [:or [:map [:kind [:= :card]] [:last4 {:description "Card digits"} :string]]
                                      [:map [:kind [:= :transfer]] [:iban {:description "Bank account"} :string]]]}
            guide (explained contract (json/parse-string (sent {:kind :transfer :iban "GB00"})))]
        (is (has? guide "`iban` (str): Bank account"))
        (is (not (has? guide "last4")))))
    (testing "keyword-declared keys with a keyword literal, keyword-keyed value that already holds the keyword"
      (let [contract {:name :x :spec [:or [:map [:kind [:= :card]] [:last4 {:description "Card digits"} :string]]
                                      [:map [:kind [:= :transfer]] [:iban {:description "Bank account"} :string]]]}
            guide (explained contract {:kind :transfer :iban "GB00"})]
        (is (has? guide "`iban` (str): Bank account"))
        (is (not (has? guide "last4")))))
    (testing "a dispatch function that reads a string key, and a tag spelled as a string"
      (let [contract {:name :x :spec [:multi {:dispatch #(get % "kind")}
                                      ["card" [:map ["last4" {:description "Card digits"} :string]]]
                                      ["transfer" [:map ["iban" {:description "Bank account"} :string]]]]}
            guide (explained contract {"kind" "transfer" "iban" "GB00"})]
        (is (has? guide "`iban` (str): Bank account"))
        (is (not (has? guide "last4")))))))

(deftest string-keyed-values-are-read-at-every-depth-test
  (let [contract {:name :account
                  :description "The customer account"
                  :spec [:map
                         [:profile {:description "Who they are"}
                          [:map [:contact {:description "How to reach them"}
                                 [:map [:phone {:description "Mobile number"} :string]
                                  [:fax {:optional true :description "Fax number"} :string]]]]]
                         [:orders {:description "Past orders"}
                          [:vector [:map [:sku {:description "Stock code"} :string]
                                    [:gift/note {:optional true :description "Gift message"} :string]
                                    [:coupon {:optional true :description "Promo code"} :string]]]]]}
        value (json/parse-string
               (sent {:profile {:contact {:phone "0123"}}
                      :orders [{:sku "A-1"} {:sku "B-2" :gift/note "Happy birthday"}]}))
        guide (sio/input-field-guide [contract] {:account value})]
    (is (= (str "Your input fields are:\n"
                "1. `account` (json object): The customer account\n"
                "   1. `profile` (json object): Who they are\n"
                "      1. `contact` (json object): How to reach them\n"
                "         1. `phone` (str): Mobile number\n"
                "   2. `orders` (json array of json object): Past orders\n"
                "      each item:\n"
                "         1. `sku` (str): Stock code\n"
                "         2. `gift/note` (str, optional): Gift message")
           guide))
    (testing "absent at depth leaves no trace"
      (is (not (str/includes? guide "fax")))
      (is (not (str/includes? guide "coupon"))))
    (testing "the same text, parsed by the guide itself, reads identically"
      (is (= guide (sio/input-field-guide
                    [contract]
                    {:account (sent {:profile {:contact {:phone "0123"}}
                                     :orders [{:sku "A-1"} {:sku "B-2" :gift/note "Happy birthday"}]})}))))))

(deftest a-recursive-schema-over-a-value-too-deep-to-check-fits-nothing-test
  (let [deep (reduce (fn [acc _] [acc]) [] (range 50000))
        contract {:name :x
                  :spec [:or
                         [:schema {:registry {:t/tree [:or :int [:vector [:ref :t/tree]]]}} :t/tree]
                         [:map [:b {:description "B"} :string]]]}]
    (is (str/starts-with? (sio/input-field-guide [contract] {:x deep})
                          "Your input fields are:\n1. `x`"))
    (is (str/includes? (sio/input-field-guide [contract] {:x {:b "1"}}) "`b` (str): B"))))
