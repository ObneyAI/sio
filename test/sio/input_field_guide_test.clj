(ns sio.input-field-guide-test
  "The input field guide: the account of each SHOWN input field -- its name, its
   type and its declared meaning -- that opens a prompt's input section.

   Every test asserts what the model is shown (the rendered text), through the
   public interface, never how the renderer walks a schema."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
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

(def ^:private fixed-contract-tail-before-the-guide-existed
  (str "Your output fields are:\n"
       "1. `category` (one of: billing, bug): Primary topic\n"
       "2. `urgent` (bool): Is it urgent?\n"
       "3. `steps` (json array of str): Next steps\n"
       "4. `detail` (json {a: int, b?: str}): Detail\n"
       "5. `score` (int or float, or null): \n"
       "All interactions will be structured in the following way, with the appropriate values filled in.\n\n"
       "[[ ## subject ## ]]\n{subject}\n\n"
       "[[ ## history ## ]]\n{history}\n\n"
       "[[ ## note ## ]]\n{note}\n\n"
       "[[ ## category ## ]]\n{category}        # note: respond with just the value, no quotes\n\n"
       "[[ ## urgent ## ]]\n{urgent}        # note: the value you produce must be True or False\n\n"
       "[[ ## steps ## ]]\n{steps}        # note: respond with valid JSON\n\n"
       "[[ ## detail ## ]]\n{detail}        # note: respond with valid JSON\n\n"
       "[[ ## score ## ]]\n{score}        # note: respond with valid JSON\n"
       "[[ ## completed ## ]]\n"
       "In adhering to this structure, your instructions are: Triage the ticket.\nBe brief."))

(defn- after-input-section [prompt]
  (subs prompt (str/index-of prompt "Your output fields are:")))

(deftest output-markers-and-instruction-sections-are-byte-identical-to-before-test
  (testing "the one-argument form"
    (is (= fixed-contract-tail-before-the-guide-existed
           (after-input-section (sio/spec->prompt fixed-contract)))))
  (testing "the two-argument form, for any set of values"
    (doseq [values [{} {:subject "s"} {:subject "s" :history [{:who "a" :text "b"}] :note "n"}]]
      (is (= fixed-contract-tail-before-the-guide-existed
             (after-input-section (sio/spec->prompt fixed-contract values)))
          (pr-str values)))))

(deftest without-values-every-declared-input-is-shown-test
  (testing "the one-argument form keeps a complete account of the contract"
    (let [prompt (sio/spec->prompt fixed-contract)]
      (is (str/starts-with? prompt "Your input fields are:\n1. `subject` (str): The ticket subject line\n"))
      (is (str/includes? prompt "`history`"))
      (is (str/includes? prompt "`note` (str)\n"))
      (is (not (str/includes? (input-section prompt) "photo"))))))
