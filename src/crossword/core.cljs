(ns crossword.core
  (:require
   [clojure.walk :refer [postwalk]]
   [clojure.core.async :as a]
   [clojure.string :as str]))


; New versions of the model are streamed through this channel.
; defonce (instead of def) preserves the value when page is reloaded due to code change.
(defonce changes (a/chan))

; The exclamation mark is a convention for denoting functions with side-effects.
(defn update-state! 
  "Applies f to the model and feeds the new model to the changes channel"
  [model f]
  (a/put! changes (f model)))

; defmulti creates a multimethod, allowing a different implementation
; to be called based on some condition. In this case we use a special
; renderer for onclick attribute to hide it, we don't want them in our DOM.
(defmulti render-attr first)
(defmethod render-attr :default [[key value]]
  (str (name key) "='" (str value) "'"))
(defmethod render-attr :onclick [_] "")

; Forward declaration
(declare html)

(defn render-element
  "Takes a 3-tuple and converts it into a HTML element string representation.
   Example: [:p {:class \"text\"} \"Hello world\" ]"
  [[type attrs & inner]]
  (str
   "<" (name type) (when-not (empty? attrs) " ") (str/join " " (map render-attr attrs)) ">"
   (html inner)
   "</" (name type) ">"))

; We use memoize as a very simple but powerful optimization:
; Thanks to Clojure using value equality (instead of reference equality)
; and thanks to pure functions (no side-effects),
; we can create a mapping from arguments to result with memoize.
; This will increase performance at the expence of higher memory use.
; The asterix notation isn't anything Clojure specific (special characters are allowed in names),
; We're just using it to denote it's not the one you should be using.
(defn html*
  "Converts a nested virtual DOM into a nested HTML string representation"
  [dom]
  (if (coll? dom)
    (if (map? (second dom))
      (render-element dom)
      (->> (map html dom)
           (str/join "\n")))
    dom
    ))
(def html (memoize html*))

(defn register-onclick!
  "Registers onclick handler for elements with onclick attribute defined"
  [model node]
  (let [{:keys [onclick id]} node]
    (when (and onclick id)
      (set!
       (-> js/document (.getElementById id) (.-onclick))
       #(update-state! model onclick))))
  node)

(defn refresh-view!
  "Updates the HTML DOM with the virtual DOM returned by applying rnd to model.
   Also walks through the virtual DOM and registers onclick handlers."
  [rnd model]
  (let [dom (rnd model)]
    (set!
     (.-innerHTML (-> js/document (.getElementById "app")))
     (html dom))
    (postwalk (partial register-onclick! model) dom)
    nil))




; Here ends the generic "react" implementation and the crossword implementation begins



; A vector of 3-tuples containing word, hint and number of leading empty space.
; Tuples are more idiomatic than maps for simple cases in Clojure.
(def words
  [["alejandro" "Köyhän miehen Carlos" 3]
   ["hipsuttelu" "Kops" 2]
   ["hauki" "Kolmannella heitolla" 6]
   ["pilalla" "Kaikki" 3]
   ["santit" "Velkojalle" 4]
   ["caverna" "Panun voittama" 1]
   ["partasuu" "Sukelluskohde" 0]
   ["saimaa" "Menomesta" 6]
   ["kruisailu" "Ajelu" 2]
   ["vetopäivä" "Tänään" 0]
   ["mölkky" "Hyrren voittama" 6]
   ["kraavi" "Lauantai-illan päätteeksi" 4]
   ["dyatlov" "Panun sanoja" 4]])

(def max-offset (apply max (map last words)))

(def initial-model
  {:words (mapv #(into [] (repeat (count (first %)) "-")) words)
   :cursor [0 0]})

; define history as an atom, a stateful object.
; The value of the atom is initially a list containing the initial model.
; Changes to an atom are "atomic" so it's safe to use with multithreading.
; Here we don't use multithreading, but atoms are still the simplest solution
; for stateful objects.
(defonce history (atom (list initial-model)))

(defn input
  "Returns new model with given character entered at cursor position. Moves cursor.
   Enter and Backspace are special cases"
  [model char]
  (case char
    "Enter" (update-state! model
                           (fn [model]
                             (-> model
                                 (assoc-in [:cursor 1] 0)
                                 (update-in [:cursor 0] #(min (dec (count words)) (inc %))))))

    "Backspace" (update-state! model
                               (fn [model]
                                 (-> model
                                     (assoc-in (concat [:words] (:cursor model)) "-")
                                     (update-in [:cursor 1] #(max (dec %) 0)))))

    (when (and (= 1 (count char)) (complement empty?) (re-seq #"[a-zA-ZäöÄÖåÅ]" char))
      (update-state! model
                     (fn [model]
                       (-> model
                           (assoc-in (concat [:words] (:cursor model)) char)
                           (update-in [:cursor 1] #(min (inc %) (dec (count (get-in model [:words (-> model :cursor first)])))))))))))

(defn move-cursor
  "Returns a new model with cursor position updated to given row and col"
  [row col model]
  (-> model
      (assoc-in [:cursor 0] row)
      (assoc-in [:cursor 1] col)))

(defn render-character
  "Returns a virtual DOM element representing one character slot on a row.
   The element has a onclick handler for moving the cursor to the slot."
  [is-word-correct offset [cr cc] row index char]
  (let [col (- index offset)]
    [:div {:class (str/join " " [(if (< col 0) "empty" "slot")
                                 (if (and (= row cr) (= col cc)) "active" "normal")
                                 (when (= col (- max-offset offset)) "main")
                                 (when is-word-correct "correct")])
           :id (str row "-" col)
           :onclick (partial move-cursor row col)}
     (when (not= "" char) [:p {:class "char"} char])]))

(defn render-word
  "Returns a virtual DOM element representing one row in the crossword grid"
  [cursor row [word _ offset] entered]
  [:div {:class "row"}
   (map-indexed (partial render-character
                         (= word (apply str entered))
                         offset
                         cursor
                         row)
                (concat (repeat offset "") entered))])

(defn render-hint
  "Returns a virtual DOM element represeting a hint for a word"
  [index [_ hint _]]
  [:p {:class "hint"} (str (inc index) ". " hint)])

(defn render
  "Returns a virtual DOM element representing the whole model"
  [model]
  [:div {}
   [[:div {:class "grid"}
     (map
      (partial render-word (:cursor model))
      (range)
      words
      (:words model))]
    [:div {:class "hints"}
     (map-indexed render-hint words)]]])

(defn undo!
  "Removes the latest entry in history and refreshes the view with the previous entry."
  []
  (swap! history rest)
  (refresh-view! render (first @history)))

; Add a global keydown handler for input
(-> js/document (.addEventListener "keydown" #(input (first @history) (.-key %))))

; Update the view with the initial model
(refresh-view! render (first @history))

; Start an asynchronous loop that pulls new models from the changes channel, 
; stores them in history and renders them.
(a/go-loop []
  (let [model (a/<! changes)]
    (swap! history #(conj % model))
    (refresh-view! render model))
  (recur))


; The comment block isn't evaluated, 
; but you can still evaluate things inside it (for example with Calva),
; so this is a neat way of storing temporary code blocks for trying out things.
(comment
  (reset! history (list initial-model))

  ; You can undo by evaluating these lines
  (undo!)
  (repeatedly 5 undo!)

  ; History can be observed in the editor
  (count @history)
  (first @history)

  ; You can try out things in the view by evaluating pieces of code
  (refresh-view! render (rand-nth @history))
  
  ; A simple virtual dom structure example
  (html [:div {:class "root"}
         [:p {} "Paragraph 1"]
         [:p {} "Paragraph 2"]
         [:p {} "Paragraph 3"]])
  
  ; Child elements can also be passed as a collection
  (html [:div {:class "root"}
         [[:p {} "Paragraph 1"]
          [:p {} "Paragraph 2"]
          [:p {} "Paragraph 3"]]])
  
  ; Because child elements can be a collection, this works too
  (html [:div {:class "root"}
         (range 1 10)])
  
  )
