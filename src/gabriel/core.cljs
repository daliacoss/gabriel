(ns gabriel.core
  (:require [reagent.core :as reagent]
            [cljs.js :as cljs]
            [shadow.cljs.bootstrap.browser :as boot]
            [hickory.core :as hickory]))

(defonce c-state (cljs/empty-state))

(defn eval-str [source cb]
  (cljs/eval-str
    c-state
    source
    "[gabriel]"
    {:eval cljs/js-eval
     :load (partial boot/load c-state)
     :ns   (symbol "gabriel.core")}
    cb))

(declare component?)

(def _ (atom nil))
(def logic-operators
  {:eq =
   :lt <
   :gt >
   :lte <=
   :gte >=
   :ne not=})

(defn some-rec [pred coll]
  (when (not (empty? coll))
    (let [x (first coll)]
      (cond
        (pred x) x
        (sequential? x) (recur pred x)
        () (recur pred (rest coll))))))

(defn replace-rec
  ([smap coll]
   (replace-rec smap coll identity))
  ([smap coll to]
   (to (map
        (fn [x]
            (if
              (sequential? x) (replace-rec smap x to)
              (get smap x x)))
        coll))))

(defn update-component-params [coll, k, v]
  (if (sequential? coll)
    (let [a (first coll), b (second coll)] 
      (into
        (if (component? a)
          (if (map? b)
            [a (assoc b k v)]
            [a {k v} (update-component-params b k v)])
          [(update-component-params a k v) (update-component-params b k v)])
        (map #(update-component-params %1 k v) (nthrest coll 2))))
    coll))

(defn atomize-vals [m]
  (zipmap (keys m) (map reagent/atom (vals m))))

(defn link [{:keys [to state]} & content] (do
  [:a {:href (str "#" (name to)) 
       :on-click
       #(reset! (state :current-page) (keyword to))}
   content]))

(defn book [{:keys [vars]}]
  (let [state (atomize-vals (assoc vars :current-page :start))]
    (fn [{:keys [pages]}]
      (let [p (pages @(state :current-page))]
        (into [:div {:class "gabriel-book"}]
              (update-component-params p :state state))))))

(defn reset [params]
  (let [state (params :state)
        params-except-state (dissoc params :state)]
    (doall (map
     #(reset! (state (key %)) (val %))
     params-except-state))
    nil))

(defn state
  ([params child] @((params :state) (keyword child)))
  ([params] (state params (params :var))))

(defn component-vector? [x]
  (-> x first component?))

(defn first=
  ([x xs] (= (first xs) x))
  ([x] #(first= x %)))

(defn firstp
  ([pred xs] (pred (first xs)))
  ([pred] #(firstp pred %)))

(defn- check-case [c x]

  (let [pred       (first c)
        op-entries (filter #(-> % key logic-operators) (second c))]
    (when (seq op-entries)
      (every? #(pred ((-> % key logic-operators) x (val %))) op-entries))))

(defn else [params child]
  (conj [else params] child))

(defn switch [params & children]
  (let [cases (filter
               ;; a case is any vector whose first element is a fn other than else
               (firstp #(and (fn? %) (not= % else)))
               children)
        x     @((params :state) (params :var))]
    (if-let [found (first (filter #(check-case % x) cases))]
      (get found 2)
      (do
        (get (first (filter (first= else) children)) 2)))))

(defn- debug [params & children]
  children)

(defn- _each [{:keys [state in wrapper]} f]
  (into
   (cond
     (nil? wrapper) [:span]
     (sequential? wrapper) (vec wrapper)
     (component? wrapper) [wrapper {:state state}]
     :else [wrapper])
   (map f @(state in))))

(defn each
  ([params child]
   (_each params (cond
                   (fn? child) child
                   (= child _) identity
                   :else #(replace-rec {_ %} child vec))))
  ([params & children]
   (_each params #(replace-rec {_ %} children vec))))

(def component? #{each link reset state switch else debug})

(def ^:private myvars
  {:contract-sealed false
   :foo ["one" "two" "three"]})

(def ^:private mypages
  {:start
   [[reset {:contract-sealed 1}]
    [:h3 "Gabriel example" " project"]
    [:p "Was this the face that launched a thousand ships," [:br]
     "And " [link {:to "homo-fuge"} "burned"] " the topless towers of "
     [:em "Ilium"] "?"]
    [:p
     [state :contract-sealed] [state {:var :contract-sealed}] [:br]
     [switch {:var :contract-sealed}
      [true? {:gt 2} "mow"]
      [true? {:lt 1} "moo"]
      [true? {:lt 0} "meow"]
      [else [link {:to "test-render-order"} "banana"]]
      ]]
    ]
   :test-render-order
   [[each {:in :foo} [:p [:strong _]]]
    [:p [link {:to "start"} "back to start"] ]]
   :homo-fuge
   [[:p "I see it plain; here in this place is writ," [:br]
     [:em "Homo, fuge"] ": yet shall not Faustus fly."]]})

(def ^:private my-current-page (reagent/atom :start))

(defn ^:export start []
  (reagent/render-component
   [book {:pages mypages
          :vars myvars}]
   (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (do (start)))

(defn ^:export stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
