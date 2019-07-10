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

;; Views

(declare component?)

(defn some-rec [pred coll]
  (when (not (empty? coll))
    (let [x (first coll)]
      (cond
        (pred x) x
        (sequential? x) (recur pred x)
        () (recur pred (rest coll))))))

(defn update-component-params [coll, k, v]
  (if (sequential? coll)
    (let [a (first coll), b (second coll)] 
      (into
        (if (component? a)
          (if (map? b)
            [a (assoc b k v)]
            [a {k v} b])
          [(update-component-params a k v) (update-component-params b k v)])
        (map #(update-component-params %1 k v) (nthrest coll 2))))
    coll))

(defn atomize-vals [m]
  (zipmap (keys m) (map reagent/atom (vals m))))

(defn PageLink [{:keys [to state]} & content] (do
  [:a {:href (str "#" (name to)) 
       :on-click #(reset! (state :current-page) (keyword to))}
   content]))

(defn Book [{:keys [vars]}]
  (let [state (atomize-vals (assoc vars :current-page :start))]
    (fn [{:keys [pages]}]
      (let [p (pages @(state :current-page))]
        (into [:div {:class "gabriel-Book"}]
              (update-component-params p :state state))))))

(defn Reset [params]
  (let [state (params :state)
        params-except-state (dissoc params :state)]
    (doall (map
     #(reset! (state (key %)) (val %))
     params-except-state))
    nil))

(defn State [{:keys [state]} child]
  @(state (keyword child)))
  ;;@(get-in params [:state (-> :var params keyword)])))

(defn Case [params & more]
  (let [conditions (apply hash-map (drop-last more))]
    (-> more second type println)
    (get conditions
         @((params :state) (params :var))
         (last more))))

(def component? #{Reset, State, Case})

(def logic-operators
  {:eq =
   :lt <
   :gt >
   :lte <=
   :gte >=
   :ne not=})

['If {:var "contract-sealed"}]

[State #{:contract-sealed}]

['If {:? pos?}]

['Switch #{:contract-sealed}]
;;[State {:var :contract-sealed}
;;  [Case {:eq 3} ]
;;  [Case {:eq 4} ]
;;  [Else {} ]]

;(cljs.js/eval-str "(+ 1 1)")
(def myvars
  {:contract-sealed false})

(def mypages
  {:start
   [[Reset {:contract-sealed 1}]
    [:h3 "Gabriel example project"]
    [:p "Was this the face that launched a thousand ships," [:br]
     "And " [PageLink {:to "homo-fuge"} "burned"] " the topless towers of "
     [:em "Ilium"] "?"]
    [:p
     [State :contract-sealed] [:br]
     [Case {:var :contract-sealed} 1 [:strong "meow"] false "moo" "mow"]]]
   :homo-fuge
   [[:p [Case {:var :contract-sealed} 1 "meow" false "moo" "mow"]]
    [:p "I see it plain; here in this place is writ," [:br]
     [:em "Homo, fuge"] ": yet shall not Faustus fly."]]})

(def my-current-page (reagent/atom :start))

(eval-str "[1 2 3]" println)

(defn ^:export start []
  (reagent/render-component
   [Book {:pages mypages
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
