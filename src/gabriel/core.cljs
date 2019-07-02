(ns gabriel.core
  (:require [reagent.core :as reagent] [cljs.js :as cljs]))

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
        (if (and (fn? a) (map? b))
          [a (assoc b k v)]
          [(update-component-params a k v) (update-component-params b k v)])
        (map #(update-component-params %1 k v) (nthrest coll 2))))
    coll))

(defn page [{:keys [current-page]} & content]
  (let [ucontent (update-component-params content :current-page current-page)]
    (if (some-rec #{:p} ucontent)
      (vec ucontent)
      (vec (cons :p ucontent)))))

(defn page-link [{:keys [id current-page]} & content]
  (do (println current-page)
  [:a {:href (str "#" (name id)) 
       :on-click #(reset! current-page (keyword id))}
   content]))

(defn book-viewer [{:keys [pages state]}]
  (let [p (pages @(state :current-page))]
    [:div {:class "gabriel-book"}
     (vec (concat [page state] p))
     ]))

(defonce mypages
  {:start
   ["Was this the face that launched a thousand ships," [:br]
    "And " [page-link {:id "homo-fuge"} "burned"] " the topless towers of " [:em "Ilium"] "?"]
   :homo-fuge
   [:p "I see it plain; here in this place is writ,", [:br]
    [:em "Homo, fuge"] ": yet shall not Faustus fly."]})

(defonce my-current-page (reagent/atom :start))

(defn start []
  (reagent/render-component
    [book-viewer {:pages mypages :state {:current-page my-current-page}}]
    (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (do (start)))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
