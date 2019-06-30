(ns gabriel.core
  (:require [reagent.core :as reagent]))

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (reagent/atom {:current-page :the-face}))

(defn some-rec [pred coll]
  (when (not (empty? coll))
    (let [x (first coll)]
      (cond
        (pred x) x
        (sequential? x) (recur pred x)
        () (recur pred (rest coll))))))

(defn page [{:keys [id title]} & content]
  (if (some-rec #{:p} content)
    [vec content]
    [vec (cons :p content)]))


(defn clicked-page-link [id]
  (swap! app-state assoc :current-page (keyword id)))

(defn page-link [{:keys [id]} & content]
  [:a {:href (str "#" id)
       :on-click #(clicked-page-link id)}
   content])

(defonce mypages
  {:the-face
   ["Was this the face that launched a thousand ships," [:br]
    "And " [page-link {:id "homo-fuge"} "burned"] " the topless towers of " [:em "Ilium"] "?"]
   :homo-fuge
   [:p "I see it plain; here in this place is writ,", [:br]
    [:em "Homo, fuge"] ": yet shall not Faustus fly."]})


(defn book [{:keys [pages]}]
  (let [k (:current-page @app-state)]
    (let [p (k pages)]
      [:div
       (apply vector (concat [page {:id k}] p))
       ])))

(defn start []
  (reagent/render-component [book {:pages mypages}]
                            (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
