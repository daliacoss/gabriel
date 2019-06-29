(ns gabriel.core
  (:require [reagent.core :as reagent]))

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (reagent/atom {:current-passage :the-face}))

(defn passage [{:keys [id title]} & content]
  [:p {:id id}
   content])

(defn got-clicked [id]
  (swap! app-state assoc :current-passage (keyword id)))

(defn p-link [{:keys [id]} & content]
  [:a {:href (str "#" id)
       :on-click #(got-clicked id)}
   content])

(defonce passages
  {:the-face
   ["Was this the face that launched a thousand ships," [:br]
    "And " [p-link {:id "homo-fuge"} "burned"] " the topless towers of " [:em "Ilium"] "?"]
   :homo-fuge
   ["I see it plain; here in this place is writ,",
    "Homo, fuge: yet shall not Faustus fly."]})


(defn main []
  (let [k (:current-passage @app-state)]
    (let [p (k passages)]
      [:div
       (apply vector (concat [passage {:id k}] p))
       ])))

(defn start []
  (reagent/render-component [main]
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
