(ns gabriel.core
  (:require [reagent.core :as reagent]
            [hickory.core :as hickory]))

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

(defn PageLink [{:keys [id state]} & content]
  (do 
  [:a {:href (str "#" (name id)) 
       :on-click #(reset! (state :current-page) (keyword id))}
   content]))

(defn Book [{:keys [pages state]}]
  (let [p (pages @(state :current-page))]
    (into [:div {:class "gabriel-Book"}]
          (update-component-params p :state state))))

(defn Reset [params]
  (let [state (params :state)
        params-except-state (dissoc params :state)]
    (doall (map
     #(reset! (state (key %)) (val %))
     params-except-state))
    nil))

(defn State [params] (do 
  ;;@(get-in params [:state :contract-sealed])))
  @(get-in params [:state (-> :var params keyword)])))

(defn Case [params & more]
  (let [conditions (apply hash-map (drop-last more))]
    (get conditions
         @((params :state) (params :var))
         (last more))))

(defn atomize-vals [m]
  (zipmap (keys m) (map reagent/atom (vals m))))

(def myvars
  {:contract-sealed false})

(def mypages
  {:start
   [[Reset {:contract-sealed 1}]
    [:h3 "Gabriel example project"]
    [:p "Was this the face that launched a thousand ships," [:br]
     "And " [PageLink {:id "homo-fuge"} "burned"] " the topless towers of "
     [:em "Ilium"] "?"]
    [:p
     [State {:var :contract-sealed}]
     [Case {:var :contract-sealed} 1 "meow" false "moo" "mow"]]]
   :homo-fuge
   [[:p [Case {:var :contract-sealed} 1 "meow" false "moo" "mow"]]
    [:p "I see it plain; here in this place is writ," [:br]
     [:em "Homo, fuge"] ": yet shall not Faustus fly."]]})

(def my-current-page (reagent/atom :start))


(defn start []
  (reagent/render-component
   [Book {:pages mypages
          :state (conj (atomize-vals myvars)
                       {:current-page my-current-page})}]
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
