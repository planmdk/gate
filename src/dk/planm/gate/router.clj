(ns dk.planm.gate.router
  (:require [dk.planm.gate.framework :as fw]
            [com.stuartsierra.component :as component]
            [com.brunobonacci.mulog :as u]
            [ring.util.response :as ru]
            [reitit.ring :as ring]))

(declare stop-router)

(defn start-router
  [{:keys [route-table sse-bus]
    :as router}]
  (if-not (::started (meta router))
    (let [features (into #{}
                         (filter (fn [v] (get (meta v) :dk.planm.gate/feature)))
                         (vals router))
          routes (mapcat (partial fw/feature->routes route-table) features)
          routes-with-default (into [["/" {:name ::root
                                           :get {:handler (fn [_]
                                                            (ru/redirect "/dash"))}}]
                                     ["/gate-sse" {:name ::sse
                                                   :get {:handler (:ring-handler sse-bus)}}]
                                     ["/assets/*" (ring/create-resource-handler {:root "public"})]]
                                    routes)]
      (u/log ::start
             :features/count (count features)
             :routes/count (count routes-with-default))
      (with-meta
        (fw/fw-router routes-with-default)
        {`component/stop #'stop-router
         ::started true}))
    router))

(defn stop-router
  [router]
  (if (::started (meta router))
    (do
      (u/log ::stop)
      (with-meta {}
        {`component/start #'start-router
         ::started false}))
    router))

(defn make-router
  []
  (with-meta
    {}
    {`component/start #'start-router}))
