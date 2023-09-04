(ns dk.planm.gate.web.components.router
  (:require
   [buddy.auth :as buddy]
   [com.brunobonacci.mulog :as u]
   [com.stuartsierra.component :as component]
   [dk.planm.gate.web.middleware :as mw]
   [dk.planm.gate.web.resolvers :as resolvers]
   [dk.planm.gate.web.route-utils :as route-utils]
   [reitit.ring :as ring]
   [ring.util.response :as ru]))

(comment
  (route-utils/sub-keyword :foo.bar/bax :moo)
  )

(defn port->route
  [port-routes feature-ns feature-indexes {:port/keys [name type indexes parameters responder visibility]}]
  (let [path (get-in port-routes [feature-ns name])
        method (condp = type
                  :mutation :post
                  :query :get
                  :subscription :get)
        parameters-map (when parameters
                         {:parameters parameters})]
    [path
     {:name (route-utils/sub-keyword feature-ns name)
      method (merge
              {:middleware [(mw/wrap-pathom-env [resolvers/base-resolvers
                                                 feature-indexes
                                                 indexes])
                            (mw/wrap-process)]
               :handler (fn [req]
                          (when (and (not= visibility :public)
                                     (not (buddy/authenticated? req)))
                            (buddy/throw-unauthorized))
                          (let [response (if (= type :subscription)
                                           (responder req)
                                           (responder))]
                            (vary-meta response merge (meta req))))}
              parameters-map)}]))

(defn feature->routes
  [port-routes {:feature/keys [indexes name ports]}]
  (mapv (partial port->route port-routes name indexes) ports))

(defn fw-router
  [routes]
  (ring/router routes))

(comment
  (feature->routes
   {:foo/bar {:create "/foo/bar"}}
   #:feature{:name :foo/bar,
             :ports #{#:port{:name :create,
                             :type :mutation,
                             :responder (constantly :yay),
                             :q []}}})

  (feature->routes
   {:foo/bar {:create "/foo/bar"}}
   #:feature{:name :foo/bar,
             :indexes [],
             :ports #{#:port{:name :create,
                             :type :mutation,
                             :responder (fn [data] :yay),
                             :q []}}})
  )

(declare stop-router)

(defn start-router
  [{:keys [route-table sse-bus]
    :as router}]
  (if-not (::started (meta router))
    (let [features (into #{}
                         (filter (fn [v] (get (meta v) :dk.planm.gate/feature)))
                         (vals router))
          routes (mapcat (partial feature->routes route-table) features)
          default-path (get (meta route-table) ::default-path)
          routes-with-default (-> [["/gate-sse" {:name ::sse
                                                 :get {:handler (:ring-handler sse-bus)}}]
                                   ["/assets/*" (ring/create-resource-handler {:root "public"})]]
                                  (into (when default-path
                                          ["/" {:name ::root
                                                :get {:handler (fn [_]
                                                                 (ru/redirect default-path))}}]))
                                  (into routes))]
      (u/log ::start
             :features/count (count features)
             :routes/count (count routes-with-default))
      (with-meta
        (fw-router routes-with-default)
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
