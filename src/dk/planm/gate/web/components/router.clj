(ns dk.planm.gate.web.components.router
  (:require
   [buddy.auth :as buddy]
   [com.brunobonacci.mulog :as u]
   [com.stuartsierra.component :as component]
   [dk.planm.gate.web.middleware :as mw]
   [dk.planm.gate.web.resolvers :as resolvers]
   [reitit.ring :as ring]
   [ring.util.response :as ru]))

(defn sub-keyword
  [base-kw kw]
  (keyword
   (str (clojure.core/namespace base-kw) "." (clojure.core/name base-kw))
   (clojure.core/name kw)))

(comment
  (sub-keyword :foo.bar/bax :moo)
  )

(defn port->route
  [port-routes feature-ns feature-indexes {:port/keys [name type indexes responder visibility]}]
  (let [path (get-in port-routes [feature-ns name])
        method (condp = type
                  :mutation :post
                  :query :get
                  :subscription :get)]
    [path
     {:name (sub-keyword feature-ns name)
      method {:middleware [(mw/wrap-pathom-env [resolvers/base-resolvers
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
                           (vary-meta response merge (meta req))))}}]))

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
