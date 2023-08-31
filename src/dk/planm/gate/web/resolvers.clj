(ns dk.planm.gate.web.resolvers
  (:require
   [clojure.set :as set]
   [com.wsscode.pathom3.connect.operation :as pco]
   [dk.planm.gate.web.components.router :as router]
   [reitit.core :as r]))

(pco/defresolver http-req-params
  [{:keys [http/req]} _]
  {:http/params (do (tap> [:req req])
                    (apply merge
                           ((juxt
                             :params
                             :path-params
                             :query-params
                             :form-params)
                            req)))})

(pco/defresolver http-session
  [{:keys [http/req]} _]
  {::pco/output [{:http/session [{:identity [:user/id :user/group :user/username]}]}]}
  {:http/session (do (tap> [:req/session req])
                     (:session req))})

(pco/defresolver session-user
  [{:keys [http/session]}]
  {::pco/output [:user/id
                 :user/group
                 :user/username]}
  (:identity session))

(pco/defresolver param-id
  [{:keys [http/params]}]
  {:http.param/id (let [id (get params :id (get params "id"))]
                    (tap> [:params params :http.param/id id])
                    id)})

(defn path-to
  "Given route-name and a map of params, resolve a path in router
  that matches.

  Throws if route-name is not found in router, or if required params
  are missing (or are nil)."
  [router route-name params]
  (when-not (some #{route-name} (r/route-names router))
    (throw (ex-info "Cannot resolve path to route-name because it is not in router"
                    {:route-name route-name
                     :registered-route-names (r/route-names router)})))
  (let [required-path-params (or (-> router
                                     (r/match-by-name route-name {})
                                     (get :required))
                                 #{})
        params-without-nil-vals (filter (comp not nil? second) params)
        {:keys [path query]} (group-by (fn [[k]]
                                         (if (required-path-params k)
                                           :path
                                           :query))
                                       params-without-nil-vals)
        path-params (into {} path)
        query-params (into {} query)]
    (if-let [missing-params (seq (set/difference required-path-params (into #{} (keys params-without-nil-vals))))]
      (throw (ex-info "Cannot resolve path to route-name due to missing or nil required params"
                      {:route-name route-name
                       :params params
                       :required-params required-path-params
                       :missing-params missing-params}))
      (-> router
          (r/match-by-name route-name path-params)
          (r/match->path query-params)))))

(comment
  (try
    (let [router (get-in user/dev-system [:http-router :router])]
        (path-to router :shopping-list.view/list-items {:foo "bar" :id nil}))
    (catch Exception err
      (ex-data err)))
  )

(pco/defresolver
  port-urls
  [{:keys [router] :as env} input]
  {::pco/input [(pco/? :target/descriptions)]}
  {:target/urls (let [descriptions (get (pco/params env) :target/descriptions
                                        (get input :target/descriptions))]
                  (into
                   {}
                   (map
                    (fn [description]
                      (let [target-name (:target/name description)
                            feature-name (:target.feature/name description)
                            port-name (:target.port/name description)
                            params (:target/params description)
                            full-target-name (router/sub-keyword feature-name
                                                                 port-name)]
                        {target-name (path-to router
                                              full-target-name
                                              (or params {}))})))
                   descriptions))})

(def base-resolvers [http-req-params http-session session-user param-id port-urls])
