(ns dk.planm.gate.framework
  (:require
   [buddy.auth :as buddy]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.zip :as zip]
   [com.brunobonacci.mulog :as u]
   [com.wsscode.pathom3.connect.built-in.plugins :as pbip]
   [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [com.wsscode.pathom3.plugin :as p.plugin]
   [dk.planm.gate.pages :refer [base-page]]
   [dk.planm.gate.framework.components :as fc]
   [dk.planm.gate.framework.event-bus :as event-bus]
   [dk.planm.gate.framework.protocols :as fp]
   [dk.planm.gate.framework.sse :as sse]
   [dk.planm.gate.framework.utils :as fw-utils]
   [hiccup.core :as h]
   [lambdaisland.uri :as uri]
   [reitit.core :as r]
   reitit.http.interceptors.parameters
   [reitit.ring :as ring]
   reitit.ring.middleware.parameters
   [ring.util.response :as ru])
  (:import
   [java.lang Exception]))

;;; Misc
;;;

(s/def :target.feature/name keyword?)
(s/def :target.port/name keyword?)
(s/def :target/name keyword?)
(s/def :target/params map?)
(s/def :target/description
  (s/keys :req [:target.feature/name :target.port/name :target/name]
          :opt [:target/params]))

(s/def :target/descriptions (s/coll-of :target/description))

(defn with-targets
  [descriptions]
  (pbir/constantly-resolver :target/descriptions descriptions))

(s/fdef with-targets
  :args (s/cat :descriptions :target/descriptions))

(comment
  (with-targets [{:target.feature/name :foobar,
                  :target.port/name :baz,
                  :target/name :snaz}])
  )

(defn sub-keyword
  [base-kw kw]
  (keyword
   (str (clojure.core/namespace base-kw) "." (clojure.core/name base-kw))
   (clojure.core/name kw)))

(comment
  (sub-keyword :foo.bar/bax :moo)
  )

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

(defn optimal-watch-loc?
  [loc]
  (let [watch-path-count (get (meta (zip/node loc)) ::watch-path)
        child-watch-path-counts (into []
                                      (comp
                                       (filter vector?)
                                       (map #(get (meta %) ::watch-path))
                                       (filter (comp not nil?)))
                                      (zip/children loc))]
    (cond
      (nil? watch-path-count) false
      (>= (count child-watch-path-counts) 2) true
      (empty? child-watch-path-counts) true
      (> watch-path-count (first child-watch-path-counts)) true
      :else false)))

(comment

  ;; Test 1 - Only child is not on a watch path
  (let [zipper (zip/vector-zip (with-meta [:main
                                           [:article "foo"]]
                                 {::watch-path 1}))]
    (optimal-watch-loc? zipper))

  ;; Test 2 - Only child is on a watch path and branches
  (let [zipper (zip/vector-zip (with-meta
                                 [:main
                                  (with-meta
                                    [:article
                                     (with-meta
                                       [:h1 [:span "Bar"]]
                                       {::watch-path 1})
                                     (with-meta
                                       [:section
                                        [:p "Foo"]]
                                       {::watch-path 1})]
                                    {::watch-path 2})]
                                 {::watch-path 2}))]
    (optimal-watch-loc? (-> zipper zip/down zip/right)))
  )

(defn compile-element
  [process elem attrs children]
  (let [query (fp/query elem attrs)
        data (process query)
        compiled-elem (elem (assoc attrs :fw/data data)
                         children)
        new-node (with-meta
                   compiled-elem
                   {::element elem
                    ::attrs attrs})]
    new-node))

(defn hiccup-zip-child
  [process [tag-or-elem attrs-or-child & children :as child]]
  (if (fc/element? tag-or-elem)
    (recur
     process
     (compile-element
      process
      tag-or-elem
      (if (map? attrs-or-child)
        attrs-or-child
        {})
      (if (or (nil? attrs-or-child)
              (map? attrs-or-child))
        children
        (into [attrs-or-child] children))))
    child))

(defn hiccup-zip
  [process s]
  (zip/zipper
   vector?
   (fn hiccup-zip-children [node]
     (let [[tag attrs-or-child-or-children & children] node]
       (if (and (sequential? attrs-or-child-or-children)
                (vector? (first attrs-or-child-or-children)))
         (recur (into [tag] attrs-or-child-or-children))
         (let [cs (if (map? attrs-or-child-or-children)
                    (mapv (partial hiccup-zip-child process) children)
                    (mapv (partial hiccup-zip-child process)
                          (into [attrs-or-child-or-children] children)))]
           cs))))
   (fn hiccup-zip-make-node [node children]
     (if (vector? node)
       (let [[tag attrs-or-child & _] node]
         (if (map? attrs-or-child)
           (into [tag attrs-or-child] children)
           (into [tag] children)))
       children))
   (if (every? vector? s)
     (throw (ex-info "Hiccup with multiple roots not supported"
                     {:hiccup s}))
     (hiccup-zip-child process s))))

(comment
  (-> (hiccup-zip nil [:main
                   [:article
                    [:section {:id 1}
                     [:p "Foo"]
                     [:p "Bar"]]]])
      (zip/down)
      (zip/down)
      (zip/down)
      (zip/down)
      (zip/edit (fn [s] (str s " + bar")))
      (zip/up)
      (zip/right)
      (zip/edit (fn [[tag attrs-or-child & children]]
                  (if (map? attrs-or-child)
                    (into [tag (assoc attrs-or-child :hx-ext "sse")] children)
                    (into [tag {:hx-ext "sse"} attrs-or-child] children))))
      #_(zip/root)
      )

  (-> (hiccup-zip nil [:main
                [:article
                 [:section {:id 1}
                  [:p "Foo"]
                  [:p "Bar"]]]])
      (zip/next)
      (zip/next)
      (zip/next)
      (zip/next)
      (zip/up)
      (zip/up)
      )
  )

(defn hiccup-node-add-attribute
  [[tag attrs-or-child & children :as node] k v]
  (if (map? attrs-or-child)
    (update node 1 assoc k v)
    (with-meta
      (into [tag {k v} attrs-or-child] children)
      (meta node))))

(defn add-watch-attributes
  [node event-id]
  (let [fqn (-> node meta ::element meta ::fc/fqn)]
    (-> node
       (hiccup-node-add-attribute :hx-ext "sse")
       (hiccup-node-add-attribute :sse-connect (-> (uri/uri "/gate-sse")
                                                   (uri/assoc-query {:event-pair (str fqn "##" event-id)})))
       (hiccup-node-add-attribute :sse-swap (str event-id))
       (hiccup-node-add-attribute :hx-swap "innerHTML"))))

(defn compile-hiccup
  [process hiccup]
  (let [watchers (atom #{})
        compile-fn (fn [h]
                     (loop [z (hiccup-zip process h)]
                       (if (zip/end? z)
                         (let [output (zip/root z)]
                           output)
                         (let [node (zip/node z)
                               {::keys [attrs element]} (meta node)
                               ;; TODO: Hack to work around the fact that zippers
                               ;; do not reflect changes made in the children fn.
                               new-z (if element
                                       (zip/replace z node)
                                       z)]
                           (if (and element
                                    (fp/watch element attrs))
                             (let [event-id (hash (fp/watch element attrs))]
                               (swap! watchers conj {::process process
                                                     ::attrs attrs
                                                     ::element element
                                                     ::event-id event-id})
                               (recur (-> new-z
                                          (zip/edit add-watch-attributes event-id)
                                          zip/next)))
                             (recur (zip/next new-z)))))))
        compiled-hiccup (cond
                          (every? vector? hiccup) (mapv compile-fn hiccup)
                          :else (compile-fn hiccup))]
    [compiled-hiccup @watchers]))

(comment
  ;; Test 1 - compile-hiccup is transitive
  (let [c1 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [:h1 "hello"]) {})
        c2 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [c1]) {})
        c3 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [c2]) {})
        ]
    (= (compile-hiccup
        (constantly {})
        [c3])
       [[:h1 "hello"] #{}]))

  ;; Test 2 - compile-hiccup handles multiple components nested under a parent
  (let [c1 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [:h1 "hello"]) {})
        c2 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [c1]) {})
        c3 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [c2]) {})
        ]
    (= (compile-hiccup
        (constantly {})
        [:div [c3] [c2]])
         [[:div [:h1 "hello"] [:h1 "hello"]] #{}]))

  ;; Test 3 - compile-hiccup correctly traverses list of siblings under a parent
  (let [c1 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [:h1 "hello"]) {})
        c2 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [c1]) {})
        c3 (fc/element* {:fw/query-fn (fn [_] [])} (fn [_ & _] [c2]) {})
        ]
    (= (compile-hiccup
          (constantly {})
          [:div [[c3 1] [c2 2]]])
       [[:div [:h1 "hello"] [:h1 "hello"]] #{}]))

  ;; Test X - playground
  (let [hic [[:nav
              (with-meta
                (fc/elem [{:keys [fw/data]} & _]
                         {::fc/fqn 'test
                          :fw/query [{[:test/id 1] [:test/name]}]}
                         [:p (get-in data [[:test/id 1] :test/name])])
                {::fc/fqn "da/nav"})
              (with-meta
                (fc/elem [{:keys [fw/data]} & _]
                         {::fc/fqn 'test
                          :fw/query [{[:test/id 1] [:test/name]}]}
                         [:p (get-in data [[:test/id 1] :test/name])])
                {::fc/fqn "da/nav2"})]
             [:main
              [:article
               [:section
                [(with-meta
                   (fc/elem [{:keys [fw/data id]} & _]
                            {::fc/fqn 'test
                             :fw/query [{[:test/id id] [:test/name :test/id]}]
                             :fw/watch [{[:test/id id] [:test/name]}]}
                            [:a {:href (get-in data [[:test/id id] :test/id])}
                             (get-in data [[:test/id id] :test/name])])
                   {::fc/fqn "foobar/baz"})
                 {:id 1}]
                [(with-meta
                   (fc/elem [{:keys [fw/data id]} & _]
                            {::fc/fqn 'test
                             :fw/query [{[:test/id id] [:test/name :test/id]}]
                             :fw/watch [{[:test/id id] [:test/name]}]}
                            [:a {:href (get-in data [[:test/id id] :test/id])}
                             (get-in data [[:test/id id] :test/name])])
                   {::fc/fqn "foobar/baz"})
                 {:id 2}]]]]]]
    (compile-hiccup
        (partial p.eql/process (pci/register [(pbir/static-table-resolver `foo :test/id
                                                                          {1 #:test{:name "John Doe"
                                                                                    :id 1}
                                                                           2 #:test{:name "Jane Dow"
                                                                                    :id 2}})]))
        hic))

  (let [hic [:main
             [:article
              [(fc/elem [{:keys [fw/data]} & _]
                        {::fc/fqn 'test
                         :fw/query [:test/name]
                         :fw/watch [:test/name]}
                        [:section
                         [:h1 (data :test/name)]
                         [(fc/elem [{:keys [fw/data]} & _]
                                   {::fc/fqn 'test
                                    :fw/query [:test/id :test/name]
                                    :fw/watch [:test/name]}
                                   [:a {:href (data :test/id)} (data :test/name)])]
                         [(fc/elem [{:keys [fw/data]} & _]
                                   {::fc/fqn 'test
                                    :fw/query [:test/id :test/name]
                                    :fw/watch [:test/name]}
                                   [:a {:href (data :test/id)} (data :test/name)])]])]]]]
    (compile-hiccup
     (constantly {:test/id 1 :test/name "John Doe"})
     hic))
  )

(defn hx-redirect
  ([url]
   (hx-redirect url {})
   #_(update (ru/redirect url) :headers assoc "HX-Location" url "HX-Push-Url" url))
  ([url query-params]
   (let [final-url (if (seq query-params)
                     (str url "?" (fw-utils/query-string query-params))
                     url)]
     (update (ru/redirect final-url)
             :headers
             assoc
             "HX-Location" final-url
             "HX-Push-Url" final-url))))

(defn hx-trigger
  [event-name]
  (update (ru/response {})
          :headers
          assoc
          "HX-Trigger" event-name))

;;; Resolvers
;;;
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
                            full-target-name (sub-keyword feature-name
                                                          port-name)]
                        {target-name (path-to router
                                              full-target-name
                                              (or params {}))})))
                   descriptions))})

(def base-resolvers [http-req-params http-session session-user param-id port-urls])

(comment
  (p.eql/process (pci/register {:http/req {:params {"id" 2}}} base-resolvers)
                 {}
                 [{:http/params [:http.param/id]}])
  )

;;;
;;; Pathom plugins
;;;

(def mutation-resolve-params-plugin pbip/mutation-resolve-params)

(def resolver-error-tap-plugin {::p.plugin/id 'resolver-err
                                :com.wsscode.pathom3.connect.runner/wrap-resolver-error
                                (fn [_]
                                  (fn [env node error]
                                    (tap> [:resolver/error (ex-message error)
                                           error])))})

(def mutation-error-tap-plugin {::p.plugin/id 'mutation-err
                                :com.wsscode.pathom3.connect.runner/wrap-mutation-error
                                (fn [_]
                                  (fn [env ast error]
                                    (tap> [:mutation/error (:key ast) (ex-message error) error])))})

(defn mutation-success-publisher-plugin
  "A Pathom plugin that published to event-bus whenever a mutation completes
  successfully.

  The event put on chan contains the keys params and event.

  event is the fqn of the mutation

  params is the return value of the mutation."
  [event-bus]
  {::p.plugin/id `mutation-success-publisher-plugin
   :com.wsscode.pathom3.connect.runner/wrap-mutate
   (fn mutation-success-publisher-plugin-wrapper [mutate]
     (fn [env ast]
       (let [mutation-result (mutate env ast)
             event {:dk.planm.gate/event (:dispatch-key ast)
                    :dk.planm.gate/params (:params ast)
                    :dk.planm.gate/result mutation-result}]
         (event-bus/publish! event-bus event)
         mutation-result)))})

;;; Ring middleware
;;;

(defn wrap-db
  [node]
  (fn [handler]
    (fn [req] (handler (vary-meta req assoc ::db-node node)))))

(defn wrap-pathom-plugins
  [plugins]
  (fn [handler]
    (fn [req]
      (handler (vary-meta req update ::pathom-env (fnil p.plugin/register {}) plugins)))))

(defn- add-pathom-env
  "Add extra keys to the Pathom environment as well as indexes."
  [env req indexes]
  (-> env
      (assoc :router (get req ::r/router))
      (assoc :http/req req)
      (assoc :node (get (meta req) ::db-node))
      (pci/register indexes)))

(defn wrap-pathom-env
  [indexes]
  (fn [handler]
    (fn [req]
      (handler (vary-meta req update ::pathom-env (fnil add-pathom-env {}) req indexes)))))

(defn wrap-process
  []
  (fn [handler]
    (fn [req]
      (let [indexes (get (meta req) ::pathom-env)]
        (handler (vary-meta req assoc ::process-fn (partial p.eql/process indexes)))))))

(defn- hh
  "Function version of hiccup.core/html for use with clojure.core/apply."
  [& children]
  (let [res (h/html children)]
    res))

(defn- expand-elements
  [response-body process-fn]
  (if (map? response-body)
    [response-body nil]
    (let [[compiled-body watchers-to-register] (compile-hiccup process-fn response-body)]
      (if (map? compiled-body) ;; Means a handler returned a raw response, so we pass it unchanged
        [compiled-body nil]
        [compiled-body watchers-to-register]))))

(defn- ->html
  [compiled-body]
  (if (vector? (first compiled-body))
    (apply hh compiled-body)
    (h/html compiled-body)))

(defn- children
  [hiccup-node]
  (let [[_ attrs-or-child & children] hiccup-node]
    (if (map? attrs-or-child)
      children
      (into [attrs-or-child] children))))

(defn wrap-htmx
  [styles sse-bus]
  (fn [handler]
   (fn [req]
     (let [response-body (handler req)
           [compiled-body watchers-to-register]
           (expand-elements response-body
                            (-> response-body meta ::process-fn))]
       (when (seq watchers-to-register)
          (doseq [watcher watchers-to-register]
            (sse/add-watcher! sse-bus watcher (fn [elem process-fn]
                                                (let [[h _] (expand-elements elem process-fn)]
                                                  (-> h children ->html))))))
       (cond
         (map? compiled-body)
         compiled-body

         (= (get-in req [:headers "hx-request"]) "true")
         (->html compiled-body)

         :else
         (base-page styles compiled-body))))))

(defn wrap-response
  [handler]
  (fn [req]
    (let [response (handler req)]
      (if (ru/response? response) #_(:status response)
        response
        (-> response (ru/response) (ru/content-type "text/html"))))))

(defn wrap-tracking-events
  "tracks api events with μ/log."
  [handler]
  (fn [req]
    (u/with-context
        {:uri (get req :uri), :request-method (get req :request-method)}
      (u/trace ::http-request
        {:pairs [:request/content-type (get-in req [:headers "content-type"])
                 :request/content-encoding
                 (get-in req [:headers "content-encoding"])],
         :capture (fn [{:keys [status]}] {:response/http-status status})}
        (handler req)))))



;;; Routes
;;;

(defn port->route
  [port-routes feature-ns feature-indexes {:port/keys [name type indexes responder visibility]}]
  (let [path (get-in port-routes [feature-ns name])
        method (condp = type
                  :mutation :post
                  :query :get
                  :subscription :get)]
    [path
     {:name (sub-keyword feature-ns name)
      method {:middleware [(wrap-pathom-env [base-resolvers
                                             feature-indexes
                                             indexes])
                           (wrap-process)]
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
