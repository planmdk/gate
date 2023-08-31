(ns dk.planm.gate.web.utils
  (:require
   [clojure.zip :as zip]
   [dk.planm.gate.core.elements :as ge]
   [dk.planm.gate.core.protocols :as fp]
   [lambdaisland.uri :as uri]))

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
        compiled-elem (elem (assoc attrs ::ge/data data)
                         children)
        new-node (with-meta
                   compiled-elem
                   {::element elem
                    ::attrs attrs})]
    new-node))

(defn hiccup-zip-child
  [process [tag-or-elem attrs-or-child & children :as child]]
  (if (ge/element? tag-or-elem)
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
  (let [fqn (-> node meta ::element meta ::ge/fqn)]
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
  (let [c1 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [:h1 "hello"]) {})
        c2 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [c1]) {})
        c3 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [c2]) {})
        ]
    (= (compile-hiccup
        (constantly {})
        [c3])
       [[:h1 "hello"] #{}]))

  ;; Test 2 - compile-hiccup handles multiple components nested under a parent
  (let [c1 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [:h1 "hello"]) {})
        c2 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [c1]) {})
        c3 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [c2]) {})
        ]
    (= (compile-hiccup
        (constantly {})
        [:div [c3] [c2]])
         [[:div [:h1 "hello"] [:h1 "hello"]] #{}]))

  ;; Test 3 - compile-hiccup correctly traverses list of siblings under a parent
  (let [c1 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [:h1 "hello"]) {})
        c2 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [c1]) {})
        c3 (ge/element* {::ge/query-fn (fn [_] [])} (fn [_ & _] [c2]) {})
        ]
    (= (compile-hiccup
          (constantly {})
          [:div [[c3 1] [c2 2]]])
       [[:div [:h1 "hello"] [:h1 "hello"]] #{}]))

  ;; Test X - playground
  (let [hic [[:nav
              (with-meta
                (ge/elem [{::ge/keys [data]} & _]
                         {::ge/fqn 'test
                          :query [{[:test/id 1] [:test/name]}]}
                         [:p (get-in data [[:test/id 1] :test/name])])
                {::ge/fqn "da/nav"})
              (with-meta
                (ge/elem [{::ge/keys [data]} & _]
                         {::ge/fqn 'test
                          :query [{[:test/id 1] [:test/name]}]}
                         [:p (get-in data [[:test/id 1] :test/name])])
                {::ge/fqn "da/nav2"})]
             [:main
              [:article
               [:section
                [(with-meta
                   (ge/elem [{::ge/keys [data id]} & _]
                            {::ge/fqn 'test
                             :query [{[:test/id id] [:test/name :test/id]}]
                             :watch [{[:test/id id] [:test/name]}]}
                            [:a {:href (get-in data [[:test/id id] :test/id])}
                             (get-in data [[:test/id id] :test/name])])
                   {::ge/fqn "foobar/baz"})
                 {:id 1}]
                [(with-meta
                   (ge/elem [{::ge/keys [data id]} & _]
                            {::ge/fqn 'test
                             :query [{[:test/id id] [:test/name :test/id]}]
                             :watch [{[:test/id id] [:test/name]}]}
                            [:a {:href (get-in data [[:test/id id] :test/id])}
                             (get-in data [[:test/id id] :test/name])])
                   {::ge/fqn "foobar/baz"})
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
              [(ge/elem [{::ge/keys [data]} & _]
                        {::ge/fqn 'test
                         :query [:test/name]
                         :watch [:test/name]}
                        [:section
                         [:h1 (data :test/name)]
                         [(ge/elem [{::ge/keys [data]} & _]
                                   {::ge/fqn 'test
                                    :query [:test/id :test/name]
                                    :watch [:test/name]}
                                   [:a {:href (data :test/id)} (data :test/name)])]
                         [(ge/elem [{::ge/keys [data]} & _]
                                   {::ge/fqn 'test
                                    :query [:test/id :test/name]
                                    :watch [:test/name]}
                                   [:a {:href (data :test/id)} (data :test/name)])]])]]]]
    (compile-hiccup
     (constantly {:test/id 1 :test/name "John Doe"})
     hic))
  )
