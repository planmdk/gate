(ns dk.planm.gate.framework.sse
  (:require
   [clojure.core.async :as async]
   [com.brunobonacci.mulog :as u]
   [com.stuartsierra.component :as component]
   [dk.planm.gate.framework.event-bus :as event-bus]
   [org.httpkit.server :as httpkit]
   [ring.util.response :as ru]
   [dk.planm.gate.framework.protocols :as fp]
   [clojure.string :as str]))

(defn ->sse
  [& {:keys [topic event]}]
  (str "event: " topic "\ndata: " event "\n\n"))

(defn add-subscriber
  [httpkit-chan watchers event-id element-fqn]
  (let [watchers' @watchers
        event-id-mult (get-in watchers' [event-id ::mult])]
    (if-let [watcher (get-in watchers' [event-id ::out (symbol element-fqn)])]
     (let [{::keys [chan result-fn]} watcher]
       (async/tap event-id-mult chan)
       (async/go-loop []
         (when-let [_ (async/<! chan)]
           (async/thread
             (when-let [result (result-fn)]
               (try
                 (httpkit/send! httpkit-chan
                                {:body (->sse :topic event-id
                                              :event result)
                                 :headers {"Content-Type" "text/event-stream"}
                                 :status 200}
                                false)
                 (catch Exception e
                   (tap> [::exception e])
                   (httpkit/close httpkit-chan)))))
           (recur)))
       (swap! watchers update-in [event-id ::out element-fqn ::subscribers] (fnil inc 0)))
     (throw (ex-info "Missing watcher for SSE"
                     {:event-id event-id
                      :element-fqn element-fqn})))))

(defn remove-subscriber
  [watchers event-id element-fqn]
  (let [watchers' @watchers]
    (if-let [watcher (get-in watchers' [event-id ::out element-fqn])]
      (let [{::keys [chan]} watcher
            new-subscribers (swap! watchers update-in [event-id ::out element-fqn ::subscribers] (fnil dec 0))]
        (when (< new-subscribers 1)
          (async/close! chan)
          (swap! watchers update-in [event-id ::out] dissoc element-fqn)))
      (throw (ex-info "Missing watcher for SSE unsubscribe"
                      {:event-id event-id
                       :element-fqn element-fqn})))))

(comment
  (str/split "foo.bar/baz##1234" #"##")
  )

(defn- sse-on-open
  [active-streams watchers event-pairs]
  (fn [httpkit-chan]
    (u/log ::client-open
           :event-ids (map second event-pairs))
    (doseq [[element-fqn event-id] event-pairs] ;; TODO: should be the other way around, event-id element-fqn to match how the index is built
      (add-subscriber
       httpkit-chan
       watchers
       event-id
       element-fqn))
    (swap! active-streams conj httpkit-chan)))

(defn- sse-on-close
  [active-streams watchers event-pairs]
  (fn [httpkit-chan status]
    (u/log ::client-close
           :status status
           :event-ids (map second event-pairs))
    (doseq [[element-fqn event-id] event-pairs]
      (remove-subscriber
       watchers
       event-id
       element-fqn))
    (swap! active-streams disj httpkit-chan)))

(defn make-sse-req-handler
  [{::keys [active-streams watchers]}]
  (fn [req]
    (let [param (get-in req [:params "event-pair"] [])
          event-pairs (if (vector? param)
                        (map
                         (fn [s]
                           (str/split s #"##"))
                         param)
                        [(str/split param #"##")])]
      (if (seq event-pairs)
        (-> (httpkit/as-channel
             req
             {:on-open (sse-on-open active-streams watchers event-pairs)
              :on-close (sse-on-close active-streams watchers event-pairs)})
            :body
            (ru/response)
            (ru/content-type "text/event-stream"))
        (ru/bad-request "Missing element-pairs query param")))))

(defn make-watcher-pred
  [watch-query process-fn]
  (let [previous-result (atom nil)]
    (fn watcher-pred [_]
      (let [query-result (process-fn watch-query)]
        (if (not= query-result @previous-result)
          (do
            (reset! previous-result query-result)
            true)
          false)))))

(defn make-watcher-result-fn
  [element attrs process-fn result-compile-fn]
  (fn watcher-result []
    (result-compile-fn [element attrs] process-fn)))

(comment
  (let [watchers {"1" {::in (async/chan)
                       ::pred-fn (make-watcher-pred [] (constantly 1))
                       ::mult nil ;; ^ this chan
                       ::out {"dk.planm.foo/a" {::chan (async/chan 1)
                                                ::result-fn nil
                                                ::subscribers 0}
                              "dk.planm.foo/b" {::chan (async/chan 1)
                                                ::subscribers 2}}}}]))

(defn add-watcher!
  [{::keys [watchers mult]} watch-entry result-compile-fn]
  (let [{:dk.planm.gate.framework/keys [attrs event-id element process]} watch-entry
        element-fqn (get-in (meta element) [:dk.planm.gate.framework.components/fqn])]
    (swap! watchers (fn [watchers']
                      (let [existing-event-in-chan (get-in watchers' [event-id ::in])
                            existing-watch-chan (get-in watchers' [event-id ::out element-fqn ::chan])]
                        (cond-> watchers'
                          (nil? existing-event-in-chan) (update (str event-id)
                                                                (fn [m]
                                                                  (let [pred-fn (make-watcher-pred (fp/watch element attrs) process)
                                                                        chan (async/chan 10 (filter pred-fn) (fn [ex]
                                                                                                               (ex-cause ex)))
                                                                        event-mult (async/mult chan)]
                                                                    (async/tap mult chan)
                                                                    (assoc m
                                                                           ::pred-fn pred-fn
                                                                           ::in chan
                                                                           ::mult event-mult))))
                          (nil? existing-watch-chan) (update-in [(str event-id) ::out element-fqn]
                                                                (fn [m]
                                                                  (let [new-watch-chan (async/chan 10)]
                                                                    (assoc m
                                                                           ::chan new-watch-chan
                                                                           ::subscribers 0
                                                                           ::result-fn (make-watcher-result-fn
                                                                                        element
                                                                                        attrs
                                                                                        process
                                                                                        result-compile-fn)))))))))))

(defn subscribe-sse-chan-to-event-bus
  [{:keys [event-bus] :as sse}]
  (let [sse-chan (async/chan 200)
        sse-mult (async/mult sse-chan)]
    (event-bus/subscribe! event-bus ::event-bus/ALL sse-chan)
    (assoc sse ::chan sse-chan ::mult sse-mult)))

(defn unsubscribe-sse-chan-from-event-bus
  [{:keys [event-bus]
    ::keys [chan]
    :as sse}]
  (event-bus/unsubscribe! event-bus ::event-bus/ALL chan)
  (async/close! chan)
  (dissoc sse ::chan))

(defn add-active-streams-storage
  [sse]
  (assoc sse ::active-streams (atom #{})))

(defn close-active-streams
  [{::keys [active-streams] :as sse}]
  (doseq [ch @active-streams]
    (httpkit/close ch))
  (dissoc sse ::active-streams))

(defn add-sse-ring-handler
  [sse]
  (assoc sse :ring-handler (make-sse-req-handler sse)))

(defn remove-sse-ring-handler
  [sse]
  (dissoc sse :ring-handler))

(defn add-watchers-cache
  [sse]
  (assoc sse ::watchers (atom {})))

(defn remove-watchers-cache
  [sse]
  (dissoc sse ::watchers))

(declare stop-sse)

(defn start-sse
  [sse]
  (if-not (::started (meta sse))
    (do
      (u/log ::start)
      (-> sse
          (subscribe-sse-chan-to-event-bus)
          (add-active-streams-storage)
          (add-watchers-cache)
          (add-sse-ring-handler)
          (with-meta {`component/stop #'stop-sse
                      ::started true})))
    sse))

(defn stop-sse
  [sse]
  (if (::started (meta sse))
    (do
      (u/log ::stop)
      (-> sse
          (remove-sse-ring-handler)
          (remove-watchers-cache)
          (close-active-streams)
          (unsubscribe-sse-chan-from-event-bus)
          (with-meta
            {`component/start #'start-sse
             ::started false})))
    sse))

(defn make-sse
  []
  (with-meta
    {}
    {`component/start #'start-sse}))
