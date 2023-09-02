(ns dk.planm.gate.core.components.feature
  (:require
   [clojure.core.async :as async]
   [com.brunobonacci.mulog :as u]
   [com.stuartsierra.component :as component]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [dk.planm.gate.core.components.event-bus :as event-bus]
   [com.wsscode.pathom3.plugin :as p.plugin]
   [dk.planm.gate.framework :as fw]))

;;;
;;; :feature/subscriptions
;;;

(defn subscribe
  [event-bus base-env [topic subscription]]
  (let [c (async/chan 10)
        run-eql (fn [eql]
                  (let [env (-> base-env
                                (pci/register (:subscription/indexes subscription))
                                (p.plugin/register [(fw/mutation-success-publisher-plugin event-bus)
                                                    fw/mutation-error-tap-plugin
                                                    fw/resolver-error-tap-plugin
                                                    fw/mutation-resolve-params-plugin]))]
                    (try
                      (p.eql/process env eql)
                      (catch Exception e
                        (tap> [:subscription-err e env eql])
                        (throw e)))))
        responder (:subscription/responder subscription)]
    (event-bus/subscribe! event-bus topic c)
    (async/go-loop []
      (when-let [event (async/<! c)]
        (async/thread
          (responder run-eql event))
        (recur)))
    [topic
     (assoc subscription ::chan c)]))

(defn unsubscribe
  [event-bus [topic subscription]]
  (let [c (::chan subscription)]
    (event-bus/unsubscribe! event-bus topic c)
    (async/close! c)
    [topic
     (dissoc subscription ::chan)]))

(defn start-subscriptions
  [{:feature/keys [indexes subscriptions]
    :keys [event-bus xtdb]
    :as feature}]
  (if (seq subscriptions)
    (do
      (assert event-bus (format "Feature %s is not passed event-bus in system; cannot add subscriptions!"
                                (:feature/name feature)))
      (let [base-env (pci/register {:node (xtdb)} indexes)
            subscriptions-with-chans (into {}
                                           (map (partial subscribe event-bus base-env))
                                           subscriptions)]
        (assoc feature :feature/subscriptions subscriptions-with-chans)))
    feature))

(defn stop-subscriptions
  [{:feature/keys [subscriptions]
    :keys [event-bus]
    :as feature}]
  (if (seq subscriptions)
    (let [subscriptions-without-chans (into {}
                                            (map (partial unsubscribe event-bus))
                                            subscriptions)]
      (assoc feature :feature/subscriptions subscriptions-without-chans))
    feature))

(declare stop-feature)

(defn- start-feature
  [feature]
  (if-not (::started (meta feature))
    (do
      (u/log :dk.planm.gate.feature/start :name (:feature/name feature))
      (-> feature
          (start-subscriptions)
          (with-meta {`component/stop #'stop-feature
                      ::started true
                      :dk.planm.gate/feature (:feature/name feature)})))
    feature))

(defn- stop-feature
  [feature]
  (if (::started (meta feature))
    (do
      (u/log :dk.planm.gate.feature/stop :name (:feature/name feature))
      (-> feature
          (stop-subscriptions)
          (with-meta {`component/start #'start-feature
                      ::started false
                      :dk.planm.gate/feauter (:feature/name feature)})))
    feature))

(defn make-feature
  [feature]
  (with-meta
    feature
    {`component/start #'start-feature
     :dk.planm.gate/feature (:feature/name feature)}))
