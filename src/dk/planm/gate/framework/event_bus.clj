(ns dk.planm.gate.framework.event-bus
  (:require
   [clojure.core.async :as async]
   [com.stuartsierra.component :as component]
   [com.brunobonacci.mulog :as u]))

(defn publish!
  [event-bus event]
  (let [chan (-> event-bus meta ::chan)]
    (tap> [`publish! chan event])
    (async/put! chan event)))

(defn subscribe!
  [event-bus topic c]
  (if (= topic ::ALL)
    (async/tap ((meta event-bus) ::mult) c)
    (async/sub event-bus topic c)))

(defn unsubscribe!
  [event-bus topic c]
  (if (= topic ::ALL)
    (async/untap ((meta event-bus) ::mult) c)
    (async/unsub event-bus topic c)))

(declare stop-event-bus)

(defn- start-event-bus
  [event-bus]
  (if-not (::started (meta event-bus))
    (do
      (u/log ::start)
      (let [chan (async/chan 200)
            mult (async/mult chan)
            pub-chan (async/chan 200)
            publisher (async/pub pub-chan :dk.planm.gate/event)]
        (async/tap mult pub-chan)
        (with-meta
          publisher
          {`component/stop #'stop-event-bus
           ::started true
           ::chan chan
           ::pub-chan pub-chan
           ::mult mult})))
    event-bus))

(defn- stop-event-bus
  [event-bus]
  (if (::started (meta event-bus))
    (let [{::keys [chan pub-chan]} (meta event-bus)]
      (u/log ::stop)
      (async/close! chan)
      (async/close! pub-chan)
      (with-meta {}
        {`component/start #'start-event-bus
         ::started false}))
    event-bus))

(defn make-event-bus
  []
  (with-meta {}
    {`component/start #'start-event-bus}))
