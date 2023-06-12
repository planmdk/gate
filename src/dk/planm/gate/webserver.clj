(ns dk.planm.gate.webserver
  (:require
    [org.httpkit.server         :as httpkit]
    [com.brunobonacci.mulog     :as u]
    [com.stuartsierra.component :as component]))

(defrecord Webserver [instance]
  component/Lifecycle
  (start [this]
    (if instance
      this
      (let [port 8080]
        (u/log ::start
               :port port)
        (assoc this :instance (httpkit/run-server (get-in this [:web-app :instance])
                                                  {:port port})))))

  (stop [this]
    (if-not instance
      this
      (do
        (u/log ::stop)
        (instance)
        (assoc this :instance nil)))))

(defn make-server
  []
  (map->Webserver {}))
