(ns dk.planm.gate.web.components.webserver
  (:require
    [org.httpkit.server         :as httpkit]
    [com.brunobonacci.mulog     :as u]
    [com.stuartsierra.component :as component]))

(defrecord Webserver [instance config]
  component/Lifecycle
  (start [this]
    (if instance
      this
      (let [{:keys [port] :or {port 3000}} config]
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
  [config]
  (map->Webserver {:config config}))
