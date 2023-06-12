(ns dk.planm.gate.xtdb
  (:require
   [com.brunobonacci.mulog :as u]
   [xtdb.api :as xt]
   [clojure.java.io :as io]))

(declare make-xtdb)

(declare stop-xtdb)

(defn start-xtdb
  [_]
  (u/log ::start)
  (let [node (xt/start-node (or (io/resource "xtdb-config.edn") {}))]
    (with-meta
      (fn [] node)
      {'com.stuartsierra.component/stop #'stop-xtdb})))

(defn stop-xtdb
  [xtdb]
  (u/log ::stop)
  (.close (xtdb))
  (make-xtdb))

(defn make-xtdb
  []
  (with-meta {}
    {'com.stuartsierra.component/start #'start-xtdb}))
