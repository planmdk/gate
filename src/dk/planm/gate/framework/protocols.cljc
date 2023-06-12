(ns dk.planm.gate.framework.protocols)

(defprotocol Querying
  (query [this args]))

(defprotocol Watching
  (watch [this args]))
