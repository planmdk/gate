(ns dk.planm.gate.core.protocols)

(defprotocol Querying
  (query [this args]))

(defprotocol Watching
  (watch [this args]))
