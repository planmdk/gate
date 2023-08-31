(ns dk.planm.gate.web.route-utils
  (:require
    [reitit.core :as r]))

(defn path-to
  [router route-name path-params]
  (-> router
      (r/match-by-name route-name path-params)
      (r/match->path)))
