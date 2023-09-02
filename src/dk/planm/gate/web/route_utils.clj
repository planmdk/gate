(ns dk.planm.gate.web.route-utils
  (:require
    [reitit.core :as r]))

(defn sub-keyword
  [base-kw kw]
  (keyword
   (str (clojure.core/namespace base-kw) "." (clojure.core/name base-kw))
   (clojure.core/name kw)))

(defn path-to
  [router route-name path-params]
  (-> router
      (r/match-by-name route-name path-params)
      (r/match->path)))
