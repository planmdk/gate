(ns dk.planm.gate.framework.components
  (:require [dk.planm.gate.framework.protocols :as fp]))

(defrecord Element [config body-fn]
  fp/Querying
  (query [_this attrs]
    ((:fw/query-fn config) attrs))

  fp/Watching
  (watch [_this attrs]
    (when-let [watch-fn (:fw/watch-fn config)]
      (watch-fn attrs)))

  #?@(:clj [clojure.lang.IFn
            (invoke [_this attrs] (_this attrs nil))
            (invoke [_this attrs children]
                    (apply body-fn attrs children))]

      :cljs [IFn
             (-invoke [_this attrs] (_this attrs nil))
             (-invoke [_this attrs children]
                      (apply body-fn attrs children))]))



(defn element? [v] (instance? Element v))

(defn element*
  [config body-fn meta]
  (with-meta
    (->Element config body-fn)
    meta))

(defn elem*
  [name args meta body]
  (let [attrs (first args)
        query (:fw/query meta)
        query-fn-fn (gensym (str name "-query-fn"))
        watch (:fw/watch meta)
        watch-fn-fn (gensym (str name "-watch-fn"))
        elem-fn-name (gensym (str name "-elem-fn"))
        query-fn `(fn ~query-fn-fn [~attrs] ~query)
        watch-fn `(fn ~watch-fn-fn [~attrs] ~watch)
        config (cond-> {}
                 watch (assoc :fw/watch-fn watch-fn)
                 query (assoc :fw/query-fn query-fn))]
    `(element* ~config
                 (fn ~elem-fn-name ~args ~@body)
                 '~meta)))

(defmacro elem
  [args meta & body]
  (assert (::fqn meta) "Missing metadata on elem.")
  (elem* "anon" args meta body))

(defmacro defelem
  [name args meta & body]
  `(def ~name
     ~(elem* name args (assoc meta ::fqn (symbol (str *ns*) (str name))) body)))

