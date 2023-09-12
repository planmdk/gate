(ns dk.planm.gate.web.middleware
  (:require
   [com.brunobonacci.mulog :as u]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [com.wsscode.pathom3.plugin :as p.plugin]
   [dk.planm.gate.web.components.sse :as sse]
   [dk.planm.gate.web.pages :as pages]
   [dk.planm.gate.web.utils :as utils]
   [huff.core :as h]
   [reitit.core :as r]
   [ring.util.response :as ru]))

(defn wrap-db
  [node]
  (fn [handler]
    (fn [req] (handler (vary-meta req assoc ::db-node node)))))

(defn wrap-pathom-plugins
  [plugins]
  (fn [handler]
    (fn [req]
      (handler (vary-meta req update ::pathom-env (fnil p.plugin/register {}) plugins)))))

(defn- add-pathom-env
  "Add extra keys to the Pathom environment as well as indexes."
  [env req indexes]
  (-> env
      (assoc :router (get req ::r/router))
      (assoc :http/req req)
      (assoc :node (get (meta req) ::db-node))
      (pci/register indexes)))

(defn wrap-pathom-env
  [indexes]
  (fn [handler]
    (fn [req]
      (handler (vary-meta req update ::pathom-env (fnil add-pathom-env {}) req indexes)))))

(defn wrap-process
  []
  (fn [handler]
    (fn [req]
      (let [indexes (get (meta req) ::pathom-env)]
        (handler (vary-meta req assoc ::process-fn (partial p.eql/process indexes)))))))

(defn- hh
  "Function version of hiccup.core/html for use with clojure.core/apply."
  [& children]
  (let [res (h/html children)]
    res))

(defn- expand-elements
  [response-body process-fn]
  (if (map? response-body)
    [response-body nil]
    (let [[compiled-body watchers-to-register] (utils/compile-hiccup process-fn response-body)]
      (if (map? compiled-body) ;; Means a handler returned a raw response, so we pass it unchanged
        [compiled-body nil]
        [compiled-body watchers-to-register]))))

(defn- children
  [hiccup-node]
  (let [[_ attrs-or-child & children] hiccup-node]
    (if (map? attrs-or-child)
      children
      (into [attrs-or-child] children))))

(defn wrap-htmx
  [styles sse-bus]
  (fn [handler]
   (fn [req]
     (let [response-body (handler req)
           [compiled-body watchers-to-register]
           (expand-elements response-body
                            (-> response-body meta ::process-fn))]
       (when (seq watchers-to-register)
          (doseq [watcher watchers-to-register]
            (sse/add-watcher! sse-bus watcher (fn [elem process-fn]
                                                (let [[h _] (expand-elements elem process-fn)]
                                                  (-> h children h/html))))))
       (cond
         (map? compiled-body)
         compiled-body

         (= (get-in req [:headers "hx-request"]) "true")
         (h/html compiled-body)

         :else
         (pages/base-page styles compiled-body))))))

(defn wrap-response
  [handler]
  (fn [req]
    (let [response (handler req)]
      (if (ru/response? response) #_(:status response)
        response
        (-> response (ru/response) (ru/content-type "text/html"))))))

(defn wrap-tracking-events
  "tracks api events with μ/log."
  [handler]
  (fn [req]
    (u/with-context
        {:uri (get req :uri), :request-method (get req :request-method)}
      (u/trace ::http-request
        {:pairs [:request/content-type (get-in req [:headers "content-type"])
                 :request/content-encoding
                 (get-in req [:headers "content-encoding"])],
         :capture (fn [{:keys [status]}] {:response/http-status status})}
        (handler req)))))
