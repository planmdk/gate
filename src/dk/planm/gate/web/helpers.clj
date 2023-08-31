(ns dk.planm.gate.web.helpers
  (:require
   [lambdaisland.uri :as uri]
   [ring.util.response :as ru]))

(defn hx-redirect
  ([url]
   (hx-redirect url {}))
  ([url query-params]
   (let [final-url (-> url
                       uri/uri
                       (uri/assoc-query query-params)
                       str)]
     (update (ru/redirect final-url)
             :headers
             assoc
             "HX-Location" final-url
             "HX-Push-Url" final-url))))

(defn hx-trigger
  [event-name]
  (update (ru/response {})
          :headers
          assoc
          "HX-Trigger" event-name))
