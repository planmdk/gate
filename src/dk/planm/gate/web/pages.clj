(ns dk.planm.gate.web.pages
  (:require
   [huff.core :as h]))

(defn ^{:originally-from "hiccup.page"} include-css
  "Include a list of external stylesheet files."
  [& styles]
  (for [style styles]
    [:link {:type "text/css", :href style, :rel "stylesheet"}]))

(defn base-page
  "Constructs hiccup for base HTML, including header, stylesheets and Javascript"
  [styles children]
  (h/page
   [:html
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1.0"}]
     [:script {:src "/assets/js/htmx.min.js" :type "application/javascript"}]
     [:script {:src "/assets/js/_hyperscript.min.js" :type "application/javascript"}]
     [:script {:src "/assets/js/sse.js" :type "application/javascript"}]
     (apply include-css styles)]
    [:body children]]))

(comment
  (base-page
   ["/foo.css" "/bar.css"]
   [:<>
    [:p "foo"]
    [:p "bar"]]))
