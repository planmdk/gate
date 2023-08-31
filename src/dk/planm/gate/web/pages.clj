(ns dk.planm.gate.web.pages
  (:require
   [hiccup.core :as h]
   [hiccup.page :as page]))

(defn base-page
  "Constructs hiccup for base HTML, including header, stylesheets and Javascript"
  [styles children]
  (let [body (if (vector? (first children))
               (into [:body] children)
               (conj [:body] children))]
    (page/html5
     {:mode :html
      :lang "en"
      :hx-boost "true"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport"
              :content "width=device-width, initial-scale=1.0"}]
      [:script {:src "/assets/js/htmx.min.js" :type "application/javascript"}]
      [:script {:src "/assets/js/_hyperscript.min.js" :type "application/javascript"}]
      [:script {:src "/assets/js/sse.js" :type "application/javascript"}]
      (apply page/include-css styles)
      ]
     body)))

(comment
  (h/html (base-page ["/css/foo.css"] [:p "foo"]))
  (h/html (base-page ["/css/foo.css"] []) [:div [:p "1 2 3"]])
)
