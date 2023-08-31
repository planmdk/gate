(ns dk.planm.gate.web.middleware.oidc
 "Ring middleware implementing the OIDC protocol for confidential
  browser-based applications, i.e. redirects.

  Usage:

  Add `wrap-oidc` middleware between the Ring session and buddy authentication
  middlewares. On your session backend's `:unauthorized-handler`, provide something
  that invokes `redirect-to-oidc-provider`. This will trigger the OIDC flow.

  In the call to `wrap-oidc`, the required functions `:check-user-fn` and
  `:make-session-fn` are responsible for respectively finding users based on the
  JWT and creating the session map. The optional `make-user-fn` allows you to create
  new users in the application based on the retrieved token."
 (:require
  [buddy.core.codecs :as codecs]
  [buddy.core.keys :as keys]
  [buddy.sign.jwt :as jwt]
  [clojure.java.io :as io]
  [clojure.spec.alpha :as s]
  [clojure.string :as str]
  [jsonista.core :as json]
  [lambdaisland.uri :as uri]
  [org.httpkit.client :as client]
  [ring.util.response :as ru]))

(s/def ::client-id string?)
(s/def ::client-secret string?)
(s/def ::issuer string?)
(s/def ::redirect-uri string?)
(s/def ::scope (s/coll-of string?))
(s/def ::prompt #{"none"})

(s/def ::oidc-config
  (s/keys :req-un [::client-id
                   ::client-secret
                   ::issuer
                   ::redirect-uri
                   ::scope]
          :opt-un [::prompt]))

(defn issuer->config-uri
  "Copied from https://github.com/yetanalytics/pedestal-oidc/blob/main/src/lib/com/yetanalytics/pedestal_oidc/discovery.clj
  OIDC discovery https://openid.net/specs/openid-connect-discovery-1_0.html"
  [issuer]
  (str issuer
       (when-not (str/ends-with? issuer "/") "/")
       ".well-known/openid-configuration"))

(defn get-openid-config
  "Copied from https://github.com/yetanalytics/pedestal-oidc/blob/main/src/lib/com/yetanalytics/pedestal_oidc/discovery.clj
  OIDC discovery https://openid.net/specs/openid-connect-discovery-1_0.html"
  [config-uri]
  (try (with-open [rdr (io/reader (io/input-stream config-uri))]
         (json/read-value rdr json/keyword-keys-object-mapper))
       (catch Exception ex
         (throw (ex-info "Could not retrieve openid configuration!"
                         {:type ::get-openid-config-fail
                          :config-uri config-uri}
                         ex)))))

(defn req->updated-redirect-uri
  "Adds the next parameter to the redirect uri. Will take the param from req if present,
  or use the current uri."
  [req {:keys [redirect-uri]}]
  (let [next (get-in req [:params "next"] (:uri req))]
    (-> redirect-uri
        uri/uri
        (uri/assoc-query {:next next})
        str)))

(defn get-openid-token
  [token-uri {:keys [client-id client-secret code state session-state redirect-uri]}]
  (try
    (let [token-result (deref (client/request {:url token-uri
                                               :method :post
                                               :form-params
                                               {:client_id client-id
                                                :client_secret client-secret
                                                :grant_type "authorization_code"
                                                :redirect_uri redirect-uri
                                                :code code
                                                :state state
                                                :session_state session-state}})
                              10000 ::timeout)]
      (if (= token-result ::timeout)
        (throw (ex-info "Fetching OpenID token timed out!"
                        {:type ::get-openid-token-fail
                         :reason ::timeout
                         :token-uri token-uri}))
        (json/read-value (:body token-result) json/keyword-keys-object-mapper)))
    (catch Exception ex
      (throw (ex-info "Could not retrieve OpenID token!"
                      {:type ::get-openid-token-fail
                       :token-uri token-uri}
                      ex)))))


(defn get-openid-keyset
  [keyset-uri]
  (try
    (let [keyset-result (deref (client/request {:url keyset-uri
                                                :method :get}))]
      (into {}
            (map (fn [{:keys [kid] :as pkey}]
                   [kid (keys/jwk->public-key pkey)]))
            (:keys (json/read-value (:body keyset-result) json/keyword-keys-object-mapper))))
    (catch Exception ex
      (throw (ex-info "Could not retrieve OpenID keyset!"
                      {:type ::get-openid-keyset-fail
                       :keyset-uri keyset-uri}
                      ex)))))

(defn redirect-to-oidc-provider
  [req {:keys [issuer scope client-id prompt] :as config}]
  (let [openid-config (-> issuer issuer->config-uri get-openid-config)
        redirect-uri-with-next (req->updated-redirect-uri req config)
        nonce (rand-int 100000)
        state (random-uuid)
        authorization-uri (-> openid-config
                              (get :authorization_endpoint)
                              uri/uri
                              (uri/assoc-query {:state state
                                                :nonce nonce
                                                :prompt prompt
                                                :scope (str/join " " scope)
                                                :redirect_uri redirect-uri-with-next
                                                :response_type "code"
                                                :client_id client-id})
                              str)]
    (-> (ru/redirect authorization-uri)
        (assoc-in [:session :nonce] nonce)
        (assoc-in [:session :state] state))))

(defn- base64-decode
  [to-decode]
  (-> to-decode codecs/str->bytes codecs/b64->bytes))

(defn- decode-token
  [token]
  (try
    (let [[headers payload signature] (str/split token #"\.")]
      (when (and headers payload signature)
        (with-meta
          {:headers   (json/read-value (base64-decode headers))
           :payload   (json/read-value (base64-decode payload))
           :signature signature}
          {::raw-token token})))
    (catch Exception ex
      (throw (ex-info "Could not decode token!"
                      {:type ::decode-token-fail
                       :token token}
                      ex)))))

(defn verify-token
  [decoded-token keyset]
  (try
    (let [kid (get-in decoded-token [:headers "kid"])
          alg (str/lower-case (get-in decoded-token [:headers "alg"]))
          public-key (get keyset kid)]
      (when (keys/public-key? public-key)
        (jwt/unsign
         (-> decoded-token meta ::raw-token)
         public-key
         {:alg (keyword alg)})))
    (catch Exception ex
      (throw (ex-info "Could not verify token!"
                      {:type ::verify-token-fail
                       :decoded-token decoded-token
                       :keyset keyset}
                      ex)))))

(defn- check-token-state
  [decoded-token session]
  (= (get-in decoded-token [:payload :state])
     (get session [:state])))

(defn- check-token-nonce
  [decoded-token session]
  (= (get-in decoded-token [:payload :nonce])
     (get session [:nonce])))

(defn- unix-time-now
  "Get the current UNIX timestamp (i.e. seconds since Jan 1st 1970)"
  []
  (long (/ (System/currentTimeMillis)
           1000)))

(defn handle-oidc-callback
  "Handle the OIDC callback from the OIDC provider. This is a redirect
  to a predefined URI in the app, controlled by this OIDC middleware.

  Takes two parameters: req and options

  * req is an HTTP request map
  * options is a map of
    * client-id, the OIDC client ID
    * client-secret, the OIDC client secret
    * issuer, the OIDC issuer
    * check-user-fn, a function that can fetch the user from a JWT
    * make-user-fn, a function that can create a new user from a JWT (optional)
    * make-session-fn, a function that creates the session map from a user and a JWT

  On success, results in a redirect to the `next` query parameter with a
  new session map.

  On failure, results in an unauthenticated response which clears the session."
  [req {:keys [issuer
               check-user-fn
               make-user-fn
               make-session-fn]
        :as config}]
  (let [openid-config (-> issuer issuer->config-uri get-openid-config)
        token-uri (-> openid-config
                      (get :token_endpoint)
                      uri/uri
                      str)
        callback-code (get-in req [:params "code"])
        callback-state (get-in req [:params "state"])
        callback-session-state (get-in req [:params "session_state"])
        token (get-openid-token token-uri (assoc config
                                                   :code callback-code
                                                   :state callback-state
                                                   :redirect-uri (req->updated-redirect-uri req config)
                                                   :session-state callback-session-state))
        decoded-token (-> token :id_token decode-token)
        keyset-uri (get openid-config :jwks_uri)
        keyset (get-openid-keyset keyset-uri)
        next-uri (get-in req [:params "next"] "/")]
    (when-not (check-token-state decoded-token (:session req))
      (throw (ex-info "Authorization response has been tampered with!"
                      {:type ::callback-csrf-fail
                       :decoded-token decoded-token
                       :session (:session req)})))
    (when-not (check-token-nonce decoded-token (:session req))
      (throw (ex-info "Attempt to replay authorization response!"
                      {:type ::callback-replay-fail
                       :decoded-token decode-token
                       :session (:session req)})))
    (if (verify-token decoded-token keyset)
      (if-let [user (check-user-fn decoded-token)]
        (-> (ru/redirect next-uri)
            (assoc :session (assoc (make-session-fn user decoded-token)
                                   ::issued (unix-time-now))))
        (if make-user-fn
          (-> (ru/redirect next-uri)
              (assoc :session (make-session-fn (make-user-fn decoded-token) decoded-token)))
          (-> (ru/response "Unauthenticated")
              (assoc :session nil)
              (ru/status 401))))
      (-> (ru/response "Unauthenticated")
          (assoc :session nil)
          (ru/status 401)))))

(s/def ::check-user-fn (s/or :fn fn? :var var?))
(s/def ::make-user-fn (s/or :fn fn? :var var?))
(s/def ::make-session-fn (s/or :fn fn? :var var?))

(s/def ::oidc-fns
  (s/keys :req-un [::check-user-fn
                   ::make-session-fn]
          :opt-un [::make-user-fn]))

(s/fdef handle-oidc-callback
  :args (s/cat :req map? :config (s/and ::oidc-config ::oidc-fns))
  :ret map?)

(defmulti handle-oidc-exception :type)

(defmethod handle-oidc-exception :default
  [ex]
  (-> (ru/response (ex-message ex))
      (ru/status 500)))

(defn oidc-session-expired?
  [req]
  (if-let [issued (get-in req [:session ::issued])]
    (< issued (- (unix-time-now) (* 15 60)))
    false))

(defn wrap-oidc
  "Ring OIDC middleware implementing the confidential browser redirect
  flow.

  NOTE: This middleware *must* be somewhere *between* the Ring session
  middleware and the Buddy authentication middleware."
  [{:keys [redirect-uri] :as config}]
  (fn wrap-oidc-handler [handler]
    (fn wrap-oidc-req [req]
      (let [current-uri (uri/uri (:uri req))]
        (if (= (:path current-uri)
               (-> redirect-uri uri/uri :path))
          (try
            (handle-oidc-callback req config)
            (catch Exception ex
              (handle-oidc-exception ex)))
          (if (oidc-session-expired? req)
            (try
              (redirect-to-oidc-provider req (assoc config :prompt "none"))
              (catch Exception ex
                (handle-oidc-exception ex)))
            (handler req)))))))

(s/fdef wrap-oidc
  :args (s/cat :config (s/and ::oidc-config ::oidc-fns))
  :ret fn?)
