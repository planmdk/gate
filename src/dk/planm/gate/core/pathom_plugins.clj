(ns dk.planm.gate.core.pathom-plugins
  (:require
   [com.wsscode.pathom3.connect.built-in.plugins :as pbip]
   [com.wsscode.pathom3.plugin :as p.plugin]
   [dk.planm.gate.core.components.event-bus :as event-bus]))

(def mutation-resolve-params-plugin pbip/mutation-resolve-params)

(def resolver-error-tap-plugin {::p.plugin/id 'resolver-err
                                :com.wsscode.pathom3.connect.runner/wrap-resolver-error
                                (fn [_]
                                  (fn [env node error]
                                    (tap> [:resolver/error (ex-message error)
                                           error])))})

(def mutation-error-tap-plugin {::p.plugin/id 'mutation-err
                                :com.wsscode.pathom3.connect.runner/wrap-mutation-error
                                (fn [_]
                                  (fn [env ast error]
                                    (tap> [:mutation/error (:key ast) (ex-message error) error])))})

(defn mutation-success-publisher-plugin
  "A Pathom plugin that published to event-bus whenever a mutation completes
  successfully.

  The event put on chan contains the keys params and event.

  event is the fqn of the mutation

  params is the return value of the mutation."
  [event-bus]
  {::p.plugin/id `mutation-success-publisher-plugin
   :com.wsscode.pathom3.connect.runner/wrap-mutate
   (fn mutation-success-publisher-plugin-wrapper [mutate]
     (fn [env ast]
       (let [mutation-result (mutate env ast)
             event {:dk.planm.gate/event (:dispatch-key ast)
                    :dk.planm.gate/params (:params ast)
                    :dk.planm.gate/result mutation-result}]
         (event-bus/publish! event-bus event)
         mutation-result)))})
