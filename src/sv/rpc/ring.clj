(ns sv.rpc.ring
  (:require [ring.util.response :as resp]
            [ring.middleware.format :as format]))

(defn default-exception-handler [context]
  (let [{:keys [args msg e]} context]
    (-> (cond-> {:error {:message (.getMessage e)
                         :fn (:fn msg)
                         :args args
                         :class (.getClass e)}
                 :cause :rpc/fn-exception}
          (instance? clojure.lang.ExceptionInfo e)
          (assoc :ex-data (ex-data e)))
        (resp/response)
        (resp/status 500))))

(defn rpc-handler
  [config]
  (fn [request]
    (let [msg (:params request)
          get-rpc-fn (:get-rpc-fn config)]
      (if-let [f (get-rpc-fn
                  (assoc
                   msg
                   ;; some rpc functions might need metadata about the
                   ;; call (like the :headers from the
                   ;; :ring/request). Therefore the get-rpc-fn is
                   ;; provided with the metadata here, so that it can
                   ;; pass it to a rpc function if needed:
                   :metadata
                   {:ring/request request}))]
        (if (or (fn? f) (and (var? f) (fn? (var-get f))))
          (let [args (:args msg)]
            (try
              (let [result (apply f args)]               
                (resp/response {:result result}))
              (catch Exception e
                (if-let [exception-handler (:exception-handler config)]
                  (exception-handler
                   {:args args
                    :e e
                    :msg msg})
                  (throw e)))))
          ;; get-rpc-fn can return a ring response map (for signaling
          ;; errors etc.)
          f)
        (-> {:error {:message "procedure not found"
                     :fn (:fn msg)
                     :args (:arg msg)}
             :cause :rpc/procedure-not-found}
            (resp/response)
            (resp/status 500))))))

(defn- route [handler config]
  (fn [request]
    (when (and (= (:request-method request) :post)
               (= (:uri request) (:path config "/rpc")))
      (handler request))))

(def default-config
  {:exception-handler default-exception-handler})

(defn ring-handler [config]
  (let [handler (rpc-handler (merge default-config config))]
    (-> handler
        (route config)
        (format/wrap-restful-format         
         :formats
         (:formats config [:edn :transit-json])))))
