(ns sv.rpc.ring
  (:require [ring.util.response :as resp]
            [ring.middleware.format :as format]))

(defn rpc-handler
  [config]
  (fn [request]
    (let [msg (:params request)
          get-rpc-fn (:get-rpc-fn config)]
      (if-let [f (get-rpc-fn msg)]
        (if (or (fn? f) (and (var? f) (fn? (var-get f))))
          (let [args (:args msg)]
            (try
              (let [result (apply f args)]               
                (resp/response {:result result}))
              (catch Exception e
                (-> (cond-> {:error {:message (.getMessage e)
                                     :fn (:fn msg)
                                     :args args
                                     :class (.getClass e)}
                             :cause :rpc/fn-exception}
                      (instance? clojure.lang.ExceptionInfo e)
                      (assoc :ex-data (ex-data e)))
                    (resp/response)
                    (resp/status 500)))))
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

(defn ring-handler [config]
  (let [handler (rpc-handler config)]
    (-> handler
        (route config)
        (format/wrap-restful-format         
         :formats
         (:formats config [:edn :transit-json])))))
