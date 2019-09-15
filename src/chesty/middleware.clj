(ns chesty.middleware
  (:require [chesty.db :as db]
            [krulak :as kr]
            [manila-john :as mj]))

(defn show-stacktrace? [request ex]
  (-> request :chesty/user deref :admin? boolean))

(defn db-url [request]
  (let [site (:chesty/site request)]
    (or (:db site)
        (-> (str "No :db for site " (or (db/id site) "nil"))
            RuntimeException.
            throw))))

(defn wrap-db [f]
  (mj/wrap-with-db f db-url))

(defn site-not-found [request]
  {:status 500
   :body (str "No site found for host " (:server-name request))})

(defn site-with-secret-keys [site]
  (if (seq (:secret-keys site))
    site
    (->> [(kr/secret-key)]
         (assoc site :secret-keys)
         mj/put-doc
         db/with-sites-db)))

(defn wrap-site [f & {:keys [not-found-handler]
                      :or {not-found-handler site-not-found}}]
  (fn [request]
    (let [site (db/site-for-host (:server-name request))
          request (assoc request :chesty/site site)]
      (if (and (not site) not-found-handler)
        (not-found-handler request)
        (f request)))))

(defn wrap-middleware [f]
  (-> f
      wrap-db
      wrap-site))
