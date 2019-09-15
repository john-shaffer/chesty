(ns chesty.db
  (:require [chesty.db.views :as v]
            [clojure.string :as str]
            [krulak :as kr]
            [manila-john :as mj]))

(def ^:dynamic *sites-db*
  (System/getenv "CHESTY_SITES_DB"))

(defmacro with-sites-db [& body]
  `(mj/with-db *sites-db* ~@body))

(defn site-for-host [host]
  (with-sites-db
    (-> {:key (str/lower-case host)
         :limit 1
         :include_docs true}
        v/sites-by-host first :doc)))

(defn id [doc-or-id]
  (if (string? doc-or-id)
    doc-or-id
    (:_id doc-or-id (:id doc-or-id))))

(defn user-id [doc]
  (or (:user-id doc) (id (:user doc))))

(defn path [doc]
  (or (first (:paths doc)) (:path doc)))

(defn doc-for-uri [uri & options]
    (->> (merge {:include_docs true
                 :key (str/lower-case uri)
                 :limit 1}
                options)
         (v/docs-by-uri)
         first
         :doc))

(defn user-for-username [username & [options]]
  (->> (merge {:include_docs true
               :key (str/lower-case username)
               :limit 1}
              options)
       (v/users-by-username)
       first
       :doc))

(defn username-available? [username]
  (let [username-owner (future (user-for-username username))
        uri-owner (->> username
                       kr/to-url-slug
                       (str \/)
                       doc-for-uri
                       future)]
    (not (or @username-owner @uri-owner))))
