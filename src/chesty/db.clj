(ns chesty.db
  (:require [chesty.db.views :as v]
            [clojure.string :as str]
            [krulak :as kr]))

(defn id [doc-or-id]
  (if (string? doc-or-id)
    doc-or-id
    (:_id doc-or-id (:id doc-or-id))))

(defn user-id [doc]
  (or (:user-id doc) (id (:user doc))))

(defn uri [doc]
  (or (first (:uris doc)) (:uri doc)))

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
