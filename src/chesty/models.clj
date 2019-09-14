(ns chesty.models
  (:require [chesty.acl :as acl]
            [chesty.api :as api]
            [chesty.db :as db]
            [krulak :as kr]
            [manila-john :as mj]
            [medley.core :as me]
            [valip.core :as va]))

(defn maybe-deref [id]
  (when id @id))

(defn wrap-drop-first-arg [f]
  (fn [_ & args]
    (apply f args)))

(def model-defaults
    {:delete-doc (wrap-drop-first-arg mj/delete-doc)
     :get-doc (wrap-drop-first-arg mj/get-doc)
     :put-doc (wrap-drop-first-arg mj/put-doc)
     :can?
     (fn [model request doc field perm]
       (boolean (acl/get-perm
        model
        field
        (api/get-role model request doc)
        perm)))
     :get-model
     (fn [models doc]
       (first (filter #(= (:type %) (:type doc)) models)))
     :get-role
     (fn [model request & [doc]]
       (or (:chesty/api-role request)
           (if-let [user (-> request :chesty/user maybe-deref)]
             (if (and doc (= (db/id user) (db/user-id doc)))
               :owner
               :user)
             :public)))
     :validate
     (fn [model request old-doc new-doc]
       (when-let [vs (:validators model)]
         (va/validate new-doc vs)))})

(defn raw-model [doc-type & kwargs]
  (-> (apply hash-map :type doc-type kwargs)
      (update :permissions acl/new-model-perms)))

(defn model [doc-type & kwargs]
  (let [m (-> (apply hash-map :type doc-type kwargs)
              (update-in [:public-read] #(conj % :_id :type))
              (kr/rmerge model-defaults)
              api/map->Model)
        perms (->> (select-keys m (keys acl/multi-perm-aliases))
                   (me/map-vals #(map keyword %))
                   (merge (select-keys m (keys acl/model-perm-aliases)))
                   acl/dealias-perms)
        m (me/remove-keys acl/aliases m)]
    (api/map->Model (assoc m :permissions perms))))

(defmacro defmodel [name-sym doc-type & kwargs]
  (let [kwargs (->> (partition 2 kwargs)
                    (map (fn [[k v :as kv]]
                           (if (contains? acl/multi-perm-aliases k)
                             [k (list 'quote v)]
                             kv)))
                    (apply concat))]
    `(def ~name-sym
       (model ~doc-type ~@kwargs))))

(defn site [models & kwargs]
  (apply hash-map :models models kwargs))

(defmacro defsite [name-sym models & kwargs]
  `(def ~name-sym
     (site ~models ~@kwargs)))
