(ns chesty.acl
  (:import clojure.lang.IPersistentMap)
  (:require [krulak :as kr]
            [medley.core :as me]))

(def role-inherits
  {:user [:public]
   :owner [:user]
   :admin [:owner]})

(defprotocol ACL
  :extend-via-metadata true
  (get-perm [this field role perm] "Get a permission value.")
  (set-perm [this field role perm value] "Set a permission value."))

(defn inherited-perm [x field role perm]
  (->> role (get role-inherits) (keep #(get-perm x field % perm)) first))

(extend-protocol ACL
  nil
  (get-perm [this field role perm] nil)
  (set-perm [this field role perm value]
    (set-perm {} field role perm value))

  IPersistentMap
  (get-perm [this field role perm]
    (let [v (get-in this [field role perm])]
      (if (nil? v)
        (inherited-perm this field role perm)
        v)))
  (set-perm [this field role perm value]
    (assoc-in this [field role perm] value)))

(defn get-model-perm [x role perm]
  (get-perm x :chesty/model role perm))

(defn set-model-perm [x role perm value]
  (set-perm x :chesty/model role perm value))

(defn has-perm? [model field role perm]
  (let [v (get-in model [field role perm])]
    (boolean
     (if (nil? v)  ; Explicit false short-circuits.
       (some #(has-perm? model field % perm) (role-inherits role))
       v))))

(defn has-perm-on-fields? [model fields role perm]
  (every? #(has-perm? model % role perm) fields))

(def add-perm #(set-perm % %2 %3 %4 true))

(defn has-model-perm? [model role perm]
  (has-perm? model :chesty/model role perm))

(defn can-create? [model role]
  (has-model-perm? model role :create?))

(defn can-delete? [model role]
  (has-model-perm? model role :delete?))

(defn can-list? [model role]
  (has-model-perm? model role :list?))

(def multi-perm-aliases
  (into {}
        (for [role '[public user owner admin]
              perm '[read write]]
          [(keyword (str role \- perm))
           (mapv keyword [role (str perm \?)])])))

(def model-perm-aliases
  (into {}
        (for [role '[public user owner admin]
              perm '[create? delete? list?]]
          [(keyword (str role "-can-" perm))
           (mapv keyword [role perm])])))

(def aliases
  (into (set (keys multi-perm-aliases))
        (keys model-perm-aliases)))

(defn dealias-perms [model]
  (kr/deep-merge
   (reduce
    (fn [m [k [role perm]]]
      (reduce #(add-perm % %2 role perm) m (k model)))
   nil
   multi-perm-aliases)
   (reduce
    (fn [m [k [role perm]]]
      (let [v (k model)]
        (if (nil? v)
          m
          (set-perm m :chesty/model role perm v))))
	  nil
    model-perm-aliases)))

(defn new-model-perms [model]
  (apply dissoc
  (->> (reduce #(update-in % [%2] (partial map keyword))
               model (keys multi-perm-aliases))
      dealias-perms
      (kr/deep-merge model))
  (concat (keys multi-perm-aliases) (keys model-perm-aliases))))

(defn select-has-perm [m model role perm]
  (me/filter-keys #(has-perm? model % role perm) m))
