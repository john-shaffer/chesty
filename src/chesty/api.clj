(ns chesty.api
  (:require [cheshire.core :as json]
            [chesty.acl :as acl]
            [chesty.api.validators :as avs]
            [chesty.db :as db]
            [clojure.string :as str]
            [krulak :as kr]
            [manila-john :as mj]
            [medley.core :as me]
            [valip.core :as va]))

(declare can? get-role)

(def ignored-keys [:_method :__anti-forgery-token "__anti-forgery-token"])

(defn ignore-keys [doc & more]
  (apply dissoc doc (concat ignored-keys more)))

(defn get-user [request]
  {:status 200
   :headers {"Content-Type" "application/json; charset-utf-8"}
   :body (json/generate-string @(:chesty/user request))})

(defn response [status & [body]]
  {:status status, :body body})

(defn error-response [status message]
  (response status {:error message}))

(def ok (response 200 {:ok true}))
(def created (response 201 {:ok true}))
(def bad-request (error-response 400 "bad request"))
(def unauthorized (error-response 401 "unauthorized"))
(def forbidden (error-response 403 "forbidden"))
(def not-found (error-response 404 "not found"))

(defmacro require-auth [auth & body]
  (let [cond-form (if (vector? auth) `if-let `if)]
    `(~cond-form ~auth
      (do ~@body)
      unauthorized)))

(defmacro errors-or [errors & body]
  `(if-let [errors# ~errors]
     (assoc forbidden :body {:errors errors#})
     (do ~@body)))

(defn can-model-fn [perm]
  (kr/rpartial can? :chesty/model perm))

(def can-create? (can-model-fn :create?))
(def can-delete? (can-model-fn :delete?))
(def can-list? (can-model-fn :list?))

(defn can-on-fields? [model request doc fields perm]
  (every? #(can? model request doc % perm) fields))

(def can-write-fields? (kr/rpartial can-on-fields? :write?))

(defn select-can [model request doc perm]
  (me/filter-keys #(can? model request doc % perm) doc))

(defn validate-can [site request doc field perm]
  (when-not (can? site request doc field perm)
    {field [(str "Role " (get-role site request doc)
                 " doesn't have permission " perm)]}))

(defn validate-can-multi [site request doc fields perm]
  (reduce merge
          (map #(validate-can site request doc % perm) fields)))

(defrecord Model [type permissions])

(def model-acl-fns
  {:get-perm
   (fn [this field role perm]
     (acl/get-perm (:permissions this) field role perm))
   :set-perm
   (fn [this field role perm value]
     (update this :permissions
            #(acl/set-perm % field role perm value)))})

(extend Model acl/ACL model-acl-fns)

(defprotocol Site
  :extend-via-metadata true
  (db-delete [this doc] "Delete the doc.")
  (db-get [this id] "Retrieve a doc from the db, or nil")
  (db-put [this doc] "Save the doc.")
  (get-model [this request doc]
    "Return a model to govern the doc, or nil. The model can be anything that
     satisfies chesty.acl.ACL.")
  (get-role [this request doc] "Return the ACL role for the request.")
  (can? [this request doc field permission]
    "Returns whether the Ring request has the given permission on field."))

(def default-site-fns
  {:db-delete
   (fn [_ doc]
     (mj/delete-doc doc))
   :db-get
   (fn [_ id]
     (mj/get-doc id))
   :db-put
   (fn [_ doc]
     (mj/put-doc doc))
   :get-model
   (fn [this _ doc]
     (let [t (:type doc)]
       (first (filter #(= t (:type %)) (:models this)))))
   :get-role
   (fn [model request & [doc]]
     (or (:chesty/api-role request)
         (if-let [user (-> request :chesty/user kr/maybe-deref)]
           (if (and doc (= (db/id user) (db/user-id doc)))
             :owner
             :user)
           :public)))
   :can?
   (fn [this request doc field perm]
     (boolean
      (when-let [model (get-model this request doc)]
        (acl/get-perm model field (get-role this request doc) perm))))})

(extend java.util.Map Site default-site-fns)

(defprotocol ValidateDoc
  :extend-via-metadata true
  (validate [this request old-doc new-doc]
    "Return a map of fields to seqs of error messges, or nil if everything is valid.
     old-doc will be nil when there is no older version."))

(def model-validate-doc-fns
  {:validate
   (fn [this _ _ new-doc]
     (when-let [vs (:validators this)]
       (apply va/validate new-doc vs)))})

(extend Model ValidateDoc model-validate-doc-fns)

(def site-validate-doc-fns
  {:validate
   (fn [this request old-doc new-doc]
     (validate (get-model this request (or old-doc new-doc))
               request old-doc new-doc))})

(extend java.util.Map ValidateDoc site-validate-doc-fns)

(defprotocol DocHooks
  :extend-via-metadata true
  (after-load [this request doc]
    "Called after a doc is retrieved from the db. Returns the doc, possibly with changes.")
  (before-save [this request old-doc new-doc]
    "Called before a doc is saved. Returns the doc, possibly with changes.")
  (after-save [this request old-doc new-doc]
    "Called after a doc is saved. Returns the doc, possibly with changes."))

(def model-doc-hooks-fns
  {:after-load (fn [_ _ doc] doc)
   :before-save (fn [_ _ _ new-doc] new-doc)
   :after-save (fn [_ _ _ new-doc] new-doc)})

(extend Model DocHooks model-doc-hooks-fns)

(def site-doc-hooks-fns
  {:after-load
   (fn [this request doc]
     (after-load (get-model this request doc) request doc))
   :before-save
   (fn [this request old-doc new-doc]
     (before-save (get-model this request new-doc) request old-doc new-doc))
   :after-save
   (fn [this request old-doc new-doc]
     (after-save (get-model this request new-doc) request old-doc new-doc))})

(extend java.util.Map DocHooks site-doc-hooks-fns)

(extend-protocol DocHooks
  nil
  (after-load [this request doc]
    doc)
  (before-save [this request old-doc new-doc]
    new-doc)
  (after-save [this request old-doc new-doc]
    new-doc))

(defn load-doc [site request id]
  (when-let [doc (db-get site id)]
    (after-load site request doc)))

(defn save-doc [site request old-doc new-doc]
  (->> (before-save site request old-doc new-doc) (db-put site)
       (after-save site request old-doc)))

(defn default-get-role [model request & [doc]]
  (or (:chesty/api-role request)
      (if-let [user @(:chesty/user request)]
        (if (and doc (= (db/id user) (db/user-id doc)))
          :owner
          :user)
        :public)))

(defn delete-doc [site request id]
  (if-let [doc (db-get site id)]
    (errors-or (validate-can site request doc :chesty/model :delete?)
               (do (db-delete site doc) ok))
    not-found))

(defn get-doc [site request id]
  (if-let [doc (load-doc site request id)]
    (if (get-model site request doc)
      (assoc ok :body (select-can site request doc :read?))
      forbidden)
    not-found))

(defn post-doc [site request]
  (let [doc (-> request :body-params ignore-keys)
        owner-request (assoc request :chesty/api-role :owner)]
    (if doc
      (errors-or
       (or (validate-can site request doc :chesty/model :create?)
           (validate-can-multi site owner-request doc (keys doc) :write?)
           (validate site owner-request nil doc))
       (->> (save-doc site owner-request nil doc) :_id (assoc-in created [:body :_id])))
      bad-request)))

(defn put-doc [site request id]
  (let [doc (db-get site id)
        new-vals (-> request :body-params (ignore-keys :_id))
        new-doc (merge doc new-vals)]
    (cond
     (empty? new-vals) bad-request
     (not doc) not-found
     :else (errors-or
            (or (validate-can-multi site request doc (keys new-vals) :write?)
                (validate site request doc new-doc))
            (->> (save-doc site request doc new-doc) :_id (assoc-in ok [:body :_id]))))))

(defn set-username [request]
  (let [{username :username} (:params request)
        username (-> username (str/replace #"\s+" " ") str/trim)
        errors (-> {:username username}
                   avs/set-username-errors
                   future)
        user @(:chesty/user request)]
    (require-auth user
     (if (:username user)
       (error-response {:username ["You already have a username."]})
       (errors-or @errors
        (mj/put-doc (assoc user :username username 
                           :uris [(str \/ (kr/to-url-slug username))]))
        (json/generate-string {:ok true}))))))
