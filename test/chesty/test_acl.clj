(ns chesty.test-acl
  (:use chesty.acl
        clojure.test))

(let [public-read {:public {:read? true}}
      owner-write {:owner {:write? true}}]
  (def post-model
    {:chesty/model
      {:public {:list? true}
       :user {:create? true}
       :owner {:delete? true}}
       :_id public-read
       :title (merge public-read owner-write)
       :content (merge public-read owner-write)}))

(def post1
  {:_id 1
   :title "Hello"
   :content "World"})

(deftest test-has-perm?
  (are [t? f r p] (= t? (has-perm? post-model f r p))
       true :content :public :read?
       true :content :user :read?
       true :title :owner :write?
       false :title :user :write?
       false `x# :owner :read?))

(deftest test-has-perm-on-fields?
  (are [t? fs r p] (= t? (has-perm-on-fields? post-model fs r p))
       true [:title :content] :owner :write?
       false [:title :image] :owner :read?
       false [:title] :public :write?))

(deftest test-set-perm
  (is (has-perm? post-model :title :public :read?))
  (-> (set-perm post-model :title :public :read? nil)
      (has-perm? :title :public :read?)
      not is))

(defmacro test-model-perm [f model & body]
  `(are [t?# role#] (= t?# (~f ~model role#))
        ~@body))

(deftest test-can-create?
  (test-model-perm can-create? post-model
                   true :user
                   false :public))

(deftest test-can-delete?
  (test-model-perm can-delete? post-model
                   true :owner
                   false :user))

(deftest test-can-list?
  (test-model-perm can-list? post-model
                   true :public
                   true :owner))

(deftest test-new-model-perms
  (let [post-model (-> (assoc post-model :public-read '[image] 
                                         :public-can-list? false)
                       new-model-perms)]
    (are [a] a
      (nil? (:public-read post-model))
      (nil? (:public-can-list? post-model))
      (has-perm? post-model :image :public :read?)
      (not (can-list? post-model :public)))))

(deftest test-dealias-perms
  (are [a b] (= a b)
  {:title {:public {:read? true}}}
      (dealias-perms {:public-read [:title]})
  {:chesty/model {:public {:list? true}}}
      (dealias-perms {:public-can-list? true})
  {:title {:public {:read? true}}
   :chesty/model {:public {:list? true}}}
      (dealias-perms {:public-read [:title] :public-can-list? true})))

(deftest test-get-perm
  (is (nil? (get-perm nil :title :public :read?)))
  (are [v f r p] (identical? v (get-perm post-model f r p))
       true :content :public :read?
       true :content :user :read?
       true :title :owner :write?
       nil :title :user :write?
       nil `x# :owner :read?)
  (testing "inherits false"
    (is (false? (get-perm {:title {:public {:write? false}}}
                  :title :public :write?)))))

(deftest test-select-has-perm
  (are [a b c] (= a (select-has-perm post1 post-model b c))
    post1 :public :read?
    {} :public :write?
    (dissoc post1 :_id) :owner :write?))
