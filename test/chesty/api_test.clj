(ns chesty.api-test
  (:use chesty.api
        clojure.test
        krulak.test)
  (:require [chesty.models :as cm]
            [manila-john :as mj]
            [valip.predicates :as vp]))

(cm/defmodel posts "blog/post"
  :public-read [title content created uris type]
  :owner-read [secret]
  :owner-write [title content uris type]
  :public-can-list? true
  :user-can-create? true
  :owner-can-delete? true
  :validators
  [[:title vp/present? "Title can't be blank."]])

(def unmodeled-doc {:_id "unmodeled" :type "unmodeled"})

(def site (cm/site [posts]))

(def post-docs
  (map (partial zipmap [:_id :type :title :content :created :uris :secret])
       [["post0" "blog/post" "First post" "content 1" "yesterday" ["/first-post"] "classified"]
        ["post1" "blog/post" "Second post" "content 2" "today" ["/second-post"] "classified"]]))

(def docs (conj post-docs unmodeled-doc))

(use-fixtures :each (mj/fixture-with-docs docs))

(def delete-doc* (partial delete-doc site))
(def get-doc* (partial get-doc site))
(def post-doc* (partial post-doc site))
(def put-doc* (partial put-doc site))

(def user-request {:chesty/api-role :user})
(def owner-request {:chesty/api-role :owner})

(deftest test-delete-doc
  (status-is 404 (delete-doc* nil "dckx"))
  (status-is 403 (delete-doc* user-request "post0"))
  (status-is 200 (delete-doc* owner-request "post0"))
  (status-is 404 (get-doc* nil "post0"))
  (status-is 403 (delete-doc* owner-request "unmodeled")))

(deftest test-get-doc
  (status-is 404 (get-doc* nil "dckx"))
  (status-is 403 (get-doc* nil "unmodeled"))
  (status-is 200 (get-doc* nil "post0"))
  (body-is (-> post-docs first (dissoc :secret)) (get-doc* nil "post0"))
  (body-is (first post-docs) (get-doc* owner-request "post0")))

(deftest test-post-doc
  (let [post-f #(post-doc* {:chesty/api-role (or % :public) :body-params %2})]
    (status-is 400 (post-f nil nil))
    (status-is 403 (post-f :user {:type "unmodeled"}))
    (status-is 403 (post-f nil {:title "New post" :type "blog/post"}))
    (testing "Can write only to allowed fields."
      (let [response (post-f :user {:title "X" :type "blog/post" :disallowed true})]
        (status-is 403 response)
        (body-is {:errors {:disallowed ["Role :owner doesn't have permission :write?"]}}
                 response)))
    (let [valid-response (post-f :user {:title "New post" :type "blog/post"})
          invalid-response (post-f :user {:content "X" :type "blog/post"})]
      (status-is 201 valid-response)
      (is (:ok (:body valid-response)))
      (is (:_id (:body valid-response)))
      (status-is 200 (->> valid-response :body :_id (get-doc* nil)))
      (status-is 403 invalid-response)
      (is (= {:title ["Title can't be blank."]} (:errors (:body invalid-response)))))))

(deftest test-put-doc
  (let [put-f #(put-doc* {:chesty/api-role (or % :public) :body-params %2} %3)]
    (status-is 403 (put-f :user {:_id "post0" :title "X"} "post0"))
    (status-is 400 (put-f :owner {:_id "post0"} "post0"))
    (status-is 404 (put-f :user {:title "X"} "dckx"))
    (status-is 403 (put-f :owner unmodeled-doc "unmodeled"))
    (let [valid-response (put-f :owner {:title "X"} "post0")
          invalid-response (put-f :owner {:title ""} "post0")]
      (status-is 200 valid-response)
      (is (:ok (:body valid-response)))
      (is (= "post0" (:_id (:body valid-response))))
      (is (-> (get-doc* nil "post0") :body :title (= "X")))
      (status-is 403 invalid-response)
      (is (= {:title ["Title can't be blank."]} (:errors (:body invalid-response)))))
    (testing "Can write only to allowed fields."
      (let [response (put-f :user {:disallowed true} "post0")]
        (status-is 403 response)
        (body-is {:errors {:disallowed ["Role :user doesn't have permission :write?"]}}
                 response)))))
