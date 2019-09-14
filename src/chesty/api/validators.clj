(ns chesty.api.validators
  (:use valip.predicates)
  (:require [chesty.db :as db]
            [valip.core :as va :only (validate)]))

(defn set-username-errors [m]
  (va/validate m
               [:username db/username-available?
                "That username is not available. Please pick another."]
               [:username present? "Username can't be blank."]
               [:username #(<= (count %) 40) "Username is too long."]
               [:username (matches #"\A[ \-\w]*\Z")
                "Username can't have punctuation besides spaces and dashes."]))

(defn different-user? [a b]
  (let [a (db/user-id a)
        b (db/user-id b)]
    (and a b (not= a b))))
