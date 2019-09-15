(ns chesty.db.views
  (:require [manila-john :as mj :refer (defviews)]))

(defviews "chesty-sites" :javascript
  (sites-by-host
   "function (doc) {
       if (doc.type == 'chesty/site') {
           for (var i in doc.hosts) {
               emit(doc.hosts[i])
           }
       }
    }"))

(defviews "chesty-uris" :javascript
  (docs-by-uri
   "function (doc) {
        if (doc.uri) emit(doc.uri.toLowerCase())
	    if (doc.uris) {
                for (var i in doc.uris) {
                    emit(doc.uris[i].toLowerCase())
                }
            }
         }"))

(defviews "chesty-users" :javascript
  (users-by-username
   "function (doc) {
        if ('chesty/user' === doc.type && doc.username) {
            emit(doc.username.toLowerCase())
        }
    }"))
