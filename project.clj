(defproject chesty "0.1.0-SNAPSHOT"
  :description "Chesty is a Clojure web framework that uses sensible defaults to create websites in minutes."
  :url "https://github.com/john-shaffer/chesty"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [krulak "0.6.0-SNAPSHOT"]
                 [manila-john "0.6.0-SNAPSHOT"]
                 [medley "1.2.0"]
                 [valip "0.2.0"]]
  :repl-options {:init-ns chesty})
