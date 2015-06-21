(defproject gambletron "0.1.0"
  :url "https://github.com/pqnelson/gambletron"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [clj-time "0.9.0"]
                 [incanter "1.9.0"]
                 ;; database packages
                 [korma "0.4.2"]
                 [org.clojure/java.jdbc "0.3.7"]
                 [org.xerial/sqlite-jdbc "3.8.10.1"]]
  :main ^:skip-aot gambletron.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
