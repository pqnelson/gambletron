(defproject gambletron "0.2.0"
  :url "https://github.com/pqnelson/gambletron"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [clj-time "0.9.0"]
                 [net.mikera/core.matrix "0.36.1"]
                 [incanter "1.9.0"]
                 [com.taoensso/timbre "4.0.2"] ; profiling purposes
                 ;; database packages
                 [korma "0.4.2"]
                 [org.clojure/java.jdbc "0.3.7"]
                 [org.xerial/sqlite-jdbc "3.8.10.1"]]
  :main ^:skip-aot gambletron.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
