(defproject gambletron "0.2.0"
  :url "https://github.com/pqnelson/gambletron"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clojure-csv/clojure-csv "2.0.2"]
                 [clj-time "0.11.0"]
                 [net.mikera/core.matrix "0.49.0"]
                 [incanter "1.5.6"]
                 [com.taoensso/timbre "4.2.1"] ; profiling purposes
                 [org.clojure/math.combinatorics "0.1.1"]
                 [weka/weka "3.6.2"]
                 
                 ;; database packages
                 [korma "0.4.2"]
                 [org.clojure/java.jdbc "0.4.2"]
                 [org.postgresql/postgresql "9.4.1207"]

                 ;; LOGGING DEPS
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.14"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]]
  :resource-paths ["resources/neuroph-core-2.9.jar"]
  :main ^:skip-aot gambletron.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
