(defproject io.spinney/lambdaconnect-model "1.0.20"
  :description "Model parsing and scoping"
  :url "https://github.com/spinneyio/lambdaconnect-model"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [org.clojure/algo.generic "0.1.3"]
                 [org.clojure/data.json "1.0.0"]
                 [org.clojure/test.check "1.1.1"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [clj-time "0.15.2"]
                 [com.taoensso/tufte "2.6.3"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :none
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
                          :dev           [:project/dev]
             :test          [:project/dev :project/test]
             
             :project/test {:dependencies []}
             :project/dev {:plugins [[com.jakemccrary/lein-test-refresh "0.25.0"]]}})
