(ns lambdaconnect-model.graph-generator-test
  (:require [lambdaconnect-model.graph-generator :as gg]
            [lambdaconnect-model.core :as mp]
            [lambdaconnect-model.spec :as spec]
             #?@(:clj [[clojure.test :refer [deftest is testing]]
                       [clojure.spec.alpha :as s]
                       [clojure.data.json :refer [read-str]]
                       [clojure.spec.gen.alpha :as gen]]
                 :cljs [[cljs.test :refer [deftest is testing]]
                        [cljs.spec.alpha :as s]
                        [cljs.spec.gen.alpha :as gen]
                        [shadow.resource :as rc]
                        [clojure.test.check.generators]]))
  #?(:cljs (:require-macros [lambdaconnect-model.macro :refer [bench]])))

(defn time-bench [] #?(:clj (System/nanoTime) :cljs (system-time)))

(defmacro bench [& forms]
`(let [s# (time-bench)
       result# (do ~@forms)
       s2# (time-bench)]
   [result# (double (/ (- s2# s#) 1000000000))]))

(deftest graph-generator 
  (let [ebn (mp/entities-by-name #?(:cljs (rc/inline "./fixtures/test-model-1.xml")
                                    :clj "test/lambdaconnect_model/fixtures/test-model-1.xml"))
        _ (spec/specs-for-entities ebn {:LAUser/email (fn [] (gen/fmap #(str % "@test.com") (gen/string-alphanumeric)))
                                        :LAUser/gender #(s/gen #{"U" "M" "F"})
                                        :LALocation/bottomHash #(s/gen #{"aaa"})
                                        :LALocation/centerHash  #(s/gen #{"aaa"})
                                        :LALocation/topHash  #(s/gen #{"aaa"})
                                        :LALocation/leftHash  #(s/gen #{"aaa"})
                                        :LALocation/rightHash   #(s/gen #{"aaa"})})
       
        [graph time] (bench (gg/generate-entity-graph ebn :vertices 2000 :edges 5000))]
    (println "Graph generation took:" time "seconds")
    (is (first (filter #(seq (get % "address")) (get graph "LAUser"))))))
