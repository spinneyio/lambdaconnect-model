(ns lambdaconnect-model.core-test
  (:require 
   [lambdaconnect-model.core :as mp]
   [lambdaconnect-model.tools :as mpt]
   
   #?@(:clj [[clojure.test :refer [deftest is testing]]
             [clojure.spec.alpha :as s]
             [clojure.data.json :refer [read-str]]
             [clojure.spec.gen.alpha :as gen]]
       :cljs [[cljs.test :refer [deftest is testing]]
              [cljs.spec.alpha :as s]
              [cljs.spec.gen.alpha :as gen]
              [shadow.resource :as rc]
              [clojure.test.check.generators]])))
  
(deftest test-core-data-xml-conversion
  (testing "Reading model file one"
    (let [model (mp/entities-by-name #?(:cljs (rc/inline "./fixtures/test-model-1.xml")
                                        :clj "test/lambdaconnect_model/fixtures/test-model-1.xml"))]
      (is (= (count model) 4))
      (testing ";Json to model converter"
        (mp/specs model {:LAUser/gender #(s/gen #{"M" "F" "U"})
                         :LAUser/email (fn [] (gen/fmap #(str % "@test.com") (gen/string-alphanumeric)))
                         })
        (let [game-model (get model "LAGame")
              json #?(:clj (->> "test/lambdaconnect_model/fixtures/fixtures.json"
                                slurp
                                read-str) 
                      :cljs (->> "./fixtures/fixtures.json" 
                                 rc/inline
                                 (.parse js/JSON)
                                 (js->clj)))
              ent (-> json (get "LAGame") first (mp/json-to-clojure game-model))
              generated-games (gen/sample (s/gen (mp/spec-for-name :LAGame)) 200)]

          (is (s/valid? (mp/spec-for-name :LAGame) ent) (s/explain-str (mp/spec-for-name :LAGame) ent))
          (testing ";Inverse"
            (is (= (mp/clojure-to-json ent game-model) (-> json (get "LAGame") first)))
            (doseq [generated-game generated-games]
              (let [processed-game (-> generated-game
                                       (mp/clojure-to-json game-model)
                                       (mp/json-to-clojure game-model))]
                (is (mpt/compare-objects generated-game processed-game game-model)))))))))


  (testing "Schema from model"
    (let [model #?(:cljs (mp/entities-by-name (rc/inline "./fixtures/test-model-1.xml"))
                   :clj (mp/entities-by-name "test/lambdaconnect_model/fixtures/test-model-1.xml"))
          schema (mp/datomic-schema model)]
      (is (= (+ 37 (count model)) (count schema)))))

  (testing "Specs"
    (mp/specs #?(:cljs (mp/entities-by-name (rc/inline "./fixtures/test-model-1.xml"))
                   :clj (mp/entities-by-name "test/lambdaconnect_model/fixtures/test-model-1.xml"))))
  
  (testing "User info"
    (let [model #?(:cljs (mp/entities-by-name (rc/inline "./fixtures/test-model-1.xml"))
                   :clj (mp/entities-by-name "test/lambdaconnect_model/fixtures/test-model-1.xml"))]
      (is (seq (get-in model ["LAGame" :user-info])))
      (is (seq (get-in model ["LAGame" :attributes "gameDescription"]))))))
