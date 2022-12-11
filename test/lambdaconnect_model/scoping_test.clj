(ns lambdaconnect-model.scoping-test
  (:require [clojure.test :refer [deftest is testing]] 
            [lambdaconnect-model.core :as mp] 
            [clojure.pprint :as pprint :refer [pprint]])
  (:import java.util.regex.Pattern)
  ;(:import java.io.File)
  )
  

(def entities-by-name-example (mp/entities-by-name "resources/test/test_model.xml"))

(def scoping-example (mp/read-pull-scoping-edn "resources/test/test_scope.edn" entities-by-name-example))



(deftest query-geneartor-test
      
        (let [comapre-db-link (fn [actual-links]
                                (let [gensym-regex #"\?G__[0-9]{1,10}"
                                      expected-links [(Pattern/compile (str "\\[\\?user :app/uuid " gensym-regex "\\]"))
                                                      (Pattern/compile (str "\\[\\(= " gensym-regex " " gensym-regex"\\)\\]"))
                                                      (Pattern/compile (str "\\[\\?LAUser\\-me :LAUser/internalUserId " gensym-regex "\\]"))]]
                                  (is (= 3 (count actual-links)) "unexpected number of actual-links was given!")
                                  (doseq [idx (range 3)]
                                    (when (not (boolean (re-matches (get expected-links idx) (str (get actual-links idx)))))(println "EXPECTED: " (get expected-links idx))
                                    (println  "ACTUAL: "(str (get actual-links idx))))
                                    (is (boolean (re-matches (get expected-links idx) (str (get actual-links idx))))))))
              
              compare-query (fn [actual expected]
                              (let [remove-elements (fn [vctr idx element-count] (into (subvec vctr 0 idx) (subvec vctr (min (count vctr) (+ idx element-count)))))
                                    actual-find (subvec actual 0 2)
                                    expected-find (subvec expected 0 2)
                                    actual-in (subvec actual 2 5)
                                    expected-in (subvec expected 2 5)
                                    actual-where (remove-elements (subvec actual 5) 1 3)
                                    expected-where (remove-elements (subvec expected 5) 1 3)
                                    actual-links (subvec actual 6 9) 
                                    ]
                                ;(println "actual-find" actual-find)
                                ;(println "expected-find" expected-find)
                                (is (= actual-find expected-find))
                                ;(println "actual-in" actual-in)
                                ;(println "expected-in" expected-in)
                                (is (= actual-in expected-in))
                                ;(println "actual-where" actual-where)
                                ;(println "expected-where" expected-where)
                                (is (= actual-where expected-where))
                                (testing "links to db"
                                  (comapre-db-link actual-links)
                                  )))
              queries (mp/get-scoping-queries entities-by-name-example scoping-example false)
              expected-queries {:LAUser.me '[:find
                                             ?LAUser-me
                                             :in
                                             $
                                             [?user ...]
                                             :where
                                             [?user :app/uuid ?G__28253]
                                             [(= ?G__28254 ?G__28253)]
                                             [?LAUser-me :LAUser/internalUserId ?G__28254]]
                                :LALocation.fromUser '[:find
                                                       ?LALocation-fromUser
                                                       :in
                                                       $
                                                       [?user ...]
                                                       :where
                                                       [?user :app/uuid ?G__28253]
                                                       [(= ?G__28254 ?G__28253)]
                                                       [?LAUser-me :LAUser/internalUserId ?G__28254]
                                                       [?LAUser-me :LAUser/address ?LALocation-fromUser]]
                                :LAGame.organisedByUser '[:find
                                                          ?LAGame-organisedByUser
                                                          :in
                                                          $
                                                          [?user ...]
                                                          :where
                                                          [?user :app/uuid ?G__28253]
                                                          [(= ?G__28254 ?G__28253)]
                                                          [?LAUser-me :LAUser/internalUserId ?G__28254]
                                                          [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]]
                                :LATicketsSold.byUserEvent '[:find
                                                             ?LATicketsSold-byUserEvent
                                                             :in
                                                             $
                                                             [?user ...]
                                                             :where
                                                             [?user :app/uuid ?G__28253]
                                                             [(= ?G__28254 ?G__28253)]
                                                             [?LAUser-me :LAUser/internalUserId ?G__28254]
                                                             [?LAUser-me :LAUser/address ?LALocation-fromUser]
                                                             [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]
                                                             [?LATicketsSold-byUserEvent :LATicketsSold/game ?LAGame-organisedByUser]
                                                             [?LATicketsSold-byUserEvent :LATicketsSold/location ?LALocation-fromUser]]
                                :LATeam.playedInGame '[:find
                                                       ?LATeam-playedInGame
                                                       :in
                                                       $
                                                       [?user ...]
                                                       :where
                                                       [?user :app/uuid ?G__28253]
                                                       [(= ?G__28254 ?G__28253)]
                                                       [?LAUser-me :LAUser/internalUserId ?G__28254]
                                                       [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]
                                                       [?LATeam-playedInGame :LATeam/playedIn ?LAGame-organisedByUser]]
                                :LASyncInfo.system '[:find ?LASyncInfo-system :in $ [?user ...] :where [?LASyncInfo-system :LASyncInfo/ident__]]}]
          (testing "all queries expected present"
            (doseq [[tag _] expected-queries]
              (is (contains? queries tag))))
          (testing "query equality"
            (doseq [[tag query] [(first queries)]]
              (compare-query query (tag expected-queries))))))