(ns lambdaconnect-model.scoping-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:cljs [shadow.resource :as rc])
            [lambdaconnect-model.core :as mp]
            [lambdaconnect-model.scoping :as scope]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk]]
            [clojure.string :as str]))



(def entities-by-name-example (mp/entities-by-name #?(:cljs (rc/inline "./fixtures/test-model-2.xml")
                                                      :clj "test/lambdaconnect_model/fixtures/test-model-2.xml")))
(mp/specs entities-by-name-example)
(def scoping-example (mp/read-pull-scoping-edn #?(:cljs (rc/inline "./fixtures/test-scope.edn")
                                                  :clj "test/lambdaconnect_model/fixtures/test-scope.edn")
 entities-by-name-example))
(def scoping-constant-example (mp/read-pull-scoping-edn #?(:cljs (rc/inline "./fixtures/test-scope-constants.edn")
                                                           :clj "test/lambdaconnect_model/fixtures/test-scope-constants.edn")
                                                         entities-by-name-example))


(deftest constants
  (let [queries (scope/get-scoping-queries 
                 entities-by-name-example 
                 (assoc scoping-constant-example
                        :constants
                        {:wow (delay true)
                         :are-you-there? false
                         :whatsupp? true}) 
                 false)]
    (is (= '[:find ?LALocation-allowed-if 
             :in $ [?user ...] 
             :where [?LALocation-allowed-if :LALocation/ident__]]
           (:LALocation.allowed-if queries)))))

(deftest query-generator-test
  (let [remap-query-syms (fn [syms-seq query]
                           (let [syms-map (atom {})]
                             (postwalk (fn [x]
                                         (if (and (symbol? x)
                                                  (str/starts-with? (name x) "?G__"))
                                           (or (get @syms-map x)
                                               (let [new-sym (nth syms-seq (count @syms-map))]
                                                 (swap! syms-map assoc x new-sym)
                                                 new-sym))
                                           x))
                                       query)))
        compare-query (fn [actual expected tag]
                        (let [syms (repeatedly #(gensym "?G__"))]
                          (is (= (remap-query-syms syms expected)
                                 (remap-query-syms syms actual)) (str "Wrong query: " tag))))
        queries (mp/get-scoping-queries entities-by-name-example scoping-example false)
        expected-queries {:LAUser.me '[:find
                                       ?LAUser-me
                                       :in
                                       $
                                       [?user ...]
                                       :where
                                       [?user :app/uuid ?G__28253]
                                       [?LAUser-me :LAUser/internalUserId ?G__28253]]
                          :LALocation.fromUser '[:find
                                                 ?LALocation-fromUser
                                                 :in
                                                 $
                                                 [?user ...]
                                                 :where
                                                 [?user :app/uuid ?G__28253]
                                                 [?LAUser-me :LAUser/internalUserId ?G__28253]
                                                 [?LAUser-me :LAUser/address ?LALocation-fromUser]]
                          :LAGame.organisedByUser '[:find
                                                    ?LAGame-organisedByUser
                                                    :in
                                                    $
                                                    [?user ...]
                                                    :where
                                                    (or-join [?user ?LAGame-organisedByUser]
                                                             [?LAGame-organisedByUser :LAGame/inThePast true]
                                                             (and [?user :app/uuid ?G__8937]
                                                                  [?LAUser-me :LAUser/internalUserId ?G__8937]
                                                                  [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]))]
                          :LATicketsSold.byUserEvent '[:find
                                                       ?LATicketsSold-byUserEvent
                                                       :in
                                                       $
                                                       [?user ...]
                                                       :where
                                                       [?user :app/uuid ?G__28253]
                                                       [?LAUser-me :LAUser/internalUserId ?G__28253]
                                                       (or-join [?user ?LAGame-organisedByUser]
                                                                [?LAGame-organisedByUser :LAGame/inThePast true]
                                                                (and [?user :app/uuid ?G__28253]
                                                                     [?LAUser-me :LAUser/internalUserId ?G__28253]
                                                                     [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]))
                                                       [?LATicketsSold-byUserEvent :LATicketsSold/game ?LAGame-organisedByUser]
                                                       [?LATicketsSold-byUserEvent :LATicketsSold/location ?LALocation-fromUser]
                                                       [?LAUser-me :LAUser/address ?LALocation-fromUser]]
                          :LATeam.playedInGame '[:find
                                                 ?LATeam-playedInGame
                                                 :in
                                                 $
                                                 [?user ...]
                                                 :where
                                                 (or-join [?user ?LAGame-organisedByUser]
                                                          [?LAGame-organisedByUser :LAGame/inThePast true]
                                                          (and [?user :app/uuid ?G__8960]
                                                               [?LAUser-me :LAUser/internalUserId ?G__8960]
                                                               [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]))
                                                 [?LATeam-playedInGame :LATeam/playedIn ?LAGame-organisedByUser]]
                          :LASyncInfo.system '[:find ?LASyncInfo-system :in $ [?user ...] :where [?LASyncInfo-system :LASyncInfo/ident__]]
                          :LACnysInfo.system '[:find ?LACnysInfo-system
                                               :in $ [?user ...]
                                               :where
                                               [(!= ?user ?LACnysInfo-system)]
                                               [(identity ?user) ?LACnysInfo-system]]}]
    (testing "all queries expected present"
      (doseq [[tag _] expected-queries]
        (is (contains? queries tag))))
    (testing "query equality"
      (doseq [[tag query] queries]
        (compare-query query (tag expected-queries) tag)))
    (testing "get selected tags"
      (let [all-subsets (->> (keys scoping-example)
                             (combinatorics/subsets)
                             (remove empty?)
                             (map set)
                             (set))]
        (doseq [subset all-subsets]
          (let [queries (mp/get-scoping-queries entities-by-name-example scoping-example false {:tags subset})]
            (is (= (set (keys queries)) subset))))))))
