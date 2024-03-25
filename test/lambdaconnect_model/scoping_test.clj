(ns lambdaconnect-model.scoping-test
  (:require [clojure.test :refer [deftest is testing]]
            [lambdaconnect-model.core :as mp]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.walk :refer [postwalk]]
            [clojure.string :as str]))


(def entities-by-name-example (mp/entities-by-name "resources/test/test_model.xml"))

(def scoping-example (mp/read-pull-scoping-edn "resources/test/test_scope.edn" entities-by-name-example))



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
              compare-query (fn [actual expected]
                              (let [syms (repeatedly #(gensym "?G__"))]
                                (is (= (remap-query-syms syms expected)
                                       (remap-query-syms syms actual)))))
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
                                                             [?LAUser-me :LAUser/address ?LALocation-fromUser]
                                                             [?LATicketsSold-byUserEvent :LATicketsSold/location ?LALocation-fromUser]]
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
                                                     [(identity ?user) ?LACnysInfo-system]
                                                     [(!= ?user ?LACnysInfo-system)]]}
              expected-reverse-queries {:LAUser.me '[:find
                                                     ?user
                                                     :in
                                                     $
                                                     [?LAUser-me ...]
                                                     :where
                                                     [?LAUser-me :LAUser/internalUserId ?G__28253]
                                                     [?user :app/uuid ?G__28253]]
                                        :LALocation.fromUser '[:find
                                                               ?user
                                                               :in
                                                               $
                                                               [?LALocation-fromUser ...]
                                                               :where
                                                               [?LAUser-me :LAUser/address ?LALocation-fromUser]
                                                               [?LAUser-me :LAUser/internalUserId ?G__28253]
                                                               [?user :app/uuid ?G__28253]]
                                        :LAGame.organisedByUser '[:find
                                                                  ?user
                                                                  :in
                                                                  $
                                                                  [?LAGame-organisedByUser ...]
                                                                  :where
                                                                  (or-join [?user ?LAGame-organisedByUser]
                                                                           (and [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]
                                                                                [?LAUser-me :LAUser/internalUserId ?G__8937]
                                                                                [?user :app/uuid ?G__8937])
                                                                           (and [?LAGame-organisedByUser :LAGame/inThePast true]
                                                                                [?user :user/username]))]
                                        :LATicketsSold.byUserEvent '[:find
                                                                     ?user
                                                                     :in
                                                                     $
                                                                     [?LATicketsSold-byUserEvent ...]
                                                                     :where
                                                                     [?LATicketsSold-byUserEvent :LATicketsSold/location ?LALocation-fromUser]
                                                                     [?LAUser-me :LAUser/address ?LALocation-fromUser]
                                                                     [?LATicketsSold-byUserEvent :LATicketsSold/game ?LAGame-organisedByUser]
                                                                     (or-join [?user ?LAGame-organisedByUser]
                                                                              (and [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]
                                                                                   [?LAUser-me :LAUser/internalUserId ?G__28253]
                                                                                   [?user :app/uuid ?G__28253])
                                                                              (and [?LAGame-organisedByUser :LAGame/inThePast true]
                                                                                   [?user :user/username]))
                                                                     [?LAUser-me :LAUser/internalUserId ?G__28253]
                                                                     [?user :app/uuid ?G__28253]]
                                        :LATeam.playedInGame '[:find
                                                               ?user
                                                               :in
                                                               $
                                                               [?LATeam-playedInGame ...]
                                                               :where
                                                               [?LATeam-playedInGame :LATeam/playedIn ?LAGame-organisedByUser]
                                                               (or-join [?user ?LAGame-organisedByUser]
                                                                        (and [?LAGame-organisedByUser :LAGame/organiser ?LAUser-me]
                                                                             [?LAUser-me :LAUser/internalUserId ?G__8960]
                                                                             [?user :app/uuid ?G__8960])
                                                                        (and [?LAGame-organisedByUser :LAGame/inThePast true]
                                                                             [?user :user/username]))]
                                        :LASyncInfo.system '[:find ?user :in $ [?LASyncInfo-system ...] :where [?user :user/username]]
                                        :LACnysInfo.system '[:find ?user
                                                             :in $ [?LACnysInfo-system ...]
                                                             :where
                                                             [(identity ?LACnysInfo-system) ?user]
                                                             [(!= ?LACnysInfo-system ?user)]]}]
          (testing "all queries expected present"
            (doseq [[tag _] expected-queries]
              (is (contains? queries tag))))
          (testing "query equality"
            (doseq [[tag query] queries]
              (compare-query query (tag expected-queries))))
          (testing "reverse queries"
            (doseq [[tag query] queries]
              (compare-query (mp/reverse-scoping-query query) (tag expected-reverse-queries))))
         (testing "get selected tags"
           (let [all-subsets (->> (set (keys scoping-example))
                                  (combinatorics/subsets)
                                  (remove empty?)
                                  (map set)
                                  (set))]
             (doseq [subset all-subsets]
               (let [queries (mp/get-scoping-queries entities-by-name-example scoping-example false {:tags subset})]
                 (is (= (set (keys queries)) subset))))))))
