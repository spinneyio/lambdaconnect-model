(ns lambdaconnect-model.scope-dependency-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:cljs [shadow.resource :as rc])
            [lambdaconnect-model.core :as mp]
            [lambdaconnect-model.scope-dependency :as scp-dep]))

(def ebn (mp/entities-by-name #?(:cljs (rc/inline "./fixtures/test-model-2.xml")
                                 :clj "test/lambdaconnect_model/fixtures/test-model-2.xml")))
(mp/specs ebn)
(def scoping-example (mp/read-pull-scoping-edn
                      #?(:cljs (rc/inline "./fixtures/test-scope.edn")
                         :clj "test/lambdaconnect_model/fixtures/test-scope.edn")
                      ebn))

(deftest dependency-tree
  (testing "buidling dependency tree"
    (let [[in out roots] (scp-dep/build-dependency-tree scoping-example)
          expected-in {:LAUser.me #{:user}
                       :LALocation.fromUser #{:LAUser.me}
                       :LAGame.organisedByUser #{:LAUser.me}
                       :LASyncInfo.system #{}
                       :LATicketsSold.byUserEvent #{:LAGame.organisedByUser :LALocation.fromUser}
                       :LATeam.playedInGame #{:LAGame.organisedByUser}
                       :LACnysInfo.system #{}
                       :user #{}}
          expected-out {:LAUser.me #{:LALocation.fromUser :LAGame.organisedByUser}
                        :LALocation.fromUser #{:LATicketsSold.byUserEvent}
                        :LAGame.organisedByUser #{:LATicketsSold.byUserEvent :LATeam.playedInGame}
                        :LATicketsSold.byUserEvent #{}
                        :LATeam.playedInGame #{}
                        :user #{:LAUser.me}
                        :LACnysInfo.system #{}
                        :LASyncInfo.system #{}}
          expected-roots #{:LAUser.me}]
      (is (= in expected-in))
      (is (= out expected-out))
      (is (= roots expected-roots)))))

(deftest minimal-scoping-sets
  (let [scoping-sets (scp-dep/get-minimum-scoping-sets scoping-example)
        expected-sets {:LAUser.me #{:LAUser.me}
                       :LALocation.fromUser #{:LAUser.me :LALocation.fromUser}
                       :LAGame.organisedByUser #{:LAUser.me :LAGame.organisedByUser}
                       :LATicketsSold.byUserEvent #{:LALocation.fromUser :LAGame.organisedByUser :LAUser.me :LATicketsSold.byUserEvent}
                       :LATeam.playedInGame #{:LAGame.organisedByUser :LAUser.me :LATeam.playedInGame}
                       :LASyncInfo.system #{:LASyncInfo.system}
                       :LACnysInfo.system #{:LACnysInfo.system}}]
    (is (= scoping-sets expected-sets))))
