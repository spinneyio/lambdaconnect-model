(ns lambdaconnect-model.scope-dependency-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [lambdaconnect-model.core :as mp]
            [lambdaconnect-model.scope-dependency :as scp-dep]
            [clojure.data.json :refer [read-str]]
            [clojure.spec.gen.alpha :as gen]
            [clojure.pprint :as pprint]))

(def test-scoping (mp/read-pull-scoping-edn "resources/test/test_scope.edn"
                                                 (mp/entities-by-name "resources/test/test_model.xml")))

(deftest dependency-tree
  (testing "buidling dependency tree"
    (let [[in out roots] (scp-dep/build-dependency-tree test-scoping)
          expected-in {:LAUser.me #{:user}
                       :LALocation.fromUser #{:LAUser.me}
                       :LAGame.organisedByUser #{:LAUser.me}
                       :LASyncInfo.system #{}
                       :LATicketsSold.byUserEvent #{:LAGame.organisedByUser :LALocation.fromUser}
                       :LATeam.playedInGame #{:LAGame.organisedByUser}
                       :user #{}}
          expected-out {:LAUser.me #{:LALocation.fromUser :LAGame.organisedByUser}
                        :LALocation.fromUser #{:LATicketsSold.byUserEvent}
                        :LAGame.organisedByUser #{:LATicketsSold.byUserEvent :LATeam.playedInGame}
                        :LATicketsSold.byUserEvent #{}
                        :LATeam.playedInGame #{}
                        :user #{:LAUser.me}
                        :LASyncInfo.system #{}}
          expected-roots #{:LAUser.me}] 
      (is (= in expected-in))
      (is (= out expected-out))
      (is (= roots expected-roots)))))

(deftest minimal-scoping-sets
  (let [scoping-sets (scp-dep/get-minimum-scoping-sets test-scoping)
        expected-sets {:LAUser.me #{:LAUser.me}
                       :LALocation.fromUser #{:LAUser.me :LALocation.fromUser}
                       :LAGame.organisedByUser #{:LAUser.me :LAGame.organisedByUser}
                       :LATicketsSold.byUserEvent #{:LALocation.fromUser :LAGame.organisedByUser :LAUser.me :LATicketsSold.byUserEvent}
                       :LATeam.playedInGame #{:LAGame.organisedByUser :LAUser.me :LATeam.playedInGame}
                       :LASyncInfo.system #{:LASyncInfo.system}}] 
    (is (= scoping-sets expected-sets))))