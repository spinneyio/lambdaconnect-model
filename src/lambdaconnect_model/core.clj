(ns lambdaconnect-model.core
  (:require [lambdaconnect-model.schema :as schema]
            [lambdaconnect-model.data-xml :as xml]
            [lambdaconnect-model.spec :as spec]
            [lambdaconnect-model.scoping :as scoping]
            [lambdaconnect-model.scope-dependency :as single] 
            [lambdaconnect-model.tools :as t]
            [lambdaconnect-model.transformations :as trafo]))

(defn entities-by-name
  "Generates a dictionary keyed with entity names and valued in entity representations for a given core data model file. 
   These include names, attributes and relationships. See core-data-xml module for details on the data model used."
  [model-path]
  (-> model-path (slurp) (xml/entities-by-name)))

(defn datomic-schema
  "Automatically generates schema for the Core Data xml model file 
  the path was provided to."
  [entities-by-name]
  (-> entities-by-name
      schema/schema-from-entities))

(defn specs
  "Define the specs for all the entities. The specs relate to the entities by their unqualified namespaced names, e.g. 
  ':lambdaconnect-model.spec.json/FIUser' for the user in jsons and
  ':lambdaconnect-model.spec.datomic/FIUser' for the user saved to database
  
  The generators table lists custom generators and is optional. For example:
  {:FIUser/gender #(s/gen #{\"U\", \"M\", \"F\"})}"
  ([entities-by-name] 
   (specs entities-by-name {}))
  ([entities-by-name generators]
   (spec/specs-for-entities entities-by-name generators)))

(defn spec-for-name
  [n]
  (keyword "lambdaconnect-model.spec.json" (name n)))

(defn datomic-spec-for-name
  [n]
  (keyword "lambdaconnect-model.spec.datomic" (name n)))

(defn read-pull-scoping-edn
  "Read and validate scoping edn."
  [path entities-by-name]
  (scoping/read-pull-scoping-edn path entities-by-name))

(defn get-minimum-scoping-sets 
  "Given a validated scoping
   returns map of sets which has tags as key and each set keeps
   tags required for scoping that tag"
  [validated-scoping]
  (single/get-minimum-scoping-sets validated-scoping))

(defn get-scoping-queries
  "Takes:
   entities-by-name, 
   parsed (and validated) EDN of scoping rules, 
   push?
   config map, with optional keys:
    - tags, a set of tags which queries are requested (if not provided all tags are taken by default)
    - necessary-tags, a set of tags which are required for scoping selected tags, calculated with tree obtained from scope_dependeny/get-minimum-scoping-sets
      if not provided queries for all tags will be calculated and result will be filtered according to tags param
   
   A typical invocation looks like this: 
   (get-scoping-queries entities-by-name validated-scope push?)))
   (get-scoping-queries entities-by-name validated-scope push? {:tags #{:RAEmployee.ofOwner} :necessary-tags #{:RAEmployee.ofOwner :RAOwner.me}}})))
   Returns a map with tags and queries generated to obtain entities for indicated user:
  {:RAEmployee.ofOwner
  [:find
  ?RAEmployee-ofOwner
  :in
  $
  [?user ...]
  :where
  [?user :app/uuid ?G__33023]
  [(= ?G__33024 ?G__33023)]
  [?RAOwner-me :RAOwner/internalUserId ?G__33024]
  [?RAEmployee-ofOwner :RAEmployee/owner ?RAOwner-me]]}
   ...
  "
  ([entities-by-name scoping-defintion push?]
  (scoping/get-scoping-queries entities-by-name scoping-defintion push?))
  ([entities-by-name scoping-defintion push? tags]
  (scoping/get-scoping-queries entities-by-name scoping-defintion push? tags)))

(defn reverse-scoping-query
  "Input:
   Single scoping query (one value of map returned by get-scoping-queries)
   This query efficiently answers question \"As a user, which entities can I access?\".
   Example input:
   [:find ?RAEmployee-ofOwner
    :in $ [?user ...]
    :where
    [?user :app/uuid ?G__33024]
    [?RAOwner-me :RAOwner/internalUserId ?G__33024]
    [?RAEmployee-ofOwner :RAEmployee/owner ?RAOwner-me]]
   Output:
   Single scoping query.
   This query efficiently answers question \"Which users can access this entity?\".
   Example output:
   [:find ?user
    :in $ [?RAEmployee-ofOwner ...]
    :where
    [?RAEmployee-ofOwner :RAEmployee/owner ?RAOwner-me]
    [?RAOwner-me :RAOwner/internalUserId ?G__33024]
    [?user :app/uuid ?G__33024]]"
  [scoping-rule]
  (scoping/reverse-scoping-query scoping-rule))

(defn scope-selected-tags-with-tree
  "Takes:
   a config map,
   a snapshot, 
   a user object from DB,
   entities-by-name, 
   parsed (and validated) EDN of scoping rules,
   scoping-sets, a map of sets indiacting which tags must be scoped per tag calculated with scope_dependency/get-minimum-scoping-sets
   tags, set of desired tags.
   
   It is advised to calculacte scoping sets once and pass the result.
   A typical invocation looks like this: 
   (scope-selected-tags-with-tree config (d/db db/conn) user entities-by-name validated-scope scoping-sets #{:RARestaurant.ofOwner})))

   Returns a map with db ids, something like:
   {:RAOwner.me #{11122, 1222} :user #{2312312}}
  "
  [config snapshot user entities-by-name scoping-defintion scoping-sets tags]
  (scoping/scope-selected-tags-with-tree config snapshot user entities-by-name scoping-defintion scoping-sets tags))

(defn scope
  "Takes a config map, a snapshot, a user object from DB, entities-by-name and the parsed EDN of rules, push? and optional set of tags to scope.
   If set of tags to scope is not provided all tags are scoped.
   A typical invocation looks like this: 
  (scope config (d/db db/conn) user entities-by-name (clojure.edn/read-string (slurp \"resources/model/pull-scope.edn\")) false)
  (scope config (d/db db/conn) user entities-by-name (clojure.edn/read-string (slurp \"resources/model/pull-scope.edn\")) false #{:NOUuser.me :NOLanguage.mine})

  Returns a map with db ids, something like:
  {:NOUser.me #{11122, 1222} :user #{2312312}}
  " 
  ([config snapshot user entities-by-name scoping-defintion push?]
  (scoping/scope config snapshot user entities-by-name scoping-defintion push? (set (keys scoping-defintion))))
  ([config snapshot user entities-by-name scoping-defintion push? tags]
  (scoping/scope config snapshot user entities-by-name scoping-defintion push? tags)))

(defn reduce-entities
  "Takes what 'scope' produces and aggregates all the entity types (so :NOUser.me and :NOUser.peer become :NOUser with unified ids)"
  [scoped-entities]
  (scoping/reduce-entities scoped-entities))

(defn json-to-clojure
  "Converts json-based map into the one that conforms to the spec. "
  ([json entity]
   (trafo/json-to-clojure
    json
    entity
    t/parser-for-attribute
    t/parser-for-relationship))
  ([json entity parser-for-attribute parser-for-relationship]
   (trafo/json-to-clojure
    json
    entity
    parser-for-attribute
    parser-for-relationship)))

(defn clojure-to-json
  ([obj entity]
   (trafo/clojure-to-json
    obj
    entity
    t/inverse-parser-for-attribute
    t/inverse-parser-for-relationship))
  ([obj entity inverse-parser-for-attribute inverse-parser-for-relationship]
   (trafo/clojure-to-json
    obj
    entity
    inverse-parser-for-attribute
    inverse-parser-for-relationship)))

(defn replace-inverses
  ([obj entity] 
   (trafo/replace-inverses obj entity false))
  ([obj entity untangle-singles]
   (trafo/replace-inverses obj entity untangle-singles)))

(defn datomic-name [o] 
  (t/datomic-name o))

(defn datomic-inverse-name [rel] 
  (t/datomic-inverse-name rel))

(def special-attribs t/special-attribs)
