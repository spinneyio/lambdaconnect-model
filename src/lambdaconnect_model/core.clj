(ns lambdaconnect-model.core
  (:require [lambdaconnect-model.schema :as schema]
            [lambdaconnect-model.data-xml :as xml]
            [lambdaconnect-model.spec :as spec]
            [lambdaconnect-model.scoping :as scoping]
            [lambdaconnect-model.scope-single-tag :as single] 
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

(defn get-minim-scoping-sets 
  "Given a validated scoping
   returns map of sets which has tags as key and each set keeps
   tags required for scoping that tag"
  [validated-scoping]
  (single/get-minimum-scoping-sets validated-scoping))

(defn scope-selected-tags
  "Takes a config map, a snapshot, a user object from DB, entities-by-name, parsed (and validated) EDN of rules, map of sets indiacting which tags must be scoped per tag and a desired tag.
   It is advised to calculacte scoping sets once and pass the result.
  A typical invocation looks like this: 
  (scope-single-tag (d/db db/conn) user entities-by-name validated-scope scoping-sets :RARestaurant.ofOwner)))

  Returns a map with db ids, something like:
  {:RAOwner.me #{11122, 1222} :user #{2312312}}
  "
  [config
   snapshot ; db snapshot
   user ; user object
   entities-by-name ; coming from xml model parser
   edn ; the EDN as read from configuration file
   tag-scope ; map of sets contating minimal sets of tag which must be evaulated fo given tag (build with get-minim-scoping-set)
   tag ;tag to be scoped
   ]
  (scoping/scope-selected-tags config snapshot user entities-by-name edn tag-scope tag))

(defn scope
  "Takes a snapshot, a user object from DB, entities-by-name and the parsed EDN of rules.
  A typical invocation looks like this: 
  (scope (d/db db/conn) user entities-by-name pull-scoping-edn)

  Returns a map with db ids, something like:
  {:FIUser.me #{11122, 1222} :user #{2312312}}"
  [config ; {:q datomic/q} 
   snapshot ; db snapshot
   user ; user object
   entities-by-name ; coming from xml model parser
   edn ; the EDN as read from configuration file
   push?]
  (scoping/scope config snapshot user entities-by-name edn push?))

(defn reduce-entities
  "Takes what 'scope' produces and aggregates all the entity types (so :NOUser.me and :NOUser.peer become :NOUser with unified ids)"
  [scoped-entities]
  (scoping/reduce-entities scoped-entities))

(defn json-to-clojure
  "Converts json-based map into the one that conforms to the spec. "
  [json entity]
  (trafo/json-to-clojure json entity))

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