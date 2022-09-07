(ns lambdaconnect-model.core
  (:require [lambdaconnect-model.schema :as schema]
            [lambdaconnect-model.data-xml :as xml]
            [lambdaconnect-model.spec :as spec]
            [lambdaconnect-model.scoping :as scoping]))

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