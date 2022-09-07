(ns lambdaconnect-model.core
  (:require [lambdaconnect-model.schema :as schema]
            [lambdaconnect-model.data-xml :as xml]
            [lambdaconnect-model.spec :as spec]))

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
  ([entities-by-name] (specs entities-by-name {}))
  ([entities-by-name generators]
   (doall ; we force the computation to perform schema generation as a side effect
    (->> entities-by-name
         (vals)
         (map (partial spec/spec-for-entity generators))))
   (doall ; we force the computation to perform schema generation as a side effect
    (->> entities-by-name
         (vals)
         (map (partial spec/datomic-spec-for-entity generators))))))

(defn spec-for-name
  [n]
  (keyword "lambdaconnect-model.spec.json" (name n)))

(defn datomic-spec-for-name
  [n]
  (keyword "lambdaconnect-model.spec.datomic" (name n)))
