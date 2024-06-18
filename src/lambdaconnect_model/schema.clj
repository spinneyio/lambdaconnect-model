(ns lambdaconnect-model.schema
  (:require [lambdaconnect-model.tools :as t]
            [lambdaconnect-model.utils :as u]))

(defn- ident-schema-from-entity [entity]
  [{:db/ident (t/unique-datomic-identifier entity)
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "A marker datom allowing for entity identification. Automatically generated based on Core Data model file."}])

(defn- attributes-schema-from-entity [entity]
  (->> entity
       :attributes
       vals
       (filter #(not (t/special-attribs (:name  %))))
       (map
        (fn [attribute]
          (u/merge (if (= (:type attribute) :db.type/uuid) {:db/index true} {})
                   {:db/ident (t/datomic-name attribute)
                    :db/valueType (:type attribute)
                    :db/cardinality :db.cardinality/one
                    :db/doc "Core Data based attribute definition. Automatically generated based on Core Data model file."})))))

(defn- relationships-schema-from-entity [entity]
  (->> entity
       :datomic-relationships
       vals
       (map
        (fn [relationship]
          {:db/ident (t/datomic-name relationship)
           :db/valueType :db.type/ref
           :db/index true
           :db/cardinality (if (:to-many relationship) 
                             :db.cardinality/many 
                             :db.cardinality/one)
           :db/doc "Core Data based relationship definition. Automatically generated based on Core Data model file"}))))

(defn- schema-from-entity [entity]
  (concat
   (ident-schema-from-entity entity)
   (attributes-schema-from-entity entity)
   (relationships-schema-from-entity entity)))

(defn schema-from-entities
  "Generates schema for CoreData model"
  [entities]
  (->> entities
       vals
       (u/mapcat schema-from-entity)))
