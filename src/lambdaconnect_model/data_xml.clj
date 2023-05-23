(ns lambdaconnect-model.data-xml
  (:require [clojure.xml]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [lambdaconnect-model.tools :as t]))

(defn regex? [r] (instance? java.util.regex.Pattern r))

(s/def :types/regex
  (s/with-gen regex?
    (fn [] (gen/fmap re-pattern (s/gen string?)))))

(def types-map {"String"     :db.type/string
                "Date"       :db.type/instant
                "Boolean"    :db.type/boolean
                "Integer 32" :db.type/long
                "Integer 64" :db.type/long
                "Integer 16" :db.type/long
                "Float"      :db.type/float
                "Double"     :db.type/double
                "Binary"     :db.type/bytes
                "UUID"       :db.type/uuid
                "URI"        :db.type/string})

(def basic-validators {:db.type/string string?
                       :db.type/instant inst?
                       :db.type/boolean boolean?
                       :db.type/long int?
                       :db.type/uuid uuid?
                       :db.type/double double?
                       :db.type/float float?
                       :db.type/bytes bytes?})

(s/def ::name string?)
(s/def ::entity-name string?)
(s/def ::type (set (vals types-map)))

(s/def ::default-value (s/nilable
                        (apply (t/functionise s/or)
                               (t/mapcat identity basic-validators))))
(s/def ::regular-expression (s/nilable :types/regex))
(s/def ::max-value (s/nilable (s/or :db.type/long int? :db.type/instant inst?)))
(s/def ::min-value (s/nilable (s/or :db.type/long int? :db.type/instant inst?)))
(s/def ::optional boolean?)
(s/def ::indexed boolean?)
(s/def ::user-info (s/nilable (s/map-of string? string?)))

(s/def ::attribute (s/keys
                    :req-un
                    [::name
                     ::type
                     ::optional
                     ::indexed
                     ::entity-name
                     ::user-info]
                    :opt-un
                    [::default-value
                     ::regular-expression
                     ::max-value
                     ::min-value]))

(defrecord Attribute [name
                      entity-name
                      type
                      optional
                      indexed
                      default-value
                      regular-expression
                      max-value
                      min-value
                      user-info])

(s/def ::to-many boolean?)
(s/def ::destination-entity string?)
(s/def ::inverse-name string?)
(s/def ::inverse-entity string?)
(s/def ::max-count (s/nilable int?))

(s/def ::relationship (s/keys
                       :req-un
                       [::name
                        ::entity-name
                        ::optional
                        ::to-many
                        ::destination-entity
                        ::inverse-name
                        ::inverse-entity
                        ::user-info]
                       :opt-un [::max-count]))

(defrecord Relationship [name
                         entity-name
                         optional
                         to-many
                         destination-entity
                         inverse-name
                         inverse-entity
                         max-count
                         user-info])

(s/def ::attributes (s/map-of string? ::attribute))
(s/def ::relationships (s/map-of string? ::relationship))
(s/def ::datomic-relationships (s/nilable (s/map-of string? ::relationship)))

(s/def ::entity (s/keys :req-un [::name
                                 ::attributes
                                 ::relationships
                                 ::user-info]
                        :opt-un [::datomic-relationships]))

; Datomic-relationships is a filtered version of relationships, holding the relationship object.
; It arbitrarily (but stably) defines, for every pair ( (entity1, relationship), (entity2, inverse) )  which relationship should be modeled
; in the db as modelling it both ways is inefficient, prone to errors and against the spirit of datomic.
; The selection logic is encoded in tools/relevant-relationship-from-pair

(defrecord Entity [name
                   attributes
                   relationships
                   datomic-relationships
                   user-info])

; ------------ PARSING ------------

(defn ->bool  [val] (= val "YES"))
(defn ->date
  "Apple uses seconds since 1.1.2001, we have to add the epoch timestamp of this date"
  [val]
  (when val (java.util.Date. (* 1000 (+ 978307200 (Integer. val))))))
(defn ->type  [val] (get types-map val))
(defn ->regex [val] (when val (re-pattern val)))
(defn ->dbl   [val] (when val (Double. val)))
(defn ->float [val] (when val (Float. val)))
(defn ->int   [val] (when val (try (Integer. val) (catch Throwable _ (Math/round (->float val))))))

(defn ->value [val type]
  (when val
    (case type
      :db.type/string  val
      :db.type/boolean (->bool val)
      :db.type/long    (->int val)
      :db.type/double  (->dbl val)
      :db.type/instant (->date val)
      :db.type/float   (->float val))))

(defn ->sync-revision [entity-name]
  (->Attribute
   "syncRevision"
   entity-name
   :db.type/long
   true
   false
   nil
   nil
   nil
   nil
   nil))


(defn attribute-from-xml [entity-name xml user-info]
  (let [type (->type (:attributeType xml))]
    (->Attribute
     (:name xml)
     entity-name
     type
     (->bool (:optional xml))
     (->bool (:indexed xml))
     (->value (or (:defaultValueString xml) (:defaultDateTimeInterval xml)) type)
     (->regex (:regularExpressionString xml))
     (or (->int (:maxValueString xml)) (->date (:maxDateTimeInterval xml)))
     (or (->int (:minValueString xml)) (->date (:minDateTimeInterval xml)))
     user-info)))

(s/fdef attribute-from-xml :ret ::attribute)

(defn relationship-from-xml [entity-name xml user-info]
  (->Relationship
   (:name xml)
   entity-name
   (->bool (:optional xml))
   (->bool (:toMany xml))
   (:destinationEntity xml)
   (:inverseName xml)
   (:inverseEntity xml)
   (->int (:maxCount xml))
   user-info))

(s/fdef relationship-from-xml :ret ::relationship)

(defn user-info-from-xml [xml]
  (->> xml
       (filter #(= :userInfo (:tag %)))
       (mapcat #(->> % :content (map (comp (juxt :key :value) :attrs))))
       (into {})))

(s/fdef user-info-from-xml :ret ::user-info)

(defn entity-from-xml
  "Parses an entity from its pre-parsed xml tree"
  [xml]
  (assert (= :entity (:tag xml)))
  (let [name (-> xml :attrs :name)
        parse-elements (fn [f type]
                         (fmap first
                               (group-by :name
                                         (map
                                          (comp (partial apply f name)
                                                (juxt :attrs (comp user-info-from-xml :content)))
                                          (filter #(and (= type (:tag %))
                                                        (not (:transient %)))
                                                  (:content xml))))))]
    (->Entity
     name
     (-> attribute-from-xml
         (parse-elements :attribute)
         (assoc "syncRevision" (->sync-revision name))
         (dissoc "isSuitableForPush"))
     (parse-elements relationship-from-xml :relationship)
     nil ; no datomic relationships at this stage
     (-> xml :content user-info-from-xml))))

(s/fdef entity-from-xml :ret ::entity)

; ------------- READING --------------

(defn entities-by-name [xml]
  (let [results (->> xml
                     (.getBytes)
                     (java.io.ByteArrayInputStream.)
                     clojure.xml/parse
                     :content
                     (filter #(= :entity (:tag %)))
                     (map entity-from-xml))
        pre-datomic (fmap first (group-by :name results))]
    (assert (reduce #(and %1 %2) (map (partial s/valid? ::entity) results)) (reduce str (map (partial s/explain-str ::entity) results)))
    (let [pairs (t/relationship-pairs pre-datomic)
          relevant-relationships (t/mapcat t/relevant-relationship-from-pair pairs)
          relevant-relationships-by-entity (group-by :entity-name relevant-relationships)
          full-entities (map #(assoc % :datomic-relationships
                                     (fmap first (group-by :name (get relevant-relationships-by-entity (:name %))))) results)]
      (assert (reduce #(and %1 %2) (map (partial s/valid? ::entity) full-entities)) (reduce str (map (partial s/explain-str ::entity) full-entities)))
      (fmap first (group-by :name full-entities)))))

