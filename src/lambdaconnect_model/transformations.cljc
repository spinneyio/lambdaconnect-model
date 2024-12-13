(ns lambdaconnect-model.transformations
  (:require [lambdaconnect-model.tools :as t]
            [clojure.set :refer [difference]]
            [lambdaconnect-model.utils :refer [merge update-vals rebuild-map]]
            [clojure.string]))


(defn build-parser-cache [entity parser-for-attribute parser-for-relationship]
  (into {} (concat 
            (map (fn [[k a]] [k [(t/datomic-name a) (parser-for-attribute a) (:optional a)]]) (:attributes entity))
            (map (fn [[k r]] [k [(t/datomic-name r) (parser-for-relationship r) (:optional r)]]) (:relationships entity))
            [[:entity-hash (hash entity)]
             [:attribute-parser-hash (hash parser-for-attribute)]
             [:relationship-parser-hash (hash parser-for-relationship)]])))

(def parser-cache (atom {}))

(defn json-to-clojure
  "Converts json-based map into the one that conforms to the spec. "
  ([json entity parser-for-attribute parser-for-relationship]
   (let [cached (get @parser-cache (:name entity))
         parsers (if (and cached 
                          (= (:attribute-parser-hash cached) (hash parser-for-attribute))
                          (= (:relationship-parser-hash cached) (hash parser-for-relationship))
                          (= (:entity-hash cached) (hash entity)))
                   cached
                   (let [built (build-parser-cache entity parser-for-attribute parser-for-relationship)]
                     (swap! parser-cache assoc (:name entity) built)
                     built))]
     (rebuild-map json 
                  (fn [key val]
                    (let [[datomic-name parser optional?] (get parsers key)]
                      (assert parser (str "The json does not match the entity '" (:name entity) "'. The attribute attribute: " key " with value: '" val "' does not exist in the model."))
                      [datomic-name
                       (if (and (nil? val) optional?) 
                         nil
                         (parser val))]))))))

(defn- inverses-fun [entity]
  (let [non-datomic-rels (difference (set (vals (:relationships entity)))
                                     (set (vals (:datomic-relationships entity))))]
    (assoc 
    (->> non-datomic-rels
         (map (juxt t/datomic-inverse-name t/datomic-name))
         (into {}))
    :entity-hash (hash entity))))
        
(def inverses-cache (atom {}))

(defn memoized-inverses [entity]
  (let [compute (fn [] 
                  (let [computed (inverses-fun entity)]
                    (swap! inverses-cache assoc  (:name entity) computed)
                    computed))]
    (if-let [cached (get @inverses-cache (:name entity))]
      (if (= (hash entity) (:entity-hash cached))
        cached
        (compute))
      (compute))))

(defn replace-inverses
  [obj entity untangle-singles]
  (let [inverses (memoized-inverses entity)]
    (rebuild-map obj 
                 (fn [key value]
                   (let [ik (get inverses key)]
                     [(or ik key) (if (and untangle-singles
                                           ik
                                           (not (:to-many (get (:relationships entity) (name ik)))))
                                    (first value)
                                    value)])))))

(defn clojure-to-json
  ([obj entity inverse-parser-for-attribute inverse-parser-for-relationship]
   (clojure-to-json obj entity inverse-parser-for-attribute inverse-parser-for-relationship false))
  ([obj entity inverse-parser-for-attribute inverse-parser-for-relationship skip-non-datomic?]
  (let [relationships (if skip-non-datomic?                        
                        (:datomic-relationships entity)
                        (:relationships entity))
        entity-fields (merge (:attributes entity) 
                             relationships)]
    (rebuild-map 
     entity-fields
     (fn [name attr-or-rel]
       (let [fake? (t/fake-attribs name)
             k (if fake? (keyword name) (t/datomic-name attr-or-rel))
             pre-v (k obj)
             val (if (nil? pre-v)
                   (if (:to-many attr-or-rel) [] nil)
                   pre-v)
             inverse-parser (cond
                              (nil? pre-v)
                              identity
                              (:destination-entity attr-or-rel) ;; this is relationship                           
                              (inverse-parser-for-relationship attr-or-rel)
                              :else
                              (inverse-parser-for-attribute attr-or-rel))]
         (if (and fake? (nil? pre-v))
           nil
           [name
            (inverse-parser val)])))))))
