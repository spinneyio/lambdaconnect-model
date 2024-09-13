(ns lambdaconnect-model.graph-generator
  (:require #?@(:clj [[clojure.spec.alpha :as s]
                      [clojure.spec.gen.alpha :as gen]] 
                :cljs [[cljs.spec.alpha :as s]
                       [cljs.spec.gen.alpha :as gen]])
            [lambdaconnect-model.spec :as spec]
            [lambdaconnect-model.tools :as t]
            [lambdaconnect-model.transformations :as trafo]
            [lambdaconnect-model.utils :as u]
            [clojure.set :refer [difference]]
            [clojure.pprint :refer [pprint]]
            [lambdaconnect-model.core :as mp])
  #?(:cljs (:require-macros [lambdaconnect-model.macro :refer [dosync future]])))


(defn- rand-nth [coll]
  ;; We do not want it to throw for empty collection. Instead let it return nil
  (when (seq coll)
    (nth coll (rand-int (count coll)))))

(defn- clojure-to-json
  ;; We want to skip datomic relationships
  ([obj entity]
   (trafo/clojure-to-json
    obj
    entity
    t/inverse-parser-for-attribute
    t/inverse-parser-for-relationship
    true))
  ([obj entity inverse-parser-for-attribute inverse-parser-for-relationship]
   (trafo/clojure-to-json
    obj
    entity
    inverse-parser-for-attribute
    inverse-parser-for-relationship
    true)))



#?(:cljs 
   (defn ref [content]
     (atom content)))

#?(:cljs 
   (def alter swap!))

(defn generate-entity-graph 
  "Generates JSON representation of a random entity graph 
   (keys are strings, values are strings or numbers).
  Assumes that specs for ebn have been generated."
  [entities-by-name & {:keys [vertices
                              edges
                              max-retries
                              create-sync-revisions?] 
                       :or {edges 1000
                            max-retries 100
                            vertices 100
                            create-sync-revisions? false}}]
  (let [datomic-spec-for-name (fn [n]
                                (keyword "lambdaconnect-model.spec.datomic" n))
        
        entity-names (vec (keys entities-by-name))
        gens (into {} (map #(vector % (s/gen (datomic-spec-for-name %))) 
                           (keys entities-by-name)))
        
        datomic-rels-for-entity (memoize (fn [entity-name]
                                           (->> entity-name
                                                (get entities-by-name)
                                                :datomic-relationships
                                                (vals)
                                                (map t/datomic-name)
                                                (set))))

        ;; Create objects, sort them by uuid and remove all relationships

        objects (->> (range vertices) 
                     (u/pmap (fn [_] (let [entity-name (rand-nth entity-names)
                                           gen (gens entity-name)
                                           generated (-> gen 
                                                         (gen/sample 1)
                                                         first)
                                           interesting-keys (set (keys generated))] 
                                       [entity-name 
                                        (ref (select-keys generated 
                                                          (difference interesting-keys 
                                                                      (datomic-rels-for-entity entity-name))))
                                        
                                        ])))
                     (group-by first)
                     ((fn [x] 
                        (update-vals x 
                                     (fn [samples]
                                       (into {} (map 
                                                 #(vector (:app/uuid @%) %) 
                                                 (map second samples))))))))
        existing-relationships (ref #{})
        
        
        add-relationship (fn [retry-count] 
                           (let [entity-name (rand-nth entity-names)
                                 entity (get entities-by-name entity-name)
                                 relationship (->> entity 
                                                  :datomic-relationships
                                                  keys
                                                  rand-nth
                                                  (get (:datomic-relationships entity)))
                                 
                                 result 
                                 (when relationship 
                                   (dosync
                                    (let [inverse (-> relationship
                                                      :inverse-entity
                                                      (as-> en (get entities-by-name en))
                                                      :relationships
                                                      (get (:inverse-name relationship)))                                 
                                          rel-name (t/datomic-name relationship)
                                          sources (-> objects
                                                       (get entity-name))
                                          source (-> sources vals rand-nth)
                                          targets (-> objects
                                                       (get (:inverse-entity relationship)))
                                          target (-> targets vals rand-nth)]
                                      (assert inverse)
                                      (when (and source target)
                                        (let [rel-flag (if (and (not (:to-many relationship))
                                                                (not (:to-many inverse)))
                                                         [rel-name (:app/uuid @source)]
                                                         [rel-name (:app/uuid @source) 
                                                          (:app/uuid @target)]
                                                         )]
                                              (when-not (@existing-relationships rel-flag)
                                                (alter existing-relationships conj rel-flag)
                                                (alter source (fn [obj]
                                                                (if (:to-many relationship)
                                                                  (update obj rel-name #(conj (or % []) {:app/uuid (:app/uuid @target)}))
                                                                  (assoc obj rel-name {:app/uuid (:app/uuid @target)}))))
                                                       true))))))]
                             (if (and (not result) (> retry-count 0))
                               (recur (dec retry-count))
                               (> retry-count 0))))
        parallel-factor (min 10 edges)
        
        _ (mapv deref  
                (map (fn [_] (future (loop [i 0] 
                                       (when (and (< i (/ edges parallel-factor))
                                                  (add-relationship max-retries))
                                         
                                         (recur (inc i)))))) 
                     (range parallel-factor)))]
    
    (u/update-vals 
     objects
     #(->> %2 
           vals
           (u/pmap (fn [r] 
                     (let [mapped (clojure-to-json @r (get entities-by-name %1))]
                       (if create-sync-revisions? 
                         (assoc mapped "syncRevision" (rand-int 1000000))
                         mapped))))
           (doall)))))


