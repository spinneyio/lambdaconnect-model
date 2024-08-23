(ns lambdaconnect-model.spec
    #?(:cljs (:require-macros [lambdaconnect-model.macro :refer [defspec]]))
  (:require #?(:clj [clojure.spec.alpha :as s] 
               :cljs [cljs.spec.alpha :as s])
            [clojure.walk :as walk]
            #?(:clj [lambdaconnect-model.utils :refer [defspec]])
            [lambdaconnect-model.tools :as t]
            [lambdaconnect-model.utils :as u]
            [lambdaconnect-model.data-xml :as xml]))

(s/def :app/uuid uuid?)
(s/def :app/active boolean?)
(s/def :app/createdAt :types/inst)
(s/def :app/updatedAt :types/inst)

(s/def :app/relationship (s/keys :req [:app/uuid]))

(defn- validator-form-for-attribute [attr]
   (let [basic (xml/basic-validators (:type attr))
         advanced (concat [basic]
                         (filter identity
                                 (case (:type attr)
                                   :db.type/string [(when (:regular-expression attr)
                                                       #(re-matches (:regular-expression attr) %))
                                                    (when (:min-value attr)
                                                      #(>= (count %) (:min-value attr)))                                                      
                                                    (when (:max-value attr)
                                                      #(<= (count %) (:max-value attr)))]
                                   :db.type/long [(when (:min-value attr)
                                                    #(>= % (:min-value attr)))
                                                  (when (:max-value attr)
                                                    #(<= % (:max-value attr)))]
                                        ; We use the default java.util.Date here for simplicity
                                   :db.type/instant #?(:clj [(when (:min-value attr)
                                                               #(.before (:min-value attr) %))
                                                             (when (:max-value attr)
                                                               #(.after (:max-value attr) %))]
                                                       :cljs [(when (:min-value attr)
                                                               #(<= (:min-value attr) %))
                                                             (when (:max-value attr)
                                                               #(>= (:max-value attr) %))])
                                   :db.type/float [(when (:min-value attr)
                                                     #(>= % (:min-value attr)))
                                                   (when (:max-value attr)
                                                      #(<= % (:max-value attr)))]

                                   :db.type/double [(when (:min-value attr)
                                                      #(>= % (:min-value attr)))
                                                    (when (:max-value attr)
                                                      #(<= % (:max-value attr)))]
                                   [])))
         form (case (count advanced)
                1 basic
                2 (s/and (first advanced) (second advanced))
                3 (s/and (first advanced) (second advanced) (nth advanced 2))
                4 (s/and (first advanced) (second advanced) (nth advanced 2) (nth advanced 3))
                )]
     (if (:optional attr) (s/nilable form) form)))


(defn validator-for-relationship
  [rel]
  (let [min (if (:min-value rel) [:min-count (:min-value rel)] [])
        max (if (:max-value rel) [:max-count (:max-value rel)] [])
        ref (concat [:app/relationship] min max)]
    (if (:to-many rel)
      (case (count ref)
        1 (s/coll-of (first ref))
        3 (s/coll-of (first ref) (nth ref 1) (nth ref 2))
        5 (s/coll-of (first ref) (nth ref 1) (nth ref 2) (nth ref 3) (nth ref 4)))
      (s/nilable :app/relationship))))

(defn spec-for-entity
  [generators entity]
  (doseq [attr (filter #(not (t/special-attribs (:name %)))
                       (vals (:attributes entity)))]
    (let [val (validator-form-for-attribute attr)
          gen ((t/datomic-name attr) generators)]
      (if gen
        (defspec (t/datomic-name attr) (s/with-gen val gen))
        (defspec (t/datomic-name attr) val))))
  (doseq [rel (vals (:relationships entity))]
    (let [val (validator-for-relationship rel)]
      (defspec  (t/datomic-name rel) val)))
  (let [all (concat (filter #(not (t/special-attribs (:name %))) (vals (:attributes entity)))
                    (vals (:relationships entity)))
        required (vec (concat
                       [:app/uuid
                        :app/active
                        :app/createdAt
                        :app/updatedAt]
                       (map t/datomic-name (filter #(not (:optional %)) all))))
        optional (vec (map t/datomic-name (filter :optional all)))]
    (defspec (keyword "lambdaconnect-model.spec.json" (:name entity)) 
      (u/keys
       :req required
       :opt optional))
    true))

(defn datomic-spec-for-entity
  [generators entity]
  (doseq [attr (filter #(not (t/special-attribs (:name %)))
                       (vals (:attributes entity)))]
    (let [val (validator-form-for-attribute attr)
          gen ((t/datomic-name attr) generators)]
      (if gen 
        (defspec (t/datomic-name attr) (s/with-gen val gen)) 
        (defspec (t/datomic-name attr) val))))
  (doseq [rel (vals (:relationships entity))]
    (let [val (validator-for-relationship rel)]
      (defspec (t/datomic-name rel) val)))
  (let [all (concat (filter #(not (t/special-attribs (:name %)))
                            (vals (:datomic-relationships entity))))
        required (vec (concat
                       [:app/uuid
                        :app/active
                        :app/createdAt
                        :app/updatedAt]
                       (map t/datomic-name (filter #(not (:optional %)) all))))
        optional (vec (map t/datomic-name (filter :optional all)))]
    (defspec (keyword "lambdaconnect-model.spec.datomic" (:name entity))
      (u/keys
       :req required
       :opt optional))
    true))

(defn specs-for-entities [entities-by-name generators]
  (doall ; we force the computation to perform schema generation as a side effect
   (->> entities-by-name
        (vals)
        (map (partial spec-for-entity generators))))
  (doall ; we force the computation to perform schema generation as a side effect
   (->> entities-by-name
        (vals)
        (map (partial datomic-spec-for-entity generators)))))
