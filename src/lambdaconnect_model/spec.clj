(ns lambdaconnect-model.spec
  (:require [clojure.spec.alpha]
            [clojure.walk :as walk]
            [lambdaconnect-model.tools :as t]
            [lambdaconnect-model.data-xml :as xml]))

(clojure.spec.alpha/def :app/uuid uuid?)
(clojure.spec.alpha/def :app/active boolean?)
(clojure.spec.alpha/def :app/createdAt inst?)
(clojure.spec.alpha/def :app/updatedAt inst?)

(clojure.spec.alpha/def :app/reference (clojure.spec.alpha/keys :req [:app/uuid]))

(defn validator-for-attribute [attr]
   (let [basic (xml/basic-validator-symbols (:type attr))
         advanced (concat [basic]
                         (filter identity
                                 (case (:type attr)
                                   :db.type/string [(when (:regular-expression attr)
                                                       `#(re-matches ~(:regular-expression attr) %))
                                                    (when (:min-value attr)
                                                      `#(>= (count %) ~(:min-value attr)))                                                      
                                                    (when (:max-value attr)
                                                      `#(<= (count %) ~(:max-value attr)))]
                                   :db.type/long [(when (:min-value attr)
                                                    `#(>= % ~(:min-value attr)))
                                                  (when (:max-value attr)
                                                    `#(<= % ~(:max-value attr)))]
                                   :db.type/instant [(when (:min-value attr)
                                        ; We use the default java.util.Date here for simplicity
                                                       `#(.before ~(:min-value attr) %))
                                                     (when (:max-value attr)
                                                       `#(.after ~(:max-value attr) %))]
                                   :db.type/float [(when (:min-value attr)
                                                     `#(>= % ~(:min-value attr)))
                                                   (when (:max-value attr)
                                                      `#(<= % ~(:max-value attr)))]

                                   :db.type/double [(when (:min-value attr)
                                                      `#(>= % ~(:min-value attr)))
                                                    (when (:max-value attr)
                                                      `#(<= % ~(:max-value attr)))]
                                   [])))
         form (if (= 1 (count advanced)) 
                basic
                `(clojure.spec.alpha/and ~@advanced))]
     (eval form)))

(defn validator-for-relationship
  [rel]
  (let [min (if (:min-value rel) [:min-count (:min-value rel)] [])
        max (if (:max-value rel) [:max-count (:max-value rel)] [])
        ref (concat [:app/reference] min max)]
    (if (:to-many rel)
      (apply (t/functionise clojure.spec.alpha/coll-of) ref)
      (clojure.spec.alpha/nilable :app/reference))))

(defn spec-for-entity
  [generators entity]
  (doseq [attr (filter #(not (t/special-attribs (:name %)))
                       (vals (:attributes entity)))]
    (let [val (validator-for-attribute attr)
          gen ((t/datomic-name attr) generators)
          full-validator (if (:optional attr) (clojure.spec.alpha/nilable val) val)]
      ((t/functionise clojure.spec.alpha/def) (t/datomic-name attr) (if gen (clojure.spec.alpha/with-gen full-validator gen) full-validator))))
  (doseq [rel (vals (:relationships entity))]
    (let [val (validator-for-relationship rel)]
      ((t/functionise clojure.spec.alpha/def) (t/datomic-name rel) val)))
  (let [all (concat (filter #(not (t/special-attribs (:name %))) (vals (:attributes entity)))
                    (vals (:relationships entity)))
        required (vec (concat
                       [:app/uuid
                        :app/active
                        :app/createdAt
                        :app/updatedAt]
                       (map t/datomic-name (filter #(not (:optional %)) all))))
        optional (vec (map t/datomic-name (filter :optional all)))]
    ((t/functionise clojure.spec.alpha/def) (keyword "lambdaconnect-model.spec.json" (:name entity))
                                            ((t/functionise clojure.spec.alpha/keys)
                                             :req required
                                             :opt optional))
    true))

(defn datomic-spec-for-entity
  [generators entity]
  (doseq [attr (filter #(not (t/special-attribs (:name %)))
                       (vals (:attributes entity)))]
    (let [val (validator-for-attribute attr)
          gen ((t/datomic-name attr) generators)
          full-validator (if (:optional attr) (clojure.spec.alpha/nilable val) val)]
      ((t/functionise clojure.spec.alpha/def) (t/datomic-name attr) (if gen (clojure.spec.alpha/with-gen full-validator gen) full-validator))))
  (doseq [rel (vals (:relationships entity))]
    (let [val (validator-for-relationship rel)]
      ((t/functionise clojure.spec.alpha/def) (t/datomic-name rel) val)))
  (let [all (concat (filter #(not (t/special-attribs (:name %)))
                            (vals (:datomic-relationships entity))))
        required (vec (concat
                       [:app/uuid
                        :app/active
                        :app/createdAt
                        :app/updatedAt]
                       (map t/datomic-name (filter #(not (:optional %)) all))))
        optional (vec (map t/datomic-name (filter :optional all)))]
    ((t/functionise clojure.spec.alpha/def) (keyword "lambdaconnect-model.spec.datomic" (:name entity))
                                            ((t/functionise clojure.spec.alpha/keys)
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

