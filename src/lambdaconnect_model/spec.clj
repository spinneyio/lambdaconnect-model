(ns lambdaconnect-model.spec
  (:require [clojure.spec.alpha]
            [lambdaconnect-model.tools :as t]
            [lambdaconnect-model.data-xml :as xml]))

(clojure.spec.alpha/def :app/uuid uuid?)
(clojure.spec.alpha/def :app/active boolean?)
(clojure.spec.alpha/def :app/createdAt inst?)
(clojure.spec.alpha/def :app/updatedAt inst?)

(clojure.spec.alpha/def :app/reference (clojure.spec.alpha/keys :req [:app/uuid]))


(defn validator-for-attribute [attr]
  (let [basic (xml/basic-validators (:type attr))
        advanced (concat [{:validator basic
                           :description basic}]
                         (filter identity
                                 (case (:type attr)
                                   :db.type/string [(when (:regular-expression attr)
                                                      {:validator #(re-matches (:regular-expression attr) %)
                                                       :description (str  "Regexp: '" (:regular-expression attr) "'")})
                                                    (when (:min-value attr)
                                                      {:validator #(>= (count %) (:min-value attr))
                                                       :description (str "Min-length: " (:min-value attr))})
                                                    (when (:max-value attr)
                                                      {:validator #(<= (count %) (:max-value attr))
                                                       :description (str "Max-length: " (:max-value attr))})]
                                   :db.type/long [(when (:min-value attr)
                                                    {:validator #(>= % (:min-value attr))
                                                     :description (str "Min-value: " (:min-value attr))})
                                                  (when (:max-value attr)
                                                    {:validator #(<= % (:max-value attr))
                                                     :description (str "Max-value " (:max-value attr))})]
                                   :db.type/instant [(when (:min-value attr)
                                        ; We use the default java.util.Date here for simplicity
                                                       {:validator #(.before (:min-value attr) %)
                                                        :description (str "Date after: " (:min-value attr))})
                                                     (when (:max-value attr)
                                                       {:validator #(.after (:max-value attr) %)
                                                        :description (str "Date before: " (:max-value attr))})]
                                   :db.type/float [(when (:min-value attr)
                                                     {:validator #(>= % (:min-value attr))
                                                      :description (str "Min-value: " (:min-value attr))})
                                                   (when (:max-value attr)
                                                     {:validator #(<= % (:max-value attr))
                                                      :description (str "Max-value " (:max-value attr))})]

                                   :db.type/double [(when (:min-value attr)
                                                      {:validator #(>= % (:min-value attr))
                                                       :description (str "Min-value: " (:min-value attr))})
                                                    (when (:max-value attr)
                                                      {:validator #(<= % (:max-value attr))
                                                       :description (str "Max-value " (:max-value attr))})]
                                   [])))]
    (clojure.spec.alpha/and-spec-impl (map :description advanced) (map :validator advanced) nil)))

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

