(ns lambdaconnect-model.transformations
  (:require [lambdaconnect-model.tools :as t]
            [clojure.string]))

(defn json-to-clojure
  "Converts json-based map into the one that conforms to the spec. "
  [json entity]
  (into {}
        (keep identity (map (fn [[k val]]
                              (let [key (clojure.string/replace k #"_" "-")
                                    attr (get (:attributes entity) key)
                                    rel  (get (:relationships entity) key)]
                                (assert (or rel attr) (str "The json does not match the entity '" (:name entity) "'. The attribute attribute: " key " with value: '" val "' does not exist in the model."))
                                [(t/datomic-name (or attr rel)), (if (and (nil? val) (:optional attr)) nil
                                                                     ((if attr (t/parser-for-attribute attr) (t/parser-for-relationship rel)) val))])) json))))

(defn- inverses-fun [names entity]
  (into {} (map (fn [n] (let [inverse (t/relationship-for-inverse-name entity n)]
                          [n (when inverse (t/datomic-name inverse))])) names)))

(def memoized-inverses (memoize inverses-fun))

(defn replace-inverses
  [obj entity untangle-singles]
  (let [names (keys obj)
        inverses (memoized-inverses names entity)]
    (into {} (map (fn [[key value]]
                    (let [ik (get inverses key)]
                      [(or ik key) (if (and untangle-singles
                                            ik
                                            (not (:to-many (get (name key) (:relationships entity)))))
                                     (first value)
                                     value)])) obj))))

(defn clojure-to-json
  [obj entity inverse-parser-for-attribute inverse-parser-for-relationship]
  (let [entity-fields (merge (:attributes entity) (:relationships entity))]
    (->> entity-fields
         (map (fn [[name attr-or-rel]]
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
                    [(clojure.string/replace name  #"-" "_")
                     (inverse-parser val)]))))
         (into {}))))