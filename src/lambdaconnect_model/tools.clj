(ns lambdaconnect-model.tools
  (:require [clojure.string :as s]
            [clj-time.format :as time-format]
            [clj-time.coerce :as c]
            [clojure.spec.alpha :as spec]
            [clojure.set :refer [difference intersection]]))

; Those fileds are excluded from automatic schema generation as they all have "app" prefix and are common for all the objects
(def special-attribs #{"active" "uuid" "createdAt" "updatedAt" "syncRevision"})
(def special-unmodifiable-attribs (difference special-attribs #{"active"}))
(def fake-attribs #{"syncRevision"})

(def time-formatter (time-format/formatter :date-time)) ; ISO8601

(defmacro functionise [macro]
  `(fn [& args#] (eval (cons '~macro args#))))

(defn unique-datomic-identifier [entity]
  (keyword (:name entity) "ident__"))

(defn datomic-name
  [o]
  (let [n (:name o)]
    (keyword (if (special-attribs n) "app" (:entity-name o)) n)))

(defn datomic-inverse-name
  [rel]
  (let [n (:inverse-name rel)]
    (keyword (:destination-entity rel) (str "_" n))))

(defn attrib-or-relationship [entities-by-name j]
  (let [[e n] (if (keyword? j) [(namespace j) (name j)] (s/split (str j) #"/"))
        entity (entities-by-name e)
        attr ((:attributes entity) n)
        rel  ((:relationships entity) n)]
    (assert (or attr rel) (str "There is no attribute nor relationship named '" n "' for entity '" e "'"))
    (assert (not (and attr rel)) (str "There is a relationship AND an attribute named '" n "' for entity '" e "'"))
    (or attr rel)))

(defn mapcat
  ; We need our own implementation, see http://clojurian.blogspot.com/2012/11/beware-of-mapcat.html
  ([f coll] (lambdaconnect-model.tools/mapcat f coll (lazy-seq [])))
  ([f coll acc]
   (if (empty? coll) acc
       (recur f (rest coll) (lazy-seq (concat acc (f (first coll))))))))

(defn to-database-date [date]
  (if (instance? java.util.Date date) date (c/to-date date)))

(defn string->uuid
  [s]
  (if (uuid? s) s
      (do
        (assert (string? s) (str "Not a string passed as a UUID: " s))
        (. java.util.UUID fromString s))))

; ================================================

;                      Parsers

(defn parser-for-attribute [attribute]
  (if (= (:name attribute) "uuid")
    string->uuid
    (case (:type attribute)
      :db.type/uuid string->uuid
      :db.type/instant #(when (not (nil? %)) (->> %
                                                  (time-format/parse time-formatter)
                                                  (to-database-date)))
      :db.type/boolean #(when (not (nil? %)) (if-not (= false %)
                                               (or (= % true) (> % 0))
                                               false))
      :db.type/bytes (assert false "Not supported yet")
      :db.type/double double
      :db.type/float float
      identity)))

(defn inverse-parser-for-attribute [attribute]
  (if (= (:name attribute) "uuid")
    str
    (case (:type attribute)
      :db.type/instant #(->> %
                             (c/from-date)
                             (time-format/unparse time-formatter))
      :db.type/uuid str
      :db.type/boolean #(if % 1 0)
      :db.type/bytes (assert false "Not supported yet")
      identity)))

(defn parser-for-relationship [rel]
  (if (:to-many rel)
    #(map (fn [uuid-string] {:app/uuid (string->uuid uuid-string)}) %)
    #(when % {:app/uuid (string->uuid %)})))

(defn inverse-parser-for-relationship [rel]
  (if (:to-many rel)
    #(map (comp str :app/uuid) %)
    #(when % (if (sequential? %) (first (map (comp str :app/uuid) %)) ((comp str :app/uuid) %)))))

(defn relationship-for-inverse-name [entity inverse-name] ; e.g. :FIGame/_organiser
  (when (= \_ (first (name inverse-name)))
    (let [rels (vals (:relationships entity))
          inverse-entity-name (namespace inverse-name)
          inverse-rel-name (subs (name inverse-name) 1)]
      (first (filter #(and (= (:inverse-entity %) inverse-entity-name)
                           (= (:inverse-name %) inverse-rel-name)) rels)))))

; ================================================

(defn relationship-pairs
  "Generates a set of pairs of relationships"
  [e-by-name]
  (map #(sort-by :name (vec %))
       (mapcat (fn [entity]
                 (set
                  (map
                   (fn [rel]
                     (let [dest-ent (get e-by-name (:destination-entity rel))]
                       [rel (get (:relationships dest-ent) (:inverse-name rel))]))
                   (vals (:relationships entity)))))
               (vals e-by-name))))

(defn relevant-relationship-from-pair
  "Relationship generation strategy."
  [[left right]]
  (cond
    (and (not (:to-many left)) (not (:to-many right))) [left]
    (and (not (:to-many left)) (:to-many right)) [left]
    (and (:to-many left) (not (:to-many right))) [right]
    (and (:to-many left) (:to-many right)) [left]))

(defn defining-attributes [entities-by-name]
  (into {} (map (fn [[name entity]]
                  [name (unique-datomic-identifier entity)])
                (vec entities-by-name))))

(defn compare-objects [o1 o2 entity]
  (let [o1-keys (set (keys o1))
        o2-keys (set (keys o2))
        common-keys (intersection o1-keys o2-keys)
        spec (keyword "lambdaconnect-model.spec.json" (:name entity))]
    (and
     (spec/valid? spec o1)
     (spec/valid? spec o2)
     (= (select-keys o1 common-keys) (select-keys o2 common-keys))
     (reduce #(and %1 %2) true (map #(or (nil? (% o1)) (empty? (% o1))) (difference o1-keys o2-keys)))
     (reduce #(and %1 %2) true (map #(or (nil? (% o2)) (empty? (% o2))) (difference o2-keys o1-keys))))))
