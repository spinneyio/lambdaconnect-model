(ns lambdaconnect-model.spec
    #?(:cljs (:require-macros [lambdaconnect-model.macro :refer [defspec]]))
  (:require #?@(:clj [[clojure.spec.alpha :as s] 
                      [clojure.spec.gen.alpha :as gen]
                      [lambdaconnect-model.utils :refer [defspec]]]
               :cljs [[cljs.spec.alpha :as s]
                      [cljs.spec.gen.alpha :as gen]])
            [clojure.walk :as walk]
            [lambdaconnect-model.tools :as t]
            [lambdaconnect-model.utils :as u]
            [clojure.repl :refer [doc]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [difference]]
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
  (if (:to-many rel)
    (cond
      (and (:min-value rel) (:max-value rel)) 
      (s/coll-of :app/relationship :min-count (:min-value rel) :max-count (:max-value rel))
      (:min-value rel)
      (s/coll-of :app/relationship :min-count (:min-value rel))
      (:max-value rel)
      (s/coll-of :app/relationship :max-count (:max-value rel))
      :default (s/coll-of :app/relationship))
    (if (:optional rel) (s/nilable :app/relationship)
        :app/relationship)))

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
  (let [all (concat (remove (comp t/fake-attribs :name) (vals (:attributes entity)))
                    (vals (:relationships entity)))        
        required (vec (map t/datomic-name (filter #(not (:optional %)) all)))
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
  (let [all (concat  (remove (comp t/fake-attribs :name) (vals (:attributes entity)))
                     (vals (:datomic-relationships entity)))
        required (vec (map t/datomic-name (filter #(not (:optional %)) all)))
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

(defn gensub
  "Sometimes the 'could not satisfy after 100 tries' message when generating spec makes us angry.
  It is because it does not return the field there are problems with. If you do (with-redefs [clojure.spec.alpha/gensub gensub] ...)
  you will receive additional info."
  [spec overrides path rmap form]
  
;;  (prn {:spec spec :over overrides :path path :form form})
  ;;(when (keyword? spec) (eval `(clojure.repl/doc ~spec)))
  (let [spec (#'s/specize spec)]
    (if-let [g (or (when-let [gfn (or (get overrides (or (#'s/spec-name spec) spec))
                                          (get overrides path))]
                       (gfn))
                     (#'s/gen* spec overrides path rmap))]
      (gen/such-that #(s/valid? spec %) g {:max-tries 100
                                           :ex-fn (fn [{:keys [max-tries]}]
                                                    (ex-info (str "Couldn't satisfy " (#'s/spec-name spec) "  after " max-tries " tries.")
                                                             {:max  max-tries
                                                              :path path
                                                              :sample-explain (->> (first (gen/sample g 1))
                                                                                   (#'s/explain-data spec)
                                                                                 :clojure.spec.alpha/problems)}))})
      (let [abbr (#'s/abbrev form)]
        (throw (ex-info (str "Unable to construct gen at: " path " for: " abbr)
                        {::path path ::form form ::failure :no-gen}))))))

(defn exception-from-failed-spec [spec object]
  (assert (not (s/valid? spec object)) "Logic error")
  (let [problems (->> object 
                      (s/explain-data spec)
                      #?(:clj :clojure.spec.alpha/problems :cljs :cljs.spec.alpha/problems)
                      (sort-by #(- (count (:in %))))
                      (sort-by #(- (count (:path %)))))
        extra {:object (pr-str object)
               :problems (map (fn [{:keys [val pred reason via in path] :as problem}]
                                (cond-> {:value (pr-str val)
                                         :validator (or reason (pr-str (s/abbrev pred)))}
                                  (not (empty? in)) (merge {:in (map str in)})
                                  (not (empty? path)) (merge {:at (map str path)})
                                  (not (empty? via)) (merge {:failing-spec-name (str (last via))
                                                             :failing-spec-description (pr-str (s/describe (last via)))})
                                  true (#(merge % (select-keys 
                                                   problem (difference 
                                                            (set (keys problem)) 
                                                            #{:val :pred :reason :via :in :path})))))) 
                              problems)}]
    (ex-info (str "Spec failed: " (s/explain-str spec object)) extra)))
