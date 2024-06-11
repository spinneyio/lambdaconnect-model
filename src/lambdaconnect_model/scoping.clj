(ns lambdaconnect-model.scoping
  (:require [clojure.edn]
            [clojure.set :refer [subset? difference intersection union]]
            [taoensso.tufte :as profile]
            [clojure.spec.alpha :as s]
            [lambdaconnect-model.utils :refer [relevant-tags]]
            [clojure.pprint :refer [pprint]]
            [lambdaconnect-model.spec :as spec]
            [clojure.string :as str]
            [lambdaconnect-model.tools :as t]))

; ====================== RULE REORDERING FOR PERFORMANCE =========================

(defn s-dependencies [rule]
  (cond
    (seq? rule)
    (let [op (first rule)]
      (cond
          ; We do not use the "not" rules for allocating paths, since these are more efficient when left alone
        (= op 'not) nil ;(s-dependencies (second rule))
        (= op 'and) (reduce union #{} (map s-dependencies (rest rule)))
        (= op 'or-join) (set (second rule)) ; or-join has been fixed and now can be asfely re-ordered
        ))

    (seq? (first rule))
        ; (= ?a ?b)
    (let [[op arg1 arg2] (first rule)]
      #{arg1 arg2})
        ; [?a :user/aaa ?b]
    :else (let [[arg1 op arg2] rule]
            #{arg1 arg2})))

(defn s-matches? [match-tag rule]
  (let [deps (s-dependencies rule)]
    (reduce #(or %1 %2) false (map #(= % match-tag) deps))))

(defn reorder-rules
  ([remaining-rules final-tags]
   (reorder-rules (set remaining-rules) [] #{'?user} final-tags))
  ([remaining-rules result next-matches final-tags]
   (assert (vector? result))
   (assert (set? remaining-rules))
   (letfn [(dive-deeper [rule overlaying-dependencies]
             (let [op (first rule)]
               (cond
                 (= op 'not) (list 'not (dive-deeper (second rule) overlaying-dependencies))
                 (= op 'and) `(~'and ~@(reorder-rules (set (rest rule)) [] (difference overlaying-dependencies final-tags) final-tags))
                 (= op 'or-join) (let [starting-dependencies (set (second rule))]
                                   `(~'or-join ~(second rule) ~@(map #(dive-deeper % starting-dependencies) (rest (rest rule)))))
                 :else rule)))]
     (let [matching-rules (set (mapcat (fn [tag] (filter (partial s-matches? tag) remaining-rules)) next-matches))
           all-rule? (fn [rule]
                       (let [attr (second rule)]
                         (and (keyword? attr) (= "ident__" (name attr)))))
           all-rules (set (filter all-rule? remaining-rules))

           comparator (fn [a b]
                        (let [goes-up? (fn [r]
                                         (or ; we manually bubble up queries that seem to need priority
                                          (and (s-matches? '?user r)
                                               (not (seq? r)))
                                          (and (keyword? (second r))
                                               (= (name (second r)) "internalUserId"))
                                          (seq? (first r))))]
                          (if (and (goes-up? a)
                                   (goes-up? b))
                            (if (s-matches? '?user a)
                              -1
                              1)
                            (cond (goes-up? a) -1
                                  (goes-up? b) 1
                                  :else 0))))
           get-ordered-result (fn [rules] (vec (sort comparator (concat result (vec (map #(dive-deeper % nil) rules))))))]
       ;; (println "MATCHING: " matching-rules)
       ;; (println "REMAINING: " remaining-rules)
       ;; (println "MATCHES: " next-matches)
       (cond
         (= (count matching-rules) (count remaining-rules)) (get-ordered-result matching-rules)
         (seq matching-rules) (recur (difference remaining-rules matching-rules)
                                     (get-ordered-result matching-rules)
                                     (difference (reduce union #{} (map set (map s-dependencies matching-rules))) next-matches)
                                     final-tags)
         (seq all-rules)  (recur (difference remaining-rules all-rules)
                                 (get-ordered-result all-rules)
                                 (difference (reduce union #{} (map set (map s-dependencies all-rules))) next-matches)
                                 final-tags)
         :else (get-ordered-result remaining-rules))))))

; ================================================== QUERY SPLIT FOR PERFORMANCE ============================================
;
; In certains scenarios the query engine takes a long long time to solve queries that have a top-level or-join
; In particular, if most of the queries contain the "?user" binding but one of them does not (the one that e.g. wants to have all of them but with a condition).
; Therefore, when we find a query that has a single "or" amongst its highest level "where" clauses, we restructure it into multiple queries and execute all of them

(defn execute-query-generic [config tag query]
;  (println "Trying to execute query: " query)
  (let [res (profile/p tag (->> query (apply (:q config)) (mapv first) set))]
 ;   (println "COMPUTED:" res)
    [tag res]))

(defn execute-query-split [config tag query]
  ; a query has the following form:
  ; [ '[:find [?LDMedia-allowedForMe ...] :in $ [?user ...]  :where ...] snapshot #{ 312932198 }]
  ;
  ; we need to extract the where part and check it for the join condition existence
  ;
  (let [[v snapshot entry-set] query
        [beginning where] (split-at (.indexOf v :where) v)
        entity-symbol (first (second beginning))
        where-clauses (rest where)
        or-join-clauses (filter #(and (seq? %) (= 'or-join (first %)))  where-clauses)]
    (if (not= (count or-join-clauses) 1)
        ;; (println "QUERY:" )
        ;; (pprint query)
        ;; (println "--------------")
      [tag (profile/p tag (set (or (apply (:q config) query) #{})))]
      (let [special-clause (first or-join-clauses)
            remaining-clauses (set (filter #(not= % special-clause) where-clauses))
            subsections (drop 2 special-clause)
            results (pmap (fn [subclause]
                            (let [subquery (if (and (seq? subclause) (= 'and (first subclause))) (set (rest subclause)) #{subclause})]
                              (second (execute-query-split config tag [(concat beginning [:where] (reorder-rules (union remaining-clauses subquery) #{entity-symbol})) snapshot entry-set])))) subsections)]
        [tag (reduce union results)]))))

; I have found that the performance gap was caused by the lack of ?user in the or join. No need for splitting anymore, but leaving the method for reference.

(defn execute-query [config tag query]
  (execute-query-generic config tag query))

; ================================================== /QUERY SPLIT FOR PERFORMANCE ============================================

; incoming json: {"FIUser" 123 "FIGame" 344} is a dictionary with entity names as keys

(defn none-rules
  [user-symbol entity-symbol]
  `[[(~'identity ~user-symbol) ~entity-symbol]
    [(~'!= ~user-symbol ~entity-symbol)]])

(defn- query-for-rule
  "returns a vector with dependencies set as first argument and rules as second.
   for example:

  [#{} []] "
  [entities-by-name
   tag
   rule
   applied-queries ; { tag {:dependencies #{:NOUser.me :NOMessage.sent} :rules [[?user :app/uuid ?m] [...]]}}
   constants
   ]
  (let [get-entity (fn [tag] (when tag (get entities-by-name (first (str/split (or (namespace tag) (name tag)) #"\.")))))
        symbol-for-tag #(when (keyword? %) (symbol (str "?" (reduce (fn [s1 s2] (str s1 "-" s2)) (str/split (or (namespace %) (name %)) #"\.")))))
        entity (get-entity tag)
        entity-symbol (symbol-for-tag tag)]

    (letfn [(nested-dependencies [dependencies]
              ; for a set of dependencies (e.g. #{ :NODietitian.me } ) produces a set that contains all the dependencies (e.g. #{ :NODietitian.me :NOUser.me :user } )
              (if (empty? dependencies) #{}
                  (union dependencies (reduce union (map nested-dependencies (map #(:dependencies (% applied-queries)) dependencies))))))

            (dependent-rules
              ([dependencies] ; #{:NOUser.me :NOMessage.sent ...}
               (dependent-rules dependencies #{:user} []))
              ([dependencies
                resolved-dependencies ; #{:NOUser.they}
                resolved-rules ; [[?user :app/uuid ?m] [...] ...]
                ]
               (let [ordered-dependencies (vec dependencies)]
                 (if-let [dependency (first ordered-dependencies)]
                   (let [{new-dependencies :dependencies rules :rules} (dependency applied-queries)
                         remaining-dependencies (difference new-dependencies resolved-dependencies)]
                     (recur
                      (union (set (rest ordered-dependencies)) remaining-dependencies)
                      (conj resolved-dependencies dependency)
                      (concat resolved-rules rules)))
                   resolved-rules))))

            (find-all-symbols [rules] ; takes a set of datomic where rules and produces a list of all symbols used
              (reduce union (map (fn [rule] (if (list? rule)
                                              (let [op (first rule)]
                                                (cond (= op 'not) (find-all-symbols [(second rule)])
                                                      (= op 'and) (reduce union (map find-all-symbols (rest rule)))
                                                      (= op 'or-join) (second rule)))
                                              (set (filter identity [(first rule) (when (> (count rule) 2) (nth rule 2))])))) rules)))

            (where-for-rule
            ;  Returns a tuple with dependencies, and where: [#{:NOUser.me :NONotification.sent} #{?user ?m} [[?user :app/uuid ?m] [...]]]
              [rule
               top-level
               ignored-dependencies]
              (case rule
                :all
                (let [symbol (symbol-for-tag tag)
                      entity-attr (keyword (:name entity) "ident__")]
                  [#{} `[[~symbol ~entity-attr]]])
                :none
                ;; Obviously falsy rule which binds output variable and USES it. Do not write [(= true false)] or sth similar,
                ;; Datomic might optimize that query and skip falsy rule, returning all objects.
                (let [symbol (symbol-for-tag tag)]
                  [#{} (none-rules '?user symbol)])

                (let [op (first rule)]
                  (if (= 'allowed-if op)
                    (let [const-key (-> 
                                     rule
                                     second
                                     name
                                     keyword)
                          _ (assert (contains? constants const-key) (str "The rule " rule " contains a constant " const-key " not present in constants map."))
                          const (const-key constants)
                          const-val (if (delay? const) @const const)]
                      (if top-level 
                        (where-for-rule (if const-val :all :none) top-level ignored-dependencies)
                        ;; We signal a special constant based rule as a boolean in the second arg instead of a real rule
                        [#{} (if const-val true false)]
                        ))                    
                    (if (list? rule)
                      (let [internal-wheres (map #(where-for-rule % false #{}) (rest rule))]
                        (let [boolean-wheres (->> internal-wheres
                                                     (map second)
                                                     (map boolean?))
                              has-bools? (seq (filter identity boolean-wheres))
                              boolean-where-vals (->> internal-wheres
                                                     (map second)
                                                     (filter boolean?))]
                          (if has-bools?
                            ;; one of the nested rules on this level is a boolean const
                            (cond (= op 'not)
                                  ;; Not always has one element
                                  (if top-level 
                                    (where-for-rule (if (first boolean-where-vals) :none :all) top-level ignored-dependencies)
                                    ;; We signal a special constant based rule as a boolean in the second arg instead of a real rule
                                    [#{} (not (first boolean-where-vals))])

                                  (= op 'or) (let [combined (reduce #(or %1 %2) boolean-where-vals)]
                                               (if combined 
                                                 (if top-level 
                                                   (where-for-rule :all top-level ignored-dependencies)
                                                   ;; We signal a special constant based rule as a boolean in the second arg instead of a real rule
                                                   [#{} true])
                                                 (where-for-rule 
                                                  ;; This complex form filters only the subrules that do not have boolean value since this was dealt with earlier.
                                                  (->> (map vector (rest rule) boolean-where-vals)
                                                       (filter (comp not second)) 
                                                       (map first)
                                                       (vec)
                                                       (concat ['or])
                                                       (reverse)
                                                       (into '())) 
                                                  top-level ignored-dependencies)))
                                  (= op 'and) (let [combined (reduce #(and %1 %2) boolean-where-vals)]
                                                (if-not combined 
                                                  (if top-level 
                                                    (where-for-rule :none top-level ignored-dependencies)
                                                    ;; We signal a special constant based rule as a boolean in the second arg instead of a real rule
                                                    [#{} false])
                                                  (where-for-rule 
                                                   ;; This complex form filters only the subrules that do not have boolean value since this was dealt with earlier.
                                                   (->> (map vector (rest rule) boolean-wheres)
                                                        (filter (comp not second)) 
                                                        (map first)
                                                        (vec)
                                                        (concat ['and])
                                                        (reverse)
                                                        (into '())) 
                                                   top-level ignored-dependencies))))
                            (let [dependency-list (map nested-dependencies (map first internal-wheres))
                                  common-dependencies (reduce (if (= op 'and) union intersection) dependency-list)
                                  final-wheres (map #(where-for-rule % false common-dependencies) (rest rule))
                                  internal-queries (reduce concat (map second final-wheres))
                                  particular-dependencies (map #(difference % common-dependencies ignored-dependencies) dependency-list)
                                  zipped (map vector (map second final-wheres) particular-dependencies)
                                  dependent-entities-from-common (union #{'?user entity-symbol} (set (map symbol-for-tag common-dependencies)))
                                  appended (reduce concat (map (fn [[where dep]]
                                                                 (let [rules (reorder-rules (set (dependent-rules dep common-dependencies where)) #{entity-symbol})]
                                                                   (if (and (> (count rules) 1) (= op 'or)) [`(~'and ~@rules)] rules))) zipped))
                                  used-entities (reduce union (map (fn [[where dep]] (find-all-symbols (dependent-rules dep common-dependencies where))) zipped))
                                  dependent-entities (intersection dependent-entities-from-common used-entities)
                                  ;; _ (println "OP: " op "rule: " rule)
                                  ;; _ (println "DEPENDENT: " dependent-entities-from-common)
                                  ;; _ (println "USED: " used-entities)
                                  ;; _ (println "INTerSeCTION: " (intersection dependent-entities-from-common used-entities))
                                  ]
                              ;; (when (or (= op 'and) (= op 'or))
                              ;;   (println "=================================================================")
                              ;;   (println "")
                              ;;   (println "--------- DEBUG WHERE FOR RULE OP=" (if (= op 'and) "AND" "OR") "-------------------")
                              ;;   (println "Relevant tags: " ordered-tags)
                              ;;   (println "----------------------------")
                              ;;   (println "Dependency list: " dependency-list)
                              ;;   (println "----------------------------")
                              ;;   (println "Dependent entities: " dependent-entities)
                              ;;   (println "----------------------------")
                              ;;   (println "common-dependencies: " common-dependencies)
                              ;;   (println "----------------------------")
                              ;;   (println "internal-queries: " internal-queries)
                              ;;   (println "----------------------------")
                              ;;   (println "particular-dependencies: " particular-dependencies)
                              ;;   (println "----------------------------")
                              ;;   (println "appended: " appended)
                              ;;   (println "----------------------------")
                              ;;    )

                              [common-dependencies (cond
                                                     (= op 'and) (if top-level
                                                                   appended
                                                                   `[(~'and ~@appended)])
                                                     (= op 'or)  `[(~'or-join [~@dependent-entities] ~@appended)]
                                                     (= op 'not) `[(~'not ~@internal-queries)])]))))
                      (let [value (second rule)                
;                            _ (println "RULE: " rule)
                            attribute (get (:attributes entity) (name value))
                            target (let [t (nth rule 2)]
                                     (if (and (keyword? t)
                                              (= "constant" (namespace t)))
                                       (let [t-k (keyword (name t))
                                             constant-value (if (delay? (t-k constants)) 
                                                              @(t-k constants) 
                                                              (t-k constants))] 
                                         (assert (contains? constants t-k) (str "Constants do not contain " t-k))
                                         (assert attribute (str "Error in rule: " rule " - only attribute fields can be assigned constants."))
                                         (assert (s/valid? (t/datomic-name attribute) constant-value) (s/explain-str (t/datomic-name attribute) constant-value))
                                         constant-value) 
                                       t))                       
                            relevant-tags (relevant-tags rule)
                            user-target (when-not (boolean? target)
                                          (if (and (= (name target) "uuid") (= (namespace target) "user")) :app/uuid target))
                            target-tag (first relevant-tags)
                            target-symbol (symbol-for-tag target-tag)
                            attribute (get (:attributes entity) (name value))
                            relationship (get (:relationships entity) (name value))
                            target-entity (get-entity target-tag)
                            target-attribute (when (keyword? target) (get (:attributes target-entity) (name target)))
                            target-relationship (when target-tag (get (:relationships target-entity) (:inverse-name relationship)))
                            target-datomic (when target-tag (get (:datomic-relationships target-entity) (:inverse-name relationship)))
                            g1 (symbol (str "?" (gensym)))
                            g2 (symbol (str "?" (gensym)))
                            g3 (symbol (str "?" (gensym)))]
                        [relevant-tags
                         (cond
                                        ; [= :fullName :NOUser.them/lastName]
                           (and attribute target-attribute)
                           `[[~target-symbol ~(t/datomic-name target-attribute) ~g1]
                             [~entity-symbol ~(t/datomic-name attribute) ~g2]
                             [(~op ~g1 ~g2)]]

                                        ; [= :fullName "marek"]
                           (and attribute (not (= target-tag :user))) `[[~entity-symbol ~(t/datomic-name attribute) ~target]]
                                        ; [= :client :NOClient.him]
                           target-relationship (if target-datomic ; ops do not matter for datmic - contains? and = are the same
                                                 `[[~target-symbol ~(t/datomic-name target-relationship) ~entity-symbol]]
                                                 `[[~entity-symbol ~(t/datomic-name relationship) ~target-symbol]])
                                        ; user
                           :else
                           (if (= op '=)
                             (if (= (:type attribute) :db.type/string)
                               `[[~target-symbol ~user-target ~g1]
                                 [[(~'.toString ~g1) ~g2]]
                                 [~entity-symbol ~(t/datomic-name attribute) ~g2]]
                               `[[~target-symbol ~user-target ~g1]
                                 [~entity-symbol ~(t/datomic-name attribute) ~g1]])
                             (if (= (:type attribute) :db.type/string)
                               `[[~target-symbol ~user-target ~g1]
                                 [[(~'.toString ~g1) ~g3]]
                                 [~entity-symbol ~(t/datomic-name attribute) ~g2]
                                 [(~op ~g2 ~g3)]]
                               `[[~target-symbol ~user-target ~g1]
                                 [~entity-symbol ~(t/datomic-name attribute) ~g2]
                                 [(~op ~g2 ~g1)]])))]))))))]
      (let [[dependencies rules] (where-for-rule rule true #{})
            additional-rules (reverse (dependent-rules dependencies))
            full-rules  (vec (reorder-rules (concat additional-rules rules) #{entity-symbol})) ;(concat additional-rules rules)
            ]
        [`[:find ~entity-symbol :in ~'$ [~'?user ...] :where ~@full-rules]
         rules
         dependencies]))))

(defn- scoping-step
  [
   entities-by-name
   applied-queries ; { tag query, ...}
   complete-tags ; #{ tag1 tag2 ... }
   remaining-edn-rules ; {tag1 [= :dada :NOUser.baba], ...}
   applied-wheres ; { tag {:dependencies #{:NOUser.me :NOMessage.sent} :rules [[?user :app/uuid ?m] [...] ]}}
   constants
   ]
;  (println "----------------")
  ;(println complete-tags)
 ; (pprint (vec (map #(vector % (vec (relevant-tags (% remaining-edn-rules)))) (keys remaining-edn-rules))))
  ;(println "---------------~~~-")
  ;(println (keys remaining-edn-rules))
  

  (if (empty? remaining-edn-rules)
    applied-queries
    (let [tag (first (filter #(subset? (relevant-tags (% remaining-edn-rules)) complete-tags) (keys remaining-edn-rules)))]
      (assert tag (str "Unable to find the next rule! Remaining rules: \n'" (with-out-str (pprint remaining-edn-rules)) "'*****, complete tags: \n'" (with-out-str (pprint complete-tags)) "'"))
      (let [[query rules dependencies] (query-for-rule
                                        entities-by-name
                                        tag
                                        (tag remaining-edn-rules)
                                        applied-wheres
                                        constants)]
        ; (println "Q---------------- QUERY " tag " ---------")
        ; (println query)

        (recur entities-by-name
               (assoc applied-queries tag query)
               (conj complete-tags tag)
               (dissoc remaining-edn-rules tag)
               (assoc applied-wheres tag {:dependencies dependencies :rules rules})
               constants)))))


; ========================================================================================
;                              INTERFACE
; ========================================================================================

(defn get-scoping-queries
  "Takes:
   entities-by-name,
   parsed (and validated) EDN of scoping rules,
   push?
   config map, with optional keys:
    - tags, a set of tags which queries are requested (if not provided all tags are taken by default)
    - necessary-tags, a set of tags which are required for scoping selected tags, calculated with tree obtained from scope_dependeny/get-minimum-scoping-sets
      if not provided queries for all tags will be calculated and result will be filtered according to tags param

   A typical invocation looks like this:
   (get-scoping-queries entities-by-name validated-scope push?)))
   (get-scoping-queries entities-by-name validated-scope push? {:tags #{:RAEmployee.ofOwner} :necessary-tags #{:RAEmployee.ofOwner :RAOwner.me}}})))
   Returns a map with tags and queries generated to obtain entities for indicated user:
  {:RAEmployee.ofOwner
  [:find
  ?RAEmployee-ofOwner
  :in
  $
  [?user ...]
  :where
  [?user :app/uuid ?G__33023]
  [(= ?G__33024 ?G__33023)]
  [?RAOwner-me :RAOwner/internalUserId ?G__33024]
  [?RAEmployee-ofOwner :RAEmployee/owner ?RAOwner-me]]}
   ...
  "
  ([entities-by-name scoping-definition push?]
   (get-scoping-queries entities-by-name scoping-definition push? nil))

  ([entities-by-name scoping-definition push? {:keys [tags necessary-tags]}]
   (let [constants (:constants scoping-definition)
         scoping-definition (dissoc scoping-definition :constants)
         tags (or tags (set (keys scoping-definition)))
         necessary-tags (or necessary-tags (set (keys scoping-definition)))
         relevant-rules (->> scoping-definition
                             (filter (fn [[tag description]]
                                       (and (:constraint description)
                                            (or (not push?)
                                                (-> description :permissions :create)
                                                (-> description :permissions :modify)
                                                (-> description :permissions :include-in-push))
                                            (contains? necessary-tags tag))))
                             (map (fn [[tag description]] [tag (:constraint description)]))
                             (into {}))
         queries (scoping-step
                  entities-by-name
                  {}
                  #{:user}
                  relevant-rules
                  {:user {:dependencies #{} :rules []}}
                  constants)
         filtered-queries (into {} (filter (fn [[tag _]] (contains? tags tag)) queries))]
     filtered-queries)))

(defn reverse-scoping-query
  "Input:
   Single scoping query (one value of map returned by get-scoping-queries)
   This query efficiently answers question \"As a user, which entities can I access?\".
   Example input:
   [:find ?RAEmployee-ofOwner
    :in $ [?user ...]
    :where
    [?user :app/uuid ?G__33024]
    [?RAOwner-me :RAOwner/internalUserId ?G__33024]
    [?RAEmployee-ofOwner :RAEmployee/owner ?RAOwner-me]]

   Output:
   Single scoping query.
   This query efficiently answers question \"Which users can access this entity?\".
   Example output:
   [:find ?user
    :in $ [?RAEmployee-ofOwner ...]
    :where
    [?RAEmployee-ofOwner :RAEmployee/owner ?RAOwner-me]
    [?RAOwner-me :RAOwner/internalUserId ?G__33024]
    [?user :app/uuid ?G__33024]]"
  [scoping-rule]
  (let [[find entity-variable
         in $ [user-variable dots]
         where & query-body] scoping-rule
        new-rule-header [find user-variable in $ [entity-variable dots] where]

        reverse-clauses (fn reverse-clauses [clauses]
                          (if (= 1 (count clauses))
                            (let [clause (first clauses)]
                              [(if (vector? clause)
                                 clause
                                 (let [sym (first clause)]
                                   (case sym
                                     (not or and) (concat [sym] (mapcat #(reverse-clauses [%]) (reverse (rest clause))))
                                     (not-join or-join) (concat [sym (second clause)] ;; variables bound by join
                                                                (mapcat #(reverse-clauses [%]) (reverse (drop 2 clause)))))))])
                            (reverse (mapcat #(reverse-clauses [%]) clauses))))

        bound-variable (fn bound-variable [clauses variable]
                         (if (= 1 (count clauses))
                           (let [clause (first clauses)]
                             (if (vector? clause)
                               (some #(= variable %) clause)
                               (let [sym (first clause)]
                                 (case sym
                                   (not-join or-join) (some #(= variable %) (second clause))
                                   or (every? #(bound-variable [%] variable) (rest clause))
                                   and (some #(bound-variable [%] variable) (rest clause))
                                   false))))
                           (some #(bound-variable [%] variable) clauses)))

        universal-binding (fn [sym]
                            [sym (if (= sym user-variable)
                                   :user/username
                                   (-> sym str (str/split #"-") first (subs 1) (keyword "ident__")))])

        reversed-query-body
        (condp = query-body
          [(universal-binding entity-variable)] [(universal-binding user-variable)]
          (none-rules user-variable entity-variable) (none-rules entity-variable user-variable)
          (letfn [(validate-clause
                    [clause variable]
                    (if (vector? clause)
                      [clause]
                      (fix-variable-binding variable false [clause])))
                  (fix-variable-binding
                    [variable requires-and clauses]
                    (if (= 1 (count clauses))
                      (let [clause (first clauses)]
                        (if (vector? clause)
                          (if (bound-variable clauses variable)
                            clauses
                            (if requires-and
                              [(list 'and clause (universal-binding variable))]
                              [clause (universal-binding variable)]))
                          (let [sym (first clause)]
                            [(case sym
                               not-join (concat (take 2 clause) (fix-variable-binding variable false (drop 2 clause)))
                               or-join (let [bindings (second clause)
                                             bindings (if (seq (filter #(= % variable) bindings))
                                                        bindings
                                                        (conj bindings variable))]
                                         (concat [sym bindings]
                                                 (reduce (fn [clauses variable]
                                                           (mapcat #(fix-variable-binding variable true [%]) clauses))
                                                         (drop 2 clause)
                                                         bindings)))
                               or (concat [sym] (mapcat #(fix-variable-binding variable true [%]) clauses))
                               and (concat [sym] (fix-variable-binding variable false (rest clause)))
                               not clause)])))
                      (let [clauses (mapcat #(validate-clause % variable) clauses)]
                        (if (bound-variable clauses variable)
                          clauses
                          (concat clauses [(universal-binding variable)])))))]
            (->> query-body
                 reverse-clauses
                 (fix-variable-binding user-variable false))))]
    (into new-rule-header reversed-query-body)))

(defn scope-selected-tags-with-tree
  "Takes:
   a snapshot,
   a user object from DB,
   entities-by-name,
   parsed (and validated) EDN of scoping rules,
   scoping-sets, a map of sets indiacting which tags must be scoped per tag calculated with scope_dependency/get-minimum-scoping-sets
   tags, set of desired tags.

   It is advised to calculacte scoping sets once and pass the result.
   A typical invocation looks like this:
   (scope-selected-tags-with-tree config (d/db db/conn) user entities-by-name validated-scope scoping-sets #{:RARestaurant.ofOwner})))

   Returns a map with db ids, something like:
   {:RAOwner.me #{11122, 1222} :user #{2312312}}
  "
  [config snapshot user entities-by-name scoping-definition scoping-sets tags]
  (let [necessary-tags (->> tags
                            (map (fn [tag]
                                   (let [all-tags (keys (dissoc scoping-definition :constants))]
                                     (->> all-tags
                                          (filter #(contains? (tag scoping-sets) %) )
                                          set))))
                            (reduce clojure.set/union))
        queries (get-scoping-queries entities-by-name scoping-definition false {:tags tags :necessary-tags necessary-tags})
        completed-queries (->> queries
                               (map (fn [[tag query]] [tag [query snapshot #{(:db/id user)}]]))
                               (into {}))]
    (->> completed-queries
         (pmap (fn [x] (apply (partial execute-query config) x)))
         (into {}))))

(defn scope
  "Takes config map, a snapshot, a user object from DB, entities-by-name and the parsed EDN of rules, push? and optional set of tags to scope.
   If set of tags to scope is not provided all tags are scoped.
   A typical invocation looks like this:
  (scope config (d/db db/conn) user entities-by-name (clojure.edn/read-string (slurp \"resources/model/pull-scope.edn\")) false)
  (scope config (d/db db/conn) user entities-by-name (clojure.edn/read-string (slurp \"resources/model/pull-scope.edn\")) false #{:NOUser.me :NOLanguage.mine})

  Returns a map with db ids, something like:
  {:NOUser.me #{11122, 1222} :user #{2312312}}
  "
  ([config snapshot user entities-by-name scoping-definition push?]
   (scope config snapshot user entities-by-name scoping-definition push? (set (keys (dissoc scoping-definition :constants)))))
    
  ([config snapshot user entities-by-name scoping-definition push? tags]
   (let [queries (get-scoping-queries entities-by-name scoping-definition push? {:tags tags :necessary-tags tags})
        completed-queries (->> queries
                               (map (fn [[tag query]] [tag [query snapshot #{(:db/id user)}]]))
                               (into {}))
        filtered-queries (into {} (filter (fn [[tag _]] (contains? tags tag)) completed-queries))]
    (->> filtered-queries
         (pmap (fn [x] (apply (partial execute-query config) x)))
         (into {})))))

(defn reduce-entities
  "Takes what 'scope' produces and aggregates all the entity types (so :NOUser.me and :NOUser.peer become :NOUser with unified ids)"
  [scoped-entities]
  (into {} (map (fn [[k l]] [k (reduce union (map second l))])
                (group-by (fn [[k v]] (keyword (first (str/split (name k) #"\."))))
                          (vec scoped-entities)))))

(defn validate-pull-scope [entities-by-name edn]
  ; This is a horrible way to validate grammar, but must suffice for now. Do not read or modify code below - it just works.
  (spec/specs-for-entities entities-by-name {})
  (letfn [(constant? [o] (and (keyword? o) (= "constant" (namespace o))))
          
          (validate-constraint [constraint entity tag all-tags]
            (if (#{:all :none} constraint)
              ; we add a special 'all' and 'none' constraints
              true
              (if (vector? constraint)
                (let [allowed-ops #{'= '> '< '>= '<= 'contains? 'allowed-if}]                  
                  (assert (allowed-ops (first constraint)) (str "We only support the following ops: " allowed-ops " in constraint: " constraint))
                  (if (= (first constraint) 'allowed-if)
                    (let [_ (assert (= 2 (count constraint)) (str "This constraint needs to have two elements: " constraint))
                          [op constant] constraint]
                      (assert (constant? constant) (str "The constraint has to have a constant keyword expression as its argument: " constraint)))                  
                    (let [_ (assert (= 3 (count constraint)) (str "This constraint needs to have three elements: " constraint))
                          [op arg1 arg2] constraint
                          attribute (get (:attributes entity) (name arg1))
                          relationship (get (:relationships entity) (name arg1))]
                      (assert (or attribute relationship) (str "The first argument of a constraint needs to be a local field for entity " (:name entity) " but it is not: " arg1))
                      (when attribute
                        (let [permitted-ops (cond
                                              (#{:db.type/instant :db.type/long :db.type/double :db.type/float} (:type attribute)) #{'= '> '< '>= '<=}
                                              (#{:db.type/boolean :db.type/uuid :db.type/uri} (:type attribute))  #{'=}
                                              (#{:db.type/string} (:type attribute))  #{'= 'contains?}
                                              :else #{})]
                          (if (or (not (keyword? arg2))
                                  (constant? arg2)) (validate-attribute tag entity (:name attribute) arg2)
                              (let [rf (remote-field? arg2 all-tags)]
                                        ; rf is attribute, relationship or true (special :user tag)
                                (assert rf (str "The second argument of a constraint needs to be a constant or a reference to a matching type tag. See " (:name entity) " value: " arg2))
                                (assert (or (= true rf) (= (:type rf) (:type attribute))) (str "The fields are not of teh same type: " arg1 ": " (:type attribute) ", " arg2 ": " (:type rf)))))
                          (assert (permitted-ops op) (str "The operation is not permitted: " op " for " (:name attribute) " on " (:name entity)))))
                      
                      (when relationship
                        (let [permitted-ops #{'= 'contains?}
                              rf (correct-relationship? relationship arg2 all-tags)]
                          
                          (assert (permitted-ops op) (str "The operation is not permitted: " op " for " (:name attribute) " on " (:name entity)))
                          (assert rf (str "The second argument of a constraint has to be a matching type tag. See " (:name entity) " value: " arg2 ": " (:name relationship))))))))
                ;; not, or, and
                (let [op (first constraint)]
                  (assert (list? constraint) (str "The constraint needs to either be a list or a vector: " constraint))
                  (cond (= op 'not) (do
                                      (assert (= 2 (count constraint)) (str "NOT op can have one argument: " constraint))
                                      (validate-constraint (second constraint) entity tag all-tags))
                        (#{'or 'and} op) (do
                                           (doseq [c (rest constraint)] (validate-constraint c entity tag all-tags))
                                           (assert (> (count (rest constraint)) 1) (str "The 'and' and 'or' constraints need at least two arguments: " constraint)))
                        :else (assert false (str "Unknown logical op: " constraint)))))))

          (correct-tag? [tag all-tags]
            (let [parts (str/split (name tag) #"\.")]
              (and
               (all-tags tag)
               (not (namespace tag))
               (or (= (count parts) 2)
                   (and
                    (= (first parts) "user")
                    (and (keyword? tag) (= (name tag) "uuid"))
                    (= (count parts) 1))))))

          (remote-field? [tag all-tags]
            (let [parts (str/split (or (namespace tag) "") #"\.")]
              (when (or (= (count parts) 2)
                        (and
                         (= (first parts) "user")
                         (and (keyword? tag) (= (name tag) "uuid"))
                         (= (count parts) 1)))
                (let [entity (get entities-by-name (first parts))
                      attribute (get (:attributes entity) (name tag))]
                  (or (and (all-tags (keyword (namespace tag)))
                           attribute) (= (first parts) "user"))))))

          (correct-relationship? [rel tag all-tags]
            (and (correct-tag? tag all-tags)
                 (let [parts (str/split (or (name tag) "") #"\.")]
                   (when (or (= (count parts) 2)
                             (and
                              (= (first parts) "user")
                              (and (keyword? tag) (= (name tag) "uuid"))
                              (= (count parts) 1)))
                     (let [entity (get entities-by-name (first parts))
                           relationship (first (filter #(= (:inverse-name %) (:name rel)) (vals (:relationships entity))))]
                       (when (or (and entity relationship) (= (first parts) "user"))
                         (or relationship true)))))))

          (validate-replacement [tag entity attribute-name replacement-attribute-name]
            (let [attribute (get (:attributes entity) attribute-name)
                  replacement-attribute (get (:attributes entity) (if (keyword? replacement-attribute-name)
                                                                    (name (replacement-attribute-name))
                                                                    replacement-attribute-name))]
              (assert (and attribute replacement-attribute) (str "One of attributes: #{" attribute-name ", " replacement-attribute-name "} not found on entity " (:name entity)))
              (assert (= (:type attribute) (:type replacement-attribute)) (str "Types do not match for replacement: " attribute-name " with " replacement-attribute-name))
              (assert (not (and (:optional replacement-attribute) 
                                (not (:optional attribute)))) (str "Non-optional attribute " attribute-name " cannot be replaced by optional " replacement-attribute-name))
              (assert (= (str (:regular-expression attribute))                         
                         (str (:regular-expression replacement-attribute))) (str "Different regex validation for " attribute-name " and " replacement-attribute-name))
              (assert (= (:min-value attribute) 
                         (:min-value replacement-attribute)) (str "Different min-length validation for " attribute-name " and " replacement-attribute-name))
              (assert (= (:max-value attribute) 
                         (:max-value replacement-attribute)) (str "Different max-length validation for " attribute-name " and " replacement-attribute-name))))

          (validate-attribute [tag entity attribute-name value]
            (let [attribute (get (:attributes entity) attribute-name)
                  parsed (when (not (constant? value)) 
                           (try 
                             ((t/parser-for-attribute attribute) value)
                             (catch Exception _ (assert false (str "Wrong value: " value " for attribute: " attribute-name)))))]
              (assert attribute (str "Entity: " tag " doesn't have the attribute: " attribute-name))
              (assert (or (:optional attribute) (not (nil? value))) (str "Non-optional attribute " (:name attribute) " has nil value for tag: " tag))
              (when-not (constant? value)                
                  (assert (or (and (nil? value) (nil? parsed)) (and (not (nil? value)) (not (nil? parsed)))) (str "Unable to parse attribute " (:name attribute) " with value: " value " for tag: " tag))
                  (assert (s/valid? (t/datomic-name attribute) parsed) (s/explain-str (t/datomic-name attribute) parsed)))))
            
          (validate-permission [tag entity permission]
            (assert (map? permission) (str "Permissions need to be a map for " tag ", " permission))
            (assert (subset? (set (keys permission)) #{:modify :create :protected-fields :writable-fields}) (str "Permission for " tag " cannot have fields: " (difference (set (keys permission)) #{:modify :create :protected-fields :writable-fields})))
            (let [{:keys [modify create protected-fields writable-fields]
                   :or {modify false
                        create false
                        protected-fields []
                        writable-fields []}} permission]
              (assert (or (boolean? modify) (constant? modify)) (str "'modify' field in permissions for " tag " needs to be a boolean or constant reference"))
              (assert (or (boolean? create) (constant? create)) (str "'create' field in permissions for " tag " needs to be a boolean or constant reference"))
              (assert (vector? protected-fields) (str "'protected-fields' field in permissions for " tag " needs to be a vector"))
              (assert (vector? writable-fields) (str "'writable-fields' field in permissions for " tag " needs to be a vector"))
              (assert (empty? (intersection (set protected-fields) (set writable-fields))) (str "'protected-fields' and 'writable-fields' for tag " tag " cannot have fields that are common: " (intersection (set protected-fields) (set writable-fields))))
              (doseq [field (concat protected-fields writable-fields)]
                (let [attribute (get (:attributes entity) field)
                      relationship (get (:relationships entity) field)]
                  (assert (or attribute 
                              relationship 
                              (constant? field)) 
                              (str "There is no such field or constant as '" field "' in 'writable-fields' or 'protected-fields' for " tag))))))]

    (let [get-entity #(first (str/split (name %) #"\."))

          all-entities (set (keys entities-by-name))
          split-edn (map #(str/split (name %) #"\.") (keys edn))
          entities-from-edn (set (map first split-edn))

          all-tags (set (keys edn))

          wrong-length-tags (filter #(not (correct-tag? % all-tags)) all-tags)]
      (assert (subset? entities-from-edn all-entities) (str "Unknown entities: " (difference entities-from-edn all-entities)))
      (assert (empty? wrong-length-tags) (str "Wrong tags: " (reduce #(str %1 ", " %2) wrong-length-tags)))

      (doseq [[tag description] edn]
        (let [entity (get entities-by-name (get-entity tag))]
          (if (= tag :user) (assert (empty? description) ":user must have an empty description")
              (do
                (assert entity (str "There is no entity: " tag))
                (assert (subset? (set (keys description)) #{:constraint :replace-fields :permissions}) (str "Tag " tag " has wrong description - unknown keys: " (difference (set (keys description)) #{:constraint :replace-fields :permissions})))
                                        ; fields

                (doseq [[attribute-name replacement] (:replace-fields description)]
                  (cond (and (keyword? replacement)
                             (not (namespace replacement)))
                        (validate-replacement tag entity (name attribute-name) (name replacement))
                        (constant? replacement) true
                        :default (validate-attribute tag entity (name attribute-name) replacement)))

                (validate-permission tag entity (:permissions description))

                (when-let [constraint (:constraint description)]
                  (assert (not ((relevant-tags constraint) tag)) (str "Self-referencing constraints are prohibited: " tag (relevant-tags constraint)))
                  (validate-constraint constraint entity tag all-tags)))))))))

(defn- referenced-tags-from-constraint [{:keys [constraint]}]
  (relevant-tags constraint))

(defn- changeable? [{:keys [permissions]}]
  (or (:modify permissions)
      (:create permissions)))

(defn- referenced-by-changeable-entity? [edn selected-tag]
  (let [tags-with-reference-to-selected (->> edn
                                             (map (fn [[tag description]]
                                                    [tag (referenced-tags-from-constraint description)]))
                                             (filter (fn [[tag referenced-tags]]
                                                       (contains? (set referenced-tags) selected-tag)))
                                             (map first))]
    (if (empty? tags-with-reference-to-selected)
      false
      (or
       (some (fn [tag] (changeable? (get edn tag))) tags-with-reference-to-selected)
       (some identity (map (partial referenced-by-changeable-entity? edn) tags-with-reference-to-selected))))))

(defn add-include-in-push-permission [edn]
  (let [all-constraints-tags (->> edn
                                  (mapcat (fn [[tag description]]
                                            (referenced-tags-from-constraint description)))
                                  (into #{}))
        referenced-unchangeable-tags (->> edn
                                         (filter (fn [[tag description]]
                                                   (not (changeable? description))))
                                         (filter (fn [[tag _]]
                                                   (contains? all-constraints-tags tag)))
                                         (filter (fn [[tag _]]
                                                   (referenced-by-changeable-entity? edn tag)))
                                         (map first))
        include-in-push (fn [edn tag] (assoc-in edn [tag :permissions :include-in-push] true))]
    (reduce include-in-push edn referenced-unchangeable-tags)))

(defn read-pull-scoping-edn [path entities-by-name]
  (let [edn (clojure.edn/read-string (slurp path))]
    (validate-pull-scope entities-by-name edn)
    (add-include-in-push-permission edn)))
