(ns lambdaconnect-model.scope-single-tag
  (:require [clojure.edn] 
            [clojure.set :as set]))

(defn- get-parents
  "returns list of parent tags given the list of contraints
   element is a current clause or operator"
  ([constraints]
   (if (vector? constraints)
     (get-parents constraints [] #{} false)
     (get-parents constraints (rest constraints) #{} false)))
  ([cur-element constraints-left parent-set root?] 
   (if-not cur-element
     [parent-set root?]
     (cond
       (vector? cur-element)
       (let [am-i-root? (if root? root? (= :user/uuid (last cur-element)))]
         [(if am-i-root? parent-set (conj parent-set (last cur-element))) am-i-root?])

       (not (list? cur-element)) (throw (Exception. (str "Encountered unexpected cur-element " cur-element)))

       (= 'not (first cur-element))
       (recur (first constraints-left)
              (rest constraints-left)
              parent-set
              root?)

       (contains? #{'or 'and} (first cur-element))
       (let [parent-sets-list (for [element (rest cur-element)]
                                (cond
                                  (vector? element) [#{(last element)} (if root? root? (= :user/uuid (last cur-element)))]
                                  (list? element) (get-parents element)
                                  :else (throw (Exception. (str "Encountered unexpected element" element)))))]
         (reduce (fn [[cur-parents cur-root?] [parents-remaining is-root?]]
                   [(clojure.set/union cur-parents parents-remaining) (or cur-root? is-root?)])
                 parent-sets-list))

       :else (throw (Exception. (str "Encountered unexpected cur-element" cur-element)))))))

(defn- build-dependency-tree
  "Reads scoping and builds tree which describes dependency between tags
   each edge represents a dependency: 
   if tag B has ingoing edge from tag A it means that tag A is present in tag's B contraint
   if tag A has outgoing edge to B it means that tag B has tag A in its contraints
   edges are represent as map of sets: tag #{set of in/out going edges}, roots are stored in set
   returns a tuple [ingoding edges, outgoing edges, roots (where root is a tag with :user/uuid in its contraints)]" 
  [scoping]
  (let [tags (keys scoping)
        empty-edge-map (->> tags
                            (map (fn [tag] [tag #{}]))
                            (into {})) 
        dependency-tree (reduce (fn [[in out roots] [tag attrs]]
                                      (let [[tag-parents root?] (get-parents (:constraint attrs))
                                            updated-in (update in tag into tag-parents)
                                            updated-out (reduce (fn [out parent-tag]
                                                                  (update out parent-tag conj tag))
                                                                out tag-parents)
                                            updated-roots (if root? (conj roots tag) roots)]
                                        [updated-in updated-out updated-roots]))
                                    [empty-edge-map empty-edge-map #{}] scoping)]
    dependency-tree))

(defn- DFS
  [cur-tag out visited call-stack] 
  (let [cur-visited (conj visited cur-tag)
        cur-call-stack (conj call-stack cur-tag)
        children-paths (for [child (cur-tag out)] 
                         (if (not (contains? cur-visited child))
                           (DFS child out cur-visited cur-call-stack)
                           (when (contains? call-stack child)
                             (throw (Exception. (str "Cycle deteced, back edge from: " cur-tag " to: " child))))))] 
    (reduce (fn [cur-paths new-path] (merge-with clojure.set/union cur-paths new-path))
            {cur-tag call-stack} children-paths)))

(defn get-minimum-scoping-sets
  "Given a validated scoping
   returns map of sets which has tags as key and each set keeps
   tags required for scoping that tag"
  [validated-scoping]
  (let [[in out roots] (build-dependency-tree validated-scoping)
        paths (for [root roots]
                (DFS root out #{} #{}))]
    (reduce (fn [cur-paths new-path] (merge cur-paths new-path))
            paths)))

