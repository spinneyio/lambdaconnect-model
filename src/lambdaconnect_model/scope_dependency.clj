(ns lambdaconnect-model.scope-dependency
  (:require [clojure.edn] 
            [clojure.set :as set]
            [lambdaconnect-model.utils :refer [relevant-tags]]))

(defn- DFS
  [cur-tag out visited call-stack] 
  (let [cur-visited (conj visited cur-tag)
        cur-call-stack (conj call-stack cur-tag)
        children-paths (for [child (cur-tag out)] 
                         (if (not (contains? cur-visited child))
                           (DFS child out cur-visited cur-call-stack)
                           (when (contains? call-stack child)
                             (throw (Exception. (str "Cycle deteced, back edge from: " cur-tag " to: " child))))))] 
    (reduce (fn [cur-paths new-path] (merge-with set/union cur-paths new-path))
            {cur-tag call-stack} children-paths)))

(defn build-dependency-tree
  "Reads scoping and builds tree which describes dependency between tags
   each edge represents a dependency: 
   if tag B has ingoing edge from tag A it means that tag A is present in tag's B contraint
   if tag A has outgoing edge to B it means that tag B has tag A in its contraints
   {tagB {:constraints [= :field tagA]}} <=> A -> B
   edges are represent as map of sets: tag #{set of in/out going edges}, roots are stored in set
   returns a tuple [ingoing edges, outgoing edges, roots (where root is a tag with :user/uuid in its contraints)]" 
  [scoping]
  (let [root-tag :user
        tags (conj (keys scoping) root-tag)
        empty-edge-map (->> tags
                            (map (fn [tag] [tag #{}]))
                            (into {})) 
        [ingoing outgoing] (reduce (fn [[in out] [tag attrs]]
                                  (let [tag-parents (relevant-tags (:constraint attrs)) 
                                        updated-in (update in tag into tag-parents)
                                        updated-out (reduce (fn [out parent-tag]
                                                              (update out parent-tag conj tag))
                                                            out tag-parents)]
                                    [updated-in updated-out ]))
                                [empty-edge-map empty-edge-map #{}] scoping)]
    [ingoing outgoing (root-tag outgoing)]))

(defn get-minimum-scoping-sets
  "Given a scoping
   returns map of sets which has tags as key and each set keeps
   tags required for scoping that tag"
  [scoping]
  (let [[in out roots] (build-dependency-tree scoping)
        paths (for [root roots]
                (DFS root out #{} #{}))
        conjoined-paths (->> paths
                             (reduce (fn [cur-paths new-path] (merge cur-paths new-path)))
                             (map (fn [[tag path]] [tag (conj path tag)]))
                             (into {}))
        ;DFS skips those tags as they are neither root nor they have any edges in (so they are detached from the tree), 
        ;only known case when such tag can occur is when tag uses :all contraint
        missing-tags (set/difference (set (keys scoping)) (set (keys conjoined-paths)))
        completed-paths (reduce (fn [scoping-paths missing-tag]
                                  (assert (= :all (get-in scoping [missing-tag :constraint]))
                                          (str "tag different than :all contrained tag was missed (scoping tree is not connected?)"))
                                  (assoc scoping-paths missing-tag #{missing-tag}))
                                conjoined-paths missing-tags)]
    (assert (= (count (keys scoping)) (count (keys completed-paths)))
            (str "tags: " (set/difference #{(keys scoping)} #{(keys conjoined-paths)}) " do not have a path!"))
    completed-paths))