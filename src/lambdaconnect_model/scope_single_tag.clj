(ns lambdaconnect-model.scope-single-tag
  (:require [clojure.edn] 
            [clojure.set :as set]
            [lambdaconnect-model.utils :refer [relevant-tags]]))

(defn- build-dependency-tree
  "Reads scoping and builds tree which describes dependency between tags
   each edge represents a dependency: 
   if tag B has ingoing edge from tag A it means that tag A is present in tag's B contraint
   if tag A has outgoing edge to B it means that tag B has tag A in its contraints
   edges are represent as map of sets: tag #{set of in/out going edges}, roots are stored in set
   returns a tuple [ingoing edges, outgoing edges, roots (where root is a tag with :user/uuid in its contraints)]" 
  [scoping]
  (let [root-tag :user
        tags (keys scoping)
        empty-edge-map (->> tags
                            (map (fn [tag] [tag #{}]))
                            (into {})) 
        dependency-tree (reduce (fn [[in out roots] [tag attrs]]
                                  (let [tag-parents (relevant-tags (:constraint attrs))
                                        root? (contains? tag-parents root-tag)
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
    (reduce (fn [cur-paths new-path] (merge-with set/union cur-paths new-path))
            {cur-tag call-stack} children-paths)))

(defn get-minimum-scoping-sets
  "Given a validated scoping
   returns map of sets which has tags as key and each set keeps
   tags required for scoping that tag"
  [validated-scoping]
  (let [[in out roots] (build-dependency-tree validated-scoping)
        paths (for [root roots]
                (DFS root out #{} #{}))
        conjoined-paths (->> paths
                             (reduce (fn [cur-paths new-path] (merge cur-paths new-path)))
                             (map (fn [[tag path]] [tag (conj path tag)]))
                             (into {}))
        ;DFS skips those tags as they are neither root nor they have any edges in (so they are detached from the tree)
        missing-tags (set/difference (set (keys validated-scoping)) (set (keys conjoined-paths)))
        completed-paths (reduce (fn [scoping-paths missing-tag]
                                  (assert (= :all (get-in validated-scoping [missing-tag :constraint]))
                                          (str "tag different than :all contrained tag was missed (scoping tree is not connected?)"))
                                  (assoc scoping-paths missing-tag #{missing-tag}))
                                conjoined-paths missing-tags)]
    (assert (= (count (keys validated-scoping)) (count (keys completed-paths)))
            (str "tags: " (set/difference #{(keys validated-scoping)} #{(keys conjoined-paths)}) " do not have a path!"))
    completed-paths))