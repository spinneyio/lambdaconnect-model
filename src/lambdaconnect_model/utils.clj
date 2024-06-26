(ns lambdaconnect-model.utils
  (:require [clojure.set :refer [union]]))

(defmacro functionise [macro]
  `(fn [& args#] (eval (cons '~macro args#))))

(defn mapcat
  ; We need our own implementation, see http://clojurian.blogspot.com/2012/11/beware-of-mapcat.html
  ([f coll] (lambdaconnect-model.utils/mapcat f coll (lazy-seq [])))
  ([f coll acc]
   (if (empty? coll) acc
       (recur f (rest coll) (lazy-seq (concat acc (f (first coll))))))))

(defn update-vals
  "m f => {k (f k v) ...}

  Given a map m and a function f of 2-arguments, returns a new map where the keys of m
  are mapped to result of applying f to the corresponding keys-values of m.
  
  A variation of clojure's update-vals that passes the key to the function as well.
  "
  [m f]
  (with-meta
    (persistent!
     (reduce-kv (fn [acc k v] (assoc! acc k (f k v)))
                (if (instance? clojure.lang.IEditableCollection m)
                  (transient m)
                  (transient {}))
                m))
    (meta m)))

(defn merge 
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result.
  
  Uses transients for speed (up to 2x speedup vs original map)"
  [& maps]
  (let [maps (keep identity maps)]
    (when (seq maps)
      (persistent! 
       (reduce (fn [acc m]   
                 (reduce-kv (fn [acc k v] 
                              (assoc! acc k v)) 
                            acc m)) 
               (transient (first maps))
               (rest maps))))))

(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter).
  
  Uses transients for speed (up to 1.5x speedup vs original map-with for large maps)"
  [f & maps]
  (let [maps (keep identity maps)]
    (when (seq maps)
      (persistent! 
       (reduce (fn [acc m]   
                 (reduce-kv (fn [acc k v] 
                              (assoc! acc k 
                                      (if (contains? acc k) 
                                        (f (get acc k) v) 
                                        v))) 
                            acc m)) 
               (transient (first maps))
               (rest maps))))))

(defn rebuild-map
  "Given a map m and a function f of 2 arguments (key, value), 
  Returns new map where entries are build from two-element vectors 
  [k v] f returns. If f returns nil the pair is skipped."
  [m f]
  (persistent!
   (reduce-kv (fn [acc k v]
                (if-let [ret (f k v)]
                  (let [[rk rv] ret]
                    (assoc! acc rk rv))
                  acc))
              (transient {}) m)))

