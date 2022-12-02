(ns lambdaconnect-model.utils
  (:require [clojure.set :refer [union]]))

(defn relevant-tags
  "Takes a rule (e.g. [= :client :NOClient.me]) and 
   returns a set of all the tags included in the rule (in this case #{:NOClient.me}

  Other example: (or (not [= :client :NOClient.me]) [= :uuid :NOClient.they/uuid]) -> #{:NOClient.me :NOClient.they}"
  [rule] ; a single top-level rule taken from edn 
  (if (list? rule)
    (let [op (first rule)]
      (case op
        'not (relevant-tags (second rule))
        (reduce union #{} (map relevant-tags (rest rule)))))
    (if-let [tag (when-not (= rule :all) (last rule))]
      (if-not (keyword? tag)
        #{}
        (if-not (namespace tag)
          #{tag}
          #{(keyword (namespace tag))}))
      #{})))