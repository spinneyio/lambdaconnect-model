(ns lambdaconnect-model.macro)

(defmacro defspec [k spec-form]
  `(cljs.spec.alpha/def-impl ~k (quote ~spec-form) ~spec-form))
