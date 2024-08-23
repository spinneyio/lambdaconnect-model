(ns lambdaconnect-model.macro)

(defmacro defspec [k spec-form]
  `(cljs.spec.alpha/def-impl ~k (quote ~spec-form) ~spec-form))

(defmacro future
  "Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?."
  {:added "1.1"}
  [& body] `(atom ((^{:once true} fn* [] ~@body))))

