(ns co.fwoar.logging-utils)

(defmulti logging-multi first)
(defmethod logging-multi 'and [forms]
  (let [assertions (rest forms)
        assertions' (map (fn [assn]
                           `(or ~assn
                                (do (print ~(str assn) " ")
                                    (pp/pprint ~assn))))
                         assertions)]
    `(and ~@assertions')))

(defmacro logging [form]
  (logging-multi form))
