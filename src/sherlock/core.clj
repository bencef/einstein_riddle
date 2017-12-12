(ns sherlock.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd])
  (:require [clojure.set :refer [difference]]))

(defn houseo [h]
  (fresh [number color occupant pet drink cigarette]
    (== h {:number number
           :color color
           :occupant occupant
           :pet pet
           :drink drink
           :cigarette cigarette})))

(defmacro partiale [q pmap]
  (let [x (gensym)
        fields #{:number :color :occupant :pet :drink :cigarette}
        fresh-needed (- (count fields) (count pmap))
        lvars (repeatedly fresh-needed gensym)
        rest-of-keys (difference
                      fields
                      (keys pmap))]
    `(fresh [~x ~@lvars]
       (membero ~x ~q)
       (project [~x]
                (== ~x
                    ~(apply conj pmap
                            (mapv #(into [] %)
                                  (partition 2
                                             (interleave rest-of-keys lvars)))))))))

(run 1 [q]
  (fresh [h1 h2 h3 h4 h5]
    (== q [h1 h2 h3 h4 h5])
    (everyg houseo q)
    ;; TODO house numbers are distinct
    (partiale q {:occupant :brit :color :red})
    (partiale q {:occupant :swede :pet :dog})
    (partiale q {:occupant :dane :drink :tea})
    (fresh [n1 n2]
      (fd/+ 1 n1 n2)
      (everyg #(fd/in % (fd/interval 1 5)) [n1 n2])
      (partiale q {:number n1 :color :green})
      (partiale q {:number n2 :color :white}))
    (partiale q {:color :green :drink :coffee})
    (partiale q {:cigarette :pall-mall :pet :bird})
    (partiale q {:color :yellow :cigarette :dunhill})
    (partiale q {:number 3 :drink :milk})
    (partiale q {:occupant :norwegian :number 1})))
