(ns repl.random)

(.nextInt
 (java.util.Random. 1))

(defn rand-ints* [rng]
  (cons (.nextInt rng)
        (lazy-seq (rand-ints* rng))))

(defn rand-ints [seed]
  (rand-ints* (java.util.Random. seed)))
