(ns lambdaisland.kaocha.random)

(defn rng [seed]
  (let [rng (java.util.Random. seed)]
    (fn [& _] (.nextInt rng))))

(defn randomize-tests [seed nss]
  (let [next-int (rng seed)
        nss' (->> nss
                  (map #(update % :tests (partial sort-by (comp str :var))))
                  (sort-by (comp str :ns)))]
    (->> nss'
         (map #(update % :tests (partial sort-by next-int)))
         (sort-by next-int))))
