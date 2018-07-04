(ns kaocha.util-test
  (:require [clojure.test :refer :all]
            [kaocha.util :as util]))

(deftest namespace-keys-test
  (are [res n m] (is (= res (util/namespace-keys n m)))
    {}                      :foo {}
    {:foo/bar 1}            :foo {:bar 1}
    {:foo/bar 1 :bar/baz 2} :foo {:bar 1 :bar/baz 2}
    {"s" 1}                 :foo {"s" 1}))
