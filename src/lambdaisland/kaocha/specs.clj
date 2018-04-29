(ns lambdaisland.kaocha.specs
  (:require [clojure.spec.alpha :as s]
            [lambdaisland.kaocha :as k]))


(s/def ::k/config (s/keys :opt-un [::k/reporter ::k/color ::k/suites]))
(s/def ::k/suites (s/coll-of ::k/suite))
(s/def ::k/suite (s/keys :opt-un [::k/id ::k/test-paths ::k/ns-patterns]))

(s/valid? ::k/suite [{}])
