(ns repl.matcher-combinators
  (:require [matcher-combinators.core :as c]
            [matcher-combinators.matchers :as m]))

(c/match?
 (c/match (c/->EqualsMap {:foo {:expected 1, :actual 2}}) {:foo {:expected 1, :actual 2}})

 )
(c/match (c/->EqualsMap {:foo 1}) {:foo 2})
[:mismatch {:foo #matcher_combinators.model.Mismatch{:expected 1, :actual 2}}]

(c/match (c/->EqualsMap {:foo {:expected 1, :actual 2}}) {:foo {:expected 1, :actual 2}})
[:match {:foo {:expected 1, :actual 2}}]

(c/match (c/->EqualsMap {:foo (java.lang.Integer. 1)}) {:foo 2})
