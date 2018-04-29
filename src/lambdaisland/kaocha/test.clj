(ns lambdaisland.kaocha.test
  (:require [clojure.test :as t]
            [lambdaisland.kaocha.report :as report]
            [lambdaisland.kaocha.load :as load]
            [lambdaisland.kaocha.output :as output]
            [lambdaisland.kaocha.random :as random]
            [lambdaisland.kaocha.config :as config]
            [slingshot.slingshot :refer [try+ throw+]]
            [lambdaisland.kaocha :as k]
            [clojure.string :as str]
            [lambdaisland.kaocha.output :as out]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure.test functions

(defn- stacktrace-file-and-line [stacktrace]
  (if (seq stacktrace)
    (let [^StackTraceElement s (first stacktrace)]
      {:file (.getFileName s) :line (.getLineNumber s)})
    {:file nil :line nil}))

;; This is an unfortunate hack. clojure.test/is wraps all assertions in a
;; try/catch, but we actually want to use an exception to signal a failure when
;; using --fail-fast, so we can skip the rest of the assertions in the var. This
;; detects our own fail-fast exception, and rethrows it, rather than reporting
;; it as an error.
(alter-var-root #'clojure.test/do-report
                (fn [_]
                  (fn [m]
                    (t/report
                     (case (:type m)
                       :fail
                       (merge (stacktrace-file-and-line (drop-while
                                                         #(let [cl-name (.getClassName ^StackTraceElement %)]
                                                            (or (str/starts-with? cl-name "java.lang.")
                                                                (str/starts-with? cl-name "clojure.test$")
                                                                (str/starts-with? cl-name "lambdaisland.kaocha.test$")))
                                                         (.getStackTrace (Thread/currentThread)))) m)
                       :error
                       (if (-> m :actual ex-data ::k/fail-fast)
                         (throw (:actual m))
                         (merge (stacktrace-file-and-line (.getStackTrace ^Throwable (:actual m))) m))
                       m)))))


(defn- rollup [results]
  (if-let [tests (:tests results)]
    (rollup tests)
    (into {}
          (map (juxt identity (fn [k]
                                (transduce (map #(k % 0)) + results))))
          [:test :fail :pass :error])))

(declare run-tests)

(defmulti run-test :type)

(defmethod run-test :default [testable]
  (output/warn "No implementation of" `run-test "for" testable))

(defmethod run-test :suite [suite]
  (t/do-report {:type :begin-test-suite :testable suite})
  (let [result (run-tests (:tests suite))
        suite' (cond-> (assoc suite :tests result)
                 (::k/fail-fast (meta result)) (assoc ::k/fail-fast true))]
    (t/do-report {:type :end-test-suite :testable suite'})
    suite'))

(defmethod run-test :ns [{:keys [ns tests] :as testable}]
  (let [ns-obj (the-ns ns)]
    (t/do-report {:type :begin-test-ns
                  :ns ns-obj
                  :testable testable})
    ;; If the namespace has a test-ns-hook function, call that:
    (if-let [v (find-var (symbol (str (ns-name ns-obj)) "test-ns-hook"))]
	    ((var-get v))
      ;; Otherwise, just test every var in the namespace.
      (let [once-fixture-fn (t/join-fixtures (::once-fixtures (meta ns)))
            each-fixture-fn (t/join-fixtures (::each-fixtures (meta ns)))
            result (once-fixture-fn
                    (fn []
                      (->> tests
                           (map #(assoc % :each-fixture-fn each-fixture-fn))
                           run-tests
                           (map #(dissoc % :each-fixture-fn)))))
            testable' (assoc testable :tests result)]
        (t/do-report {:type :end-test-ns
                      :ns ns-obj
                      :testable testable})
        testable'))))

(defmethod run-test :var [{:keys [var each-fixture-fn]
                           :or {each-fixture-fn #(%)}
                           :as testable}]
  (binding [t/*report-counters* (ref t/*initial-report-counters*)]
    (when-let [t (:test (meta var))]
      (binding [t/*testing-vars* (conj t/*testing-vars* var)]
        (t/do-report {:type :begin-test-var, :var var})
        (t/inc-report-counter :test)
        (try+
         (each-fixture-fn t)
         (catch ::k/fail-fast m
           (t/do-report {:type :end-test-var, :var var})
           (throw+ m))
         (catch Throwable e
           (t/do-report {:type :error, :message "Uncaught exception, not in assertion."
                         :expected nil, :actual e})))
        (t/do-report {:type :end-test-var, :var var})
        (merge testable @t/*report-counters*)
        ))))

(defn try-run-test [testable]
  (try+
   (run-test testable)
   (catch ::k/fail-fast m
     m)))

(defn run-tests [tests]
  (loop [[testable & tests] tests
         report             []]
    (if testable
      (let [test-result (try-run-test testable)
            report      (conj report (merge testable test-result))]
        (if (::k/fail-fast test-result)
          (recur [] (with-meta (concat report tests) {::k/fail-fast true}))
          (recur tests report)))
      report)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private with-reporter [r & body]
  `(with-redefs [t/report ~r]
     ~@body))

(defn result->report [results]
  (reduce (fn [r {type :type}]
            (cond
              (contains? #{:fail :pass :error} type) (update r type inc)
              (= :begin-test-var type)               (update r :test inc)
              :else                                  r))
          {:test 0 :pass 0 :fail 0 :error 0}
          results))

(defn test-plan
  "This loads and finds test namespaces and vars, and adds them to the config.
  From this point on the config is referred as the test plan."
  [config]
  (let [{:keys [only-suites suites randomize seed]} config]
    (assoc config
           :suites
           (->> suites
                (config/filter-suites only-suites)
                (map load/find-tests)))))

(defn add-random-seed [test-plan]
  (if (and (:randomize test-plan) (nil? (:seed test-plan)))
    (assoc test-plan :seed (rand-int (Integer/MAX_VALUE)))
    test-plan))

(defn randomize-tests [{:keys [randomize seed] :as test-plan}]
  (if randomize
    (update test-plan
            :suites
            (fn [suites]
              (map #(update % :tests (partial random/randomize-tests seed)) suites)))
    test-plan))

(defn run [config]
  (let [test-plan           (-> config
                                config/normalize
                                test-plan
                                add-random-seed
                                randomize-tests)
        reporter            (:reporter test-plan)
        color               (:color test-plan)
        fail-fast           (:fail-fast test-plan)
        reporter            (config/resolve-reporter
                             (if fail-fast
                               [reporter report/fail-fast]
                               reporter))
        results             (atom [])
        runtime             (java.lang.Runtime/getRuntime)
        main-thread         (Thread/currentThread)
        on-shutdown         (Thread. (fn []
                                       (println "^C")
                                       (binding [report/*results* results]
                                         (t/do-report (assoc (result->report @results)
                                                             :type :summary)))))
        do-finish           (fn [report]
                              (t/do-report (assoc report :type :summary))
                              (.removeShutdownHook runtime on-shutdown)
                              report)]
    (.addShutdownHook runtime on-shutdown)
    (when (:randomize test-plan)
      (println "Running with --seed" (:seed test-plan)))
    (try
      (with-reporter reporter
        (binding [output/*colored-output* color
                  report/*results*        results]
          (let [results (run-tests (:suites test-plan))]
            (do-finish (rollup results)))))
      (finally
        (.removeShutdownHook runtime on-shutdown)))))

(comment
  (->> "tests.edn"
       config/load-config
       config/normalize
       test-plan
       #_:suites
       #_first
       #_run-test)

  (-> "tests.edn"
      config/load-config
      (assoc :suites [{:type :suite
                       :tests
                       [{:type :ns
                         :ns (find-ns 'lambdaisland.kaocha.config-test)
                         :tests
                         [{:type :var
                           :var #'lambdaisland.kaocha.config-test/resolve-reporter}]}]}])
      run)

  (with-redefs [t/report (config/resolve-reporter ['lambdaisland.kaocha.report/dots
                                                   'lambdaisland.kaocha.report/fail-fast])]
    (run-test {:type :var, :var #'lambdaisland.kaocha.config-test/normalize-test}))

  #_=>
  {:type :var, :var #'lambdaisland.kaocha.config-test/resolve-reporter, :test 1, :pass 0, :fail 0, :error 1}

  (run-test
   {:type :ns
    :ns 'lambdaisland.kaocha.config-test
    :tests [{:type :var, :var #'lambdaisland.kaocha.config-test/resolve-reporter}
            {:type :var, :var #'lambdaisland.kaocha.config-test/normalize-test}]})

  #_=>

  ;; {:type :ns,
  ;;  :ns lambdaisland.kaocha.config-test,
  ;;  :tests [{:type :var,
  ;;          :var #'lambdaisland.kaocha.config-test/resolve-reporter,
  ;;          :each-fixture-fn #function[clojure.test/default-fixture],
  ;;          :test 1,
  ;;          :pass 7,
  ;;          :fail 0,
  ;;          :error 0}
  ;;         {:type :var,
  ;;          :var #'lambdaisland.kaocha.config-test/normalize-test,
  ;;          :each-fixture-fn #function[clojure.test/default-fixture],
  ;;          :test 1,
  ;;          :pass 0,
  ;;          :fail 1,
  ;;          :error 0}],
  ;;  :test 2,
  ;;  :fail 1,
  ;;  :pass 7,
  ;;  :error 0}

  )
