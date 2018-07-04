(ns cljs-prepl
  (:require [cljs.repl.rhino :as rhino]
            [cljs.core.server :as cljs-server]
            [clojure.tools.reader.reader-types]))

(defprotocol Writer
  (write [writer string])
  (close [this]))

(deftype WritableReader [^java.util.Queue queue
                         ^:unsynchronized-mutable ^String s
                         ^:unsynchronized-mutable ^long s-pos]
  Writer
  (write [this string]
    (.put queue string))
  (close [this]
    (.put queue :done))

  clojure.tools.reader.reader-types/Reader
  (read-char [this]
    (cond
      (or (nil? s) (>= s-pos (count s)))
      (do
        (set! s (.take queue))
        (set! s-pos 0)
        (clojure.tools.reader.reader-types/read-char this))

      (= :done s)
      nil

      (> (count s) s-pos)
      (let [r (nth s s-pos)]
        (set! s-pos (inc s-pos))
        r)))

  (peek-char [this]
    (cond
      (or (nil? s) (>= s-pos (count s)))
      (do
        (set! s (.take queue))
        (set! s-pos 0)
        (clojure.tools.reader.reader-types/peek-char this))

      (= :done s)
      nil

      (> (count s) s-pos)
      (nth s s-pos))))

(defn writeable-reader []
  (let [queue (java.util.concurrent.LinkedBlockingQueue.)
        reader (->WritableReader queue nil 0)]
    reader))

(def reader (writeable-reader))


#_
(def reader
  (clojure.tools.reader.reader-types/source-logging-push-back-reader (clojure.tools.reader.reader-types/string-reader "(+ 1 1)\n" )))

(future
  (let [out *out*
        env (rhino/repl-env)]
    (cljs-server/prepl env
                       {}
                       (clojure.tools.reader.reader-types/source-logging-push-back-reader reader)
                       #(binding [*out* out, *flush-on-newline* true, *print-readably* true]
                          (prn %1)) )))


#_
(write reader "(require 'clojure.test)\n")
(write reader "(enable-console-print!)\n")
(write reader "(clojure.test/run-tests)\n")
(.queue reader)

(require 'cljs.main)
(cljs.main/-main)
