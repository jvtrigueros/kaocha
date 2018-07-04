
(run! (partial add-classpath "test/unit") (filter modifiable-classloader? (classloader-hierarchy)))

(clojure.core/add-classpath (java.net.URL. "file:///home/arne/github/kaocha/test/unit")
                            )
(require 'kaocha.config-test)

(first (filter modifiable-classloader? (classloader-hierarchy)))
#object[clojure.lang.DynamicClassLoader 0x5fb365de "clojure.lang.DynamicClassLoader@5fb365de"]

(last (filter modifiable-classloader? (classloader-hierarchy)))
#object[clojure.lang.DynamicClassLoader 0x6d723864 "clojure.lang.DynamicClassLoader@6d723864"]

(.getContextClassLoader (Thread/currentThread))
#object[clojure.lang.DynamicClassLoader 0x7e7ae9da "clojure.lang.DynamicClassLoader@7e7ae9da"]
