(in-ns 'user)

(import 'org.junit.platform.console.ConsoleLauncher)
(def console (System/console))
(def stdout (.writer console))
(def stderr (.writer console))

(defn junit
  ([] (junit "^(Test.*|.+[.$]Test.*|.*Tests?)$"))
  ([include]
   (ConsoleLauncher/run stdout stderr
                        (into-array String ["execute" "--disable-banner" "--scan-classpath"
                                            "-cp" "target/classes:target/test-classes"
                                            "-n" include]))))

(require '[clj-reload.core :as reload])
(reload/init {:dirs ["dev" "src"]})

(require 'virgil)
(virgil/watch-and-recompile ["src" "test"]
                            :options ["-Xlint:unchecked"]
                            :post-hook junit
                            :verbose true)

(require '[portal.api :as p])
(add-tap #'p/submit)

(set! *warn-on-reflection* true)