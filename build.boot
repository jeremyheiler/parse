(set-env! :source-paths #{"src"}
          :dependencies '[[org.clojure/clojure "1.6.0"]])

(deftask build
  []
  (comp (pom :project 'parse :version "0.1.0")
        (aot :all true)
        (jar)))
