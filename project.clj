(defproject pci-book "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.0"]
                 [midje "1.6.3"]
                 [criterium "0.4.3"]]
  :main ^:skip-aot pci-book.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
