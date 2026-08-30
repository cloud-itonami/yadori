(require '[clojure.test :as t])
(def test-namespaces
  '[yadori.murakumo-test
    yadori.methods.test-availability
    yadori.methods.test-confusable-fqdn
    yadori.methods.test-homograph
    yadori.cloudflare-test
    yadori.cells.availability-check.test-state-machine
    yadori.cells.reservation.test-state-machine])
(doseq [ns-sym test-namespaces] (require ns-sym))
(let [result (apply t/run-tests test-namespaces)]
  (when-not (zero? (+ (:fail result) (:error result))) (System/exit 1)))
