(ns yadori.cells.reservation.test-state-machine
  "State-machine tests for the yadori 宿り reservation cell (R0).
  1:1 port of the reservation portion of cells/test_state_machines.py (ADR-2606038400).
  Pins the member-principal (G2) / registrar-default (G3) / no-server-key (G5) / no-squatting (G6)
  invariants as hard refusals; .solve() raises at R0."
  (:require [clojure.test :refer [deftest is]]
            [yadori.cells.reservation.state-machine :as sm]))

(defn- run
  [& {:keys [sld speculative charter-clean registrar council-approved-registrar
             funding-source member-sig server-sig]
      :or {sld "example-newproject" speculative false charter-clean true
           registrar "cloudflare" council-approved-registrar false
           funding-source "member-okaimono" member-sig "member-ed25519-sig" server-sig ""}}]
  (let [s (sm/transition-to-screened
           {"cell_state" {} "sld" sld "speculative" speculative "charter_clean" charter-clean})
        s (sm/transition-to-quoted
           (merge s {"registrar" registrar
                     "council_approved_registrar" council-approved-registrar
                     "funding_source" funding-source}))
        s (sm/transition-to-intent-built s)
        s (sm/transition-to-authorized (merge s {"member_sig" member-sig "server_sig" server-sig}))]
    s))

(deftest test-happy-path-reaches-authorized
  (let [cs (get (run) "cell_state")
        intent (get-in cs ["payload" "reservation_intent"])]
    (is (= sm/phase-authorized (get cs "phase")))
    (is (= false (get intent "serverHeldKey")))            ; G5
    (is (= "member" (get intent "payer")))                 ; G2 — yadori never the buyer
    (is (= true (get intent "signed")))
    (is (= "member" (get intent "signedBy")))
    (is (= false (get-in cs ["payload" "authorization" "serverSigned"])))
    (is (= true (get-in cs ["payload" "authorization" "outwardGated"])))))  ; G7

(deftest test-g6-blocks-trademark-name
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G6 violation" (run :sld "google"))))

(deftest test-g6-blocks-idn-homograph-sld
  ;; Cyrillic а + Latin "pple" — same known-homograph fixture as
  ;; methods/test_confusable_fqdn.cljc's flags-a-homograph-in-the-second-level-name.
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G6 violation" (run :sld "аpple"))
      "a homograph SLD (Cyrillic а + Latin pple, mimicking 'apple') fails G6 even though it is not
      literally in the held-trademark list"))

(deftest test-g6-allows-single-script-idn-sld
  (is (= sm/phase-screened
         (get-in (sm/transition-to-screened {"cell_state" {} "sld" "café"}) ["cell_state" "phase"]))
      "an accented-but-single-script SLD (café) is not a homograph and is not trademark-blocked"))

(deftest test-g6-blocks-speculation
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G6 violation" (run :speculative true))))

(deftest test-g6-blocks-charter-unclean-name
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G6 violation" (run :charter-clean false))))

(deftest test-g3-default-registrar-is-cloudflare
  (is (= "cloudflare" (get-in (run :registrar "cloudflare") ["cell_state" "registrar"]))))

(deftest test-g3-external-registrar-requires-council
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G3 violation"
                        (run :registrar "godaddy" :council-approved-registrar false))))

(deftest test-g3-external-registrar-allowed-with-council-flag
  (is (= "godaddy" (get-in (run :registrar "godaddy" :council-approved-registrar true)
                           ["cell_state" "registrar"]))))

(deftest test-g2-rejects-fiat-funding
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G2 violation" (run :funding-source "org-fiat"))))

(deftest test-g5-refuses-server-signature
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G5 violation"
                        (run :member-sig "member-sig" :server-sig "server-sig"))))

(deftest test-g5-requires-member-signature
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G5 violation" (run :member-sig ""))))

(deftest test-solve-raises-at-r0
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (sm/solve {}))))
