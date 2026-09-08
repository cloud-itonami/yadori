(ns yadori.cells.availability-check.test-state-machine
  "State-machine tests for the yadori 宿り availability_check cell (R0).
  1:1 port of the availability_check portion of cells/test_state_machines.py (ADR-2606038400).
  .solve() is NOT exercised for output (it raises). The env-gated POSITIVE live case
  (operator+YADORI_ALLOW_LIVE_RDAP both present → true) needs a process env flag and is covered by
  the Python suite under monkeypatch; here we pin the G7 SAFETY property: without BOTH, live is refused."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.lang.text :as str]
            [yadori.cells.availability-check.state-machine :as sm]))

(defn- check
  "Mirror _check(): run the 4 transitions, return the emitted availability_record map."
  [fqdn & {:keys [fixtures operator-gate] :or {fixtures {} operator-gate false}}]
  (let [s (sm/transition-to-normalized {"cell_state" {} "fqdn" fqdn})
        s (sm/transition-to-rdap-resolved (assoc s "operator_gate" operator-gate))
        s (sm/transition-to-classified (assoc s "fixtures" fixtures))
        s (sm/transition-to-availability-recorded s)]
    (get-in s ["cell_state" "payload" "availability_record"])))

(deftest test-available-from-fixture-through-cell
  (let [rec (check "free-name.dev" :fixtures {"free-name.dev" 404})]
    (is (= "available" (get rec "availability")))
    (is (= "fixture" (get rec "source")))
    (is (str/ends-with? (get rec "rdapUrl") "/domain/free-name.dev"))))

(deftest test-registered-from-fixture-through-cell
  (let [rec (check "example.com" :fixtures {"example.com" 200})]
    (is (= "registered" (get rec "availability")))))

(deftest test-idn-normalized-through-cell
  (let [rec (check "café.com" :fixtures {"xn--caf-dma.com" 404})]
    (is (= "xn--caf-dma.com" (get rec "asciiFqdn")))
    (is (= "available" (get rec "availability")))))

(deftest test-invalid-domain-flagged-through-cell
  (let [rec (check "nodot")]
    (is (= "invalid" (get rec "availability")))))

(deftest test-offline-no-fixture-is-unknown-not-available
  ;; G8: never guess :available with no evidence.
  (let [rec (check "mystery.com")]
    (is (= "unknown" (get rec "availability")))
    (is (= "none" (get rec "source")))))

(deftest test-g7-live-blocked-without-operator-attestation
  ;; no operator attestation → live NOT allowed regardless of env flag.
  (is (= false (sm/live-rdap-allowed? {"operator_gate" false}))))

(deftest test-g7-live-blocked-without-env-flag
  ;; operator attests but (in this test process) env flag absent → live NOT allowed → offline unknown.
  ;; (guarded: only meaningful when YADORI_ALLOW_LIVE_RDAP is unset, the default test env.)
  (when-not (= (System/getenv "YADORI_ALLOW_LIVE_RDAP") "1")
    (is (= false (sm/live-rdap-allowed? {"operator_gate" true})))
    (is (= "unknown" (get (check "mystery.org" :operator-gate true) "availability")))))

(deftest test-availability-cell-solve-raises-at-r0
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (sm/solve {}))))
