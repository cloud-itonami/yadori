;; yadori 宿り — tests for the IDN homograph (mixed-script) confusable screen.
;; Run with `bb test`.
(ns yadori.methods.test-homograph
  "Tests for mixed-script-label? / label-scripts — the G6/N2 confusable-screen primitive that flags an
  IDN HOMOGRAPH (a label mixing Unicode scripts, e.g. a Cyrillic 'а' impersonating Latin 'apple')
  while leaving a legitimate single-script IDN (café / яндекс / 東京) un-flagged. Read-only (G1)."
  (:require [yadori.methods.availability :as a]
            [clojure.test :refer [deftest is run-tests]]))

(deftest flags-a-cyrillic-latin-homograph
  (is (a/mixed-script-label? "аpple") "'аpple' with a Cyrillic а impersonates 'apple' → flagged")
  (is (= #{:cyrillic :latin} (a/label-scripts "аpple")) "both scripts detected")
  (is (a/mixed-script-label? "gреek") "'gреek' with Cyrillic р,е → flagged"))

(deftest does-not-flag-a-legitimate-single-script-name
  (is (not (a/mixed-script-label? "apple")) "all-Latin")
  (is (not (a/mixed-script-label? "café")) "accented Latin (café) stays Latin — not a homograph")
  (is (not (a/mixed-script-label? "яндекс")) "all-Cyrillic (яндекс)")
  (is (not (a/mixed-script-label? "東京")) "all-Han (東京)"))

(deftest digits-and-hyphen-are-script-neutral
  (is (not (a/mixed-script-label? "my-app2")) "a hyphen and a digit do not make a Latin label mixed")
  (is (= #{:latin} (a/label-scripts "my-app2"))))

(deftest empty-and-pure-ascii-are-not-mixed
  (is (not (a/mixed-script-label? "")) "empty label")
  (is (not (a/mixed-script-label? "example")) "plain ASCII"))

#?(:clj
   (when (= *file* (System/getProperty "babashka.file"))
     (let [{:keys [fail error]} (run-tests 'yadori.methods.test-homograph)]
       (System/exit (if (zero? (+ fail error)) 0 1)))))
