;; yadori 宿り — tests for the FQDN-level confusable (IDN-homograph) screen.
;; Run with `bb test`.
(ns yadori.methods.test-confusable-fqdn
  "Tests for confusable-fqdn? / confusable-labels — the domain-level G6/N2 confusable screen that
  checks EVERY label (a homograph can hide in a subdomain or the second-level name), composing the
  per-label mixed-script-label?. Read-only (G1)."
  (:require [yadori.methods.availability :as a]
            [clojure.test :refer [deftest is run-tests]]))

(deftest flags-a-homograph-in-the-second-level-name
  (is (a/confusable-fqdn? "аpple.com") "Cyrillic а in the SLD")
  (is (= ["аpple"] (a/confusable-labels "аpple.com")) "the offending label is named"))

(deftest flags-a-homograph-hidden-in-a-subdomain
  (is (a/confusable-fqdn? "secure.раypal.com") "the homograph is in the SLD under a subdomain")
  (is (= ["раypal"] (a/confusable-labels "secure.раypal.com"))))

(deftest does-not-flag-an-all-ascii-domain
  (is (not (a/confusable-fqdn? "apple.com")))
  (is (= [] (a/confusable-labels "www.example.org"))))

(deftest does-not-flag-a-legitimate-single-script-idn
  (is (not (a/confusable-fqdn? "café.fr")) "accented Latin SLD is not a homograph")
  ;; a Latin name under a Cyrillic IDN ccTLD: each label is single-script — legitimate, not a
  ;; within-label homograph
  (is (not (a/confusable-fqdn? "apple.яндекс.рф")) "different scripts in DIFFERENT labels are allowed"))

(deftest names-every-confusable-label-when-more-than-one
  (is (= ["раypal" "gооgle"] (a/confusable-labels "раypal.gооgle.com"))
      "both mixed-script labels are reported, in order; the ascii tld is clean"))

#?(:clj
   (when (= *file* (System/getProperty "babashka.file"))
     (let [{:keys [fail error]} (run-tests 'yadori.methods.test-confusable-fqdn)]
       (System/exit (if (zero? (+ fail error)) 0 1)))))
