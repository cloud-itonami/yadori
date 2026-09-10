(ns yadori.cells.availability-check.state-machine
  "Phase state machine for the yadori 宿り availability_check (dan) cell.
  1:1 port of cells/availability_check/state_machine.py (ADR-2606038400).

  normalize → resolve-rdap → classify → availability-recorded. The transitions wrap the real RDAP
  classifier in yadori.methods.availability (the Python loaded it via importlib.util to dodge package
  imports; cljc resolves the sibling ns directly). The cell's .solve() raises until Council activation.

  Gates enforced here:
    G1 — read-only: only RDAP `domain` lookups; never zone enumeration / AXFR.
    G7 — outward-gated: LIVE RDAP fetch reachable only when BOTH operator_gate=true in-state AND the
         process env YADORI_ALLOW_LIVE_RDAP=1 is set; otherwise offline (fixtures or :unknown), never a socket.
    G8 — sourcing-honesty: offline results are representative; with no fixture and live not allowed,
         the verdict is STATUS-UNKNOWN (never a guessed :available).

  Conventions: dataclass AvailabilityCheckState → a plain map with the SAME string field keys the
  Python `cs.__dict__` round-trips (phase/fqdn/ascii_fqdn/…); phase enum value identities stay strings."
  (:require [yadori.methods.availability :as availability]))

(def live-env-flag "YADORI_ALLOW_LIVE_RDAP")

;; ── AvailabilityPhase (enum — Python value identities preserved) ──
(def availability-phases
  {:init                  "init"
   :normalized            "normalized"
   :rdap-resolved         "rdap_resolved"
   :classified            "classified"
   :availability-recorded "availability_recorded"})

(def phase-init                  (:init availability-phases))
(def phase-normalized            (:normalized availability-phases))
(def phase-rdap-resolved         (:rdap-resolved availability-phases))
(def phase-classified            (:classified availability-phases))
(def phase-availability-recorded (:availability-recorded availability-phases))

;; ── AvailabilityCheckState (dataclass → plain map, string keys + field defaults) ──
(def state-defaults
  {"phase"          phase-init
   "fqdn"           "example.com"
   "ascii_fqdn"     ""
   "tld"            ""
   "invalid"        false
   "rdap_url"       ""
   "live_allowed"   false       ; G7: resolved from operator_gate AND env flag
   "status"         ""
   "source"         "none"
   "representative" true
   "payload"        {}})

(defn- cell-state
  "Mirror _state(d): the cell_state sub-map merged onto the dataclass defaults."
  [state]
  (merge state-defaults (get state "cell_state" {})))

(defn live-rdap-allowed?
  "G7 gate: an operator attestation in-state AND the process env flag must BOTH be present."
  [state]
  (let [operator-attested (boolean (get state "operator_gate"))
        env-enabled (= #?(:clj (System/getenv live-env-flag) :cljs nil) "1")]
    (and operator-attested env-enabled)))

(defn transition-to-normalized
  "G1: normalize (IDNA/punycode). Invalid names are flagged, not raised."
  [state]
  (let [cs (cell-state state)
        fqdn (get state "fqdn" (get cs "fqdn"))
        cs (assoc cs "fqdn" fqdn)
        cs (try
             (let [ascii (availability/normalize fqdn)]
               (assoc cs "ascii_fqdn" ascii "tld" (availability/tld-of ascii) "invalid" false))
             (catch #?(:clj Exception :cljs js/Error) _e
               (assoc cs "ascii_fqdn" "" "tld" "" "invalid" true)))
        cs (assoc cs "phase" phase-normalized)]
    {"cell_state" cs "next_node" "resolve_rdap"}))

(defn transition-to-rdap-resolved
  "Resolve the RDAP URL and the G7 live-fetch decision (no network here)."
  [state]
  (let [cs (cell-state state)
        cs (assoc cs "live_allowed" (live-rdap-allowed? state))
        cs (if-not (get cs "invalid")
             (assoc cs "rdap_url" (or (availability/rdap-url (get cs "ascii_fqdn")) ""))
             cs)
        cs (assoc cs "phase" phase-rdap-resolved)]
    {"cell_state" cs "next_node" "classify"}))

(defn transition-to-classified
  "Classify via the real RDAP classifier. Live fetch happens ONLY if G7 allowed it."
  [state]
  (let [cs (cell-state state)
        fixtures (or (get state "fixtures") {})
        result (availability/check-availability (get cs "fqdn")
                                                :fixtures fixtures
                                                :allow-live (get cs "live_allowed"))
        cs (assoc cs
                  "status" (get result "status")
                  "source" (get result "source")
                  "representative" (get result "representative")
                  "rdap_url" (or (get result "rdap_url") (get cs "rdap_url"))
                  "tld" (or (get result "tld") (get cs "tld"))
                  "phase" phase-classified)]
    {"cell_state" cs "next_node" "availability_recorded"}))

(defn transition-to-availability-recorded
  "Emit the com.etzhayyim.yadori.availabilityRecord payload."
  [state]
  (let [cs (cell-state state)
        cs (assoc cs "phase" phase-availability-recorded)
        record {"fqdn" (get cs "fqdn")
                "asciiFqdn" (get cs "ascii_fqdn")
                "tld" (get cs "tld")
                "availability" (get cs "status")
                "rdapUrl" (get cs "rdap_url")
                "source" (get cs "source")            ; "fixture" | "live" | "none"
                "representative" (get cs "representative")}
        cs (assoc cs "payload" (assoc (get cs "payload") "availability_record" record))]
    {"cell_state" cs "next_node" "end"}))

(defn solve
  "R0 scaffold: .solve() raises until Council activation (ADR-2606038400 §Decision)."
  [_input-state]
  (throw (ex-info "yadori R0 scaffold: activate availability_check via Council ADR (post-2606038400 ratification)"
                  {:scaffold true})))
