(ns yadori.cells.reservation.state-machine
  "Phase state machine for the yadori 宿り reservation (asher) cell.
  1:1 port of cells/reservation/state_machine.py (ADR-2606038400).

  The defining yadori skill: turn an available domain into an *unsigned, member-principal*
  reservation intent, then authorize it with a member signature only. Pure, unit-tested transitions;
  the cell's .solve() raises until Council activation.

  Invariants enforced:
    G2 — no-fiat-inflow / member-principal: the payer is the member (okaimono assisted-checkout),
         never the religious-corp / a fiat processor. yadori is never the buyer-of-record.
    G3 — cloudflare-registrar-default: Cloudflare is default; an external registrar needs a Council flag.
    G5 — no-server-key: serverHeldKey=false; authorization requires a member sig, REFUSES a server sig.
    G6 — no-squatting: held-trademark/confusable screen + no speculation/parking + Charter-Rider scan.

  Conventions: dataclass ReservationState → a plain map with the SAME string field keys the Python
  `cs.__dict__` round-trips; phase enum value identities stay strings; ValueError → ex-info."
  (:require [kotoba.lang.text :as str]
            [yadori.methods.availability :as availability]))

;; G3: registrars yadori may select without a Council approval flag.
(def default-registrar "cloudflare")
(def council-gated-registrars #{"godaddy" "namecheap" "squarespace" "google" "porkbun"})

;; G2: funding sources. Only member-principal (okaimono assisted-checkout) is allowed.
(def allowed-funding #{"member-okaimono"})
(def prohibited-funding #{"org-fiat" "org-treasury" "stripe" "paypal" "card-on-file"})

;; G6: a :representative held-trademark / confusable screen list. Bounded seed (G8).
(def blocked-names #{"google" "amazon" "microsoft" "apple" "godaddy" "cloudflare" "meta"})

;; ── ReservationPhase (enum — Python value identities preserved) ──
(def reservation-phases
  {:init         "init"
   :screened     "screened"
   :quoted       "quoted"
   :intent-built "intent_built"
   :authorized   "authorized"})

(def phase-init         (:init reservation-phases))
(def phase-screened     (:screened reservation-phases))
(def phase-quoted       (:quoted reservation-phases))
(def phase-intent-built (:intent-built reservation-phases))
(def phase-authorized   (:authorized reservation-phases))

;; ── ReservationState (dataclass → plain map, string keys + field defaults) ──
(def state-defaults
  {"phase"                      phase-init
   "fqdn"                       "example-newproject.org"
   "sld"                        "example-newproject"   ; second-level label being claimed
   "registrar"                  default-registrar
   "council_approved_registrar" false
   "funding_source"             "member-okaimono"
   "payer"                      "member"               ; G2: always the member, never the org
   "server_held_key"            false                  ; G5: always false
   "member_sig"                 ""
   "server_sig"                 ""                     ; G5: must remain empty
   "charter_clean"              true
   "speculative"                false                  ; G6: parking / resale / drop-catch intent
   "payload"                    {}})

(defn- cell-state [state]
  (merge state-defaults (get state "cell_state" {})))

(defn transition-to-screened
  "G6: no-squatting eligibility screen — held-trademark list AND IDN-homograph
  confusable screen (the confusable half was previously undocumented-missing:
  `methods/availability.cljc` already shipped a fully-implemented, fully-
  tested `confusable-fqdn?` primitive, but no cell ever called it, so a
  homograph SLD like Cyrillic 'аpple' sailed through this gate — the same
  class of gap as kuni-umi's jurisdiction-eligibility fix)."
  [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "sld" (get state "sld" (get cs "sld"))
                  "speculative" (boolean (get state "speculative" false))
                  "charter_clean" (boolean (get state "charter_clean" true)))]
    (when (contains? blocked-names (str/lower (get cs "sld")))
      (throw (ex-info (str "G6 violation: '" (get cs "sld") "' fails held-trademark/confusable screen") {:gate "G6"})))
    (when (availability/confusable-fqdn? (get cs "sld"))
      (throw (ex-info (str "G6 violation: '" (get cs "sld")
                           "' is an IDN homograph (mixes Unicode scripts) — fails the confusable screen")
                      {:gate "G6" :confusable-labels (availability/confusable-labels (get cs "sld"))})))
    (when (get cs "speculative")
      (throw (ex-info "G6 violation: speculation / resale / parking intent is prohibited (N1/N2)" {:gate "G6"})))
    (when-not (get cs "charter_clean")
      (throw (ex-info "G6 violation: name fails Charter-Rider §2(a)-(h) scan" {:gate "G6"})))
    {"cell_state" (assoc cs "phase" phase-screened) "next_node" "quote"}))

(defn transition-to-quoted
  "G3: registrar-default. G2: funding source must be member-principal."
  [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "registrar" (get state "registrar" (get cs "registrar"))
                  "council_approved_registrar" (boolean (get state "council_approved_registrar" false))
                  "funding_source" (get state "funding_source" (get cs "funding_source")))]
    (when (and (contains? council-gated-registrars (get cs "registrar"))
               (not (get cs "council_approved_registrar")))
      (throw (ex-info (str "G3 violation: registrar '" (get cs "registrar") "' is Council-gated; "
                           "default is '" default-registrar "' (at-cost, no markup)") {:gate "G3"})))
    (when (contains? prohibited-funding (get cs "funding_source"))
      (throw (ex-info (str "G2 violation: funding '" (get cs "funding_source") "' forbidden; "
                           "acquisition is member-principal (okaimono assisted-checkout)") {:gate "G2"})))
    (when-not (contains? allowed-funding (get cs "funding_source"))
      (throw (ex-info (str "G2 violation: unknown funding '" (get cs "funding_source") "'") {:gate "G2"})))
    {"cell_state" (assoc cs "phase" phase-quoted) "next_node" "intent_built"}))

(defn transition-to-intent-built
  "G2/G5: unsigned, member-principal intent; serverHeldKey=false."
  [state]
  (let [cs (cell-state state)
        cs (assoc cs "payer" "member" "server_held_key" false "phase" phase-intent-built)
        intent {"fqdn" (get cs "fqdn")
                "registrar" (get cs "registrar")
                "registrantPrincipal" "member"
                "payer" "member"
                "fundingSource" (get cs "funding_source")
                "serverHeldKey" false
                "signed" false}]
    {"cell_state" (assoc cs "payload" (assoc (get cs "payload") "reservation_intent" intent))
     "next_node" "authorized"}))

(defn transition-to-authorized
  "G5: authorize on a MEMBER signature only; refuse any server signature."
  [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "member_sig" (get state "member_sig" "")
                  "server_sig" (get state "server_sig" ""))]
    (when (seq (get cs "server_sig"))
      (throw (ex-info "G5 violation: server signature refused (no-server-key, ADR-2605231525)" {:gate "G5"})))
    (when-not (seq (get cs "member_sig"))
      (throw (ex-info "G5 violation: member signature required to authorize a reservation" {:gate "G5"})))
    (let [cs (assoc cs "phase" phase-authorized)
          intent (assoc (get (get cs "payload") "reservation_intent" {}) "signed" true "signedBy" "member")
          payload (assoc (get cs "payload")
                         "reservation_intent" intent
                         "authorization" {"authorizedBy" "member"
                                          "serverSigned" false
                                          "outwardGated" true})]   ; G7
      {"cell_state" (assoc cs "payload" payload) "next_node" "end"})))

(defn solve
  "R0 scaffold: .solve() raises until Council activation (ADR-2606038400 §Decision)."
  [_input-state]
  (throw (ex-info "yadori R0 scaffold: activate reservation via Council ADR (post-2606038400 ratification)"
                  {:scaffold true})))
