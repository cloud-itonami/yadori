(ns yadori.methods.availability
  "availability.py — yadori (宿り) domain-availability classifier — RDAP-based, stdlib only
  (ADR-2606038400). 1:1 Clojure port of `methods/availability.py`.

  RDAP (RFC 7482/9082) is the structured, rate-limit-friendly successor to WHOIS port-43 and the
  IANA-mandated availability primitive. The public contract is simple:

      GET {rdap_base}/domain/{ascii_fqdn}
        200  -> the domain is REGISTERED
        404  -> the domain is AVAILABLE
        429  -> rate-limited (unknown)

  This module does the offline-safe parts purely and deterministically: IDN/punycode normalization,
  TLD extraction, RDAP-endpoint resolution from a :representative IANA bootstrap table, RDAP-URL
  construction, status -> availability classification, and cross-TLD alternative generation. The
  live HTTP fetch is OFF by default (G7 outward-gated); enabling it is a Council Lv6+ + operator
  action.

  G1 read-only · G7 live fetch gated · G8 :representative bootstrap (bounded subset).

  House style: the Python dataclass `AvailabilityResult` is a string-keyed map (keys exactly the
  Python field names). Python's `.encode(\"idna\")` per-label punycode (RFC 3490/3492) is INLINED
  below because java.net.IDN is unavailable in Babashka and we keep this self-contained — ASCII
  labels pass through, unicode labels become xn--…. The live RDAP socket fetch is behind
  #?(:clj …). The Python `__main__` offline demo printer is intentionally omitted."
  (:require [kotoba.lang.text :as str]
            [clojure.set :as set]
            #?(:clj [clojure.java.io :as io])))

;; ── :representative IANA RDAP bootstrap (subset; G8). Source of truth at runtime is
;;    https://data.iana.org/rdap/dns.json — fetched only when live mode is operator-enabled. ──
(def RDAP-BOOTSTRAP
  {"com"  "https://rdap.verisign.com/com/v1"
   "net"  "https://rdap.verisign.com/net/v1"
   "org"  "https://rdap.publicinterestregistry.org/rdap"
   "info" "https://rdap.identitydigital.services/rdap"
   "io"   "https://rdap.identitydigital.services/rdap"
   "dev"  "https://www.registry.google/rdap"
   "app"  "https://www.registry.google/rdap"
   "ai"   "https://rdap.nic.ai"
   "xyz"  "https://rdap.centralnic.com/xyz"
   "jp"   "https://rdap.jprs.jp/rdap"})

;; A status this module can report without ever touching the network.
(def STATUS-AVAILABLE "available")
(def STATUS-REGISTERED "registered")
(def STATUS-UNSUPPORTED-TLD "unsupported-tld")
(def STATUS-INVALID "invalid")
(def STATUS-RATE-LIMITED "rate-limited")
(def STATUS-UNKNOWN "unknown")

(defn availability-result
  "Mirror of the Python AvailabilityResult dataclass as a string-keyed map.
  Field defaults match the dataclass (rdap_url=None, source=\"none\", representative=True, note=\"\")."
  [fqdn ascii-fqdn tld status
   & {:keys [rdap-url source representative note]
      :or {rdap-url nil source "none" representative true note ""}}]
  {"fqdn"           fqdn
   "ascii_fqdn"     ascii-fqdn
   "tld"            tld
   "status"         status
   "rdap_url"       rdap-url
   "source"         source
   "representative" representative
   "note"           note})

;; ── RFC 3492 Punycode + RFC 3490 ToASCII (per-label) — inlined (Python `.encode(\"idna\")`). ──
(def ^:private puny-base 36)
(def ^:private puny-tmin 1)
(def ^:private puny-tmax 26)
(def ^:private puny-skew 38)
(def ^:private puny-damp 700)
(def ^:private puny-initial-bias 72)
(def ^:private puny-initial-n 128)
(def ^:private puny-delimiter 0x2D) ; '-'

(defn- str->codepoints [s]
  #?(:clj (-> ^String s .codePoints .toArray vec)
     :cljs (mapv #(.charCodeAt s %) (range (count s)))))

(defn- digit->char
  "0..25 -> 'a'..'z', 26..35 -> '0'..'9'."
  [d]
  (char (if (< d 26) (+ d (int \a)) (+ (- d 26) (int \0)))))

(defn- adapt [delta numpoints first?]
  (let [delta (if first? (quot delta puny-damp) (quot delta 2))
        delta (+ delta (quot delta numpoints))]
    (loop [delta delta k 0]
      (if (> delta (quot (* (- puny-base puny-tmin) puny-tmax) 2))
        (recur (quot delta (- puny-base puny-tmin)) (+ k puny-base))
        (+ k (quot (* (+ puny-base 1 (- puny-tmin)) delta) (+ delta puny-skew)))))))

(defn- punycode-encode
  "RFC 3492 punycode encoding of a code-point vector (the unicode-only encoder body)."
  [input]
  (let [n* puny-initial-n
        delta* 0
        bias* puny-initial-bias
        basic (filterv #(< % 0x80) input)
        b (count basic)
        out (StringBuilder.)]
    (doseq [cp basic] (.append out (char cp)))
    (when (pos? b) (.append out (char puny-delimiter)))
    (loop [n n* delta delta* bias bias* h b]
      (if (< h (count input))
        (let [m (reduce (fn [m cp] (if (and (>= cp n) (< cp m)) cp m))
                        #?(:clj Integer/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)
                        input)
              delta (+ delta (* (- m n) (+ h 1)))
              n m
              ;; iterate code points, emitting variable-length integers
              res (reduce
                   (fn [{:keys [delta bias h] :as st} cp]
                     (cond
                       (< cp n) (assoc st :delta (+ delta 1))
                       (= cp n)
                       (let [_ (loop [q delta k puny-base]
                                 (let [t (cond
                                           (<= k bias) puny-tmin
                                           (>= k (+ bias puny-tmax)) puny-tmax
                                           :else (- k bias))]
                                   (if (< q t)
                                     (.append out (digit->char q))
                                     (do (.append out (digit->char (+ t (mod (- q t) (- puny-base t)))))
                                         (recur (quot (- q t) (- puny-base t)) (+ k puny-base))))))
                             bias (adapt delta (+ h 1) (= h b))]
                         (assoc st :delta 0 :bias bias :h (+ h 1)))
                       :else st))
                   {:delta delta :bias bias :h h}
                   input)]
          (recur (+ n 1) (+ (:delta res) 1) (:bias res) (:h res)))
        (.toString out)))))

(defn- encode-label
  "Per-label RFC 3490 ToASCII: ASCII labels pass through; labels with non-ASCII become xn--…
  Raises on an empty/oversized/invalid label, mirroring Python's idna codec ValueError."
  [label]
  (when (= "" label)
    (throw (ex-info "empty label" {:label label})))
  (let [cps (str->codepoints label)]
    (if (every? #(< % 0x80) cps)
      label
      (let [encoded (punycode-encode cps)]
        (str "xn--" encoded)))))

(defn normalize
  "Lowercase + strip + IDNA(punycode) encode. Raises ValueError-equivalent on an invalid name."
  [fqdn]
  (let [name (-> (or fqdn "") str/trim
                 (str/replace #"\.+$" "")   ; rstrip('.')
                 str/lower)]
    (when (or (= "" name) (not (str/includes? name ".")))
      (throw (ex-info (str "invalid domain: " (pr-str fqdn)) {:fqdn fqdn})))
    (let [labels (str/split name #"\." -1)]
      (str/join "."
                (mapv (fn [label]
                        (when (= "" label)
                          (throw (ex-info (str "invalid domain (empty label): " (pr-str fqdn))
                                          {:fqdn fqdn})))
                        (try
                          (encode-label label)
                          (catch #?(:clj Exception :cljs js/Error) exc
                            (throw (ex-info (str "invalid domain label " (pr-str label) ": " #?(:clj (.getMessage exc) :cljs (.-message exc)))
                                            {:label label})))))
                      labels)))))

(defn tld-of [ascii-fqdn]
  (last (str/split ascii-fqdn #"\.")))

(def ^:private script-ranges
  "Unicode block ranges for the scripts most relevant to IDN HOMOGRAPH confusion — the visually
  confusable Latin / Greek / Cyrillic triad plus CJK Han / Japanese Kana. Digits and the hyphen are
  script-neutral (:common)."
  [[:latin 0x41 0x5a] [:latin 0x61 0x7a] [:latin 0x00c0 0x024f]
   [:greek 0x0370 0x03ff] [:cyrillic 0x0400 0x04ff]
   [:han 0x4e00 0x9fff] [:kana 0x3040 0x30ff]])

(defn- char-script [ch]
  (let [c (int ch)]
    (if (or (<= 0x30 c 0x39) (= c 0x2d))
      :common
      (or (some (fn [[s lo hi]] (when (<= lo c hi) s)) script-ranges) :other))))

(defn label-scripts
  "The set of Unicode scripts present in a (Unicode, NOT punycode) domain label, excluding the
  script-neutral :common (digits, hyphen). Covers the homograph-relevant scripts Latin / Greek /
  Cyrillic / Han / Kana; anything else is :other."
  [label]
  (->> label (map char-script) (remove #{:common}) set))

(defn mixed-script-label?
  "Confusable-screen primitive (G6 no-squatting / N2 no-impersonation): is a domain label an IDN
  HOMOGRAPH — does it MIX Unicode scripts, e.g. a Cyrillic 'а' inside an otherwise-Latin 'аpple' that
  visually impersonates 'apple'? Read-only (G1; a pure string check, no socket, no zone enumeration)
  — it flags the risk so yadori can refuse to SURFACE or register a confusable name, never silently.
  A single-script label (all Latin 'café', all Cyrillic 'яндекс', all Han '東京') is legitimate and
  NOT flagged. Takes the Unicode label (decode punycode first); true iff ≥2 non-common scripts."
  [label]
  (> (count (label-scripts label)) 1))

(defn confusable-labels
  "The labels of a (Unicode, NOT punycode) domain that are IDN HOMOGRAPHS — each a single label that
  MIXES scripts (per `mixed-script-label?`). The DOMAIN-level confusable screen: a real FQDN has
  several labels and a homograph can hide in any of them (a Cyrillic char in the second-level name, a
  lookalike in a subdomain), so screening one label is not enough. Splits on '.', returns the
  confusable labels in order (empty = every label is script-consistent — note this allows a
  legitimately different script PER label, e.g. a Latin name under a Cyrillic IDN ccTLD, which is not
  a within-label homograph). Decode punycode first."
  [fqdn]
  (->> (str/split (str fqdn) #"\.")
       (filter mixed-script-label?)
       vec))

(defn confusable-fqdn?
  "Whether a (Unicode) domain carries ANY IDN-homograph label — the FQDN-level confusable screen that
  G6 (no-squatting) / N2 (no-impersonation) call for, so yadori can refuse to SURFACE or register a
  confusable name. Read-only (G1; pure string check, no socket / no zone enumeration). True iff some
  label mixes Unicode scripts."
  [fqdn]
  (boolean (seq (confusable-labels fqdn))))

(defn rdap-url
  "Construct the RDAP query URL, or nil if the TLD is not in the bootstrap table."
  ([ascii-fqdn] (rdap-url ascii-fqdn nil))
  ([ascii-fqdn bootstrap]
   (let [table (if (some? bootstrap) bootstrap RDAP-BOOTSTRAP)
         base (get table (tld-of ascii-fqdn))]
     (if-not base
       nil
       (str (str/replace base #"/+$" "") "/domain/" ascii-fqdn)))))

(defn classify-status
  "Map an RDAP HTTP status to an availability verdict."
  [http-status]
  (cond
    (= http-status 404) STATUS-AVAILABLE
    (= http-status 200) STATUS-REGISTERED
    (= http-status 429) STATUS-RATE-LIMITED
    :else STATUS-UNKNOWN))

;; A descriptive User-Agent is required: several RDAP servers (e.g. PIR/.org) return 403 to bare
;; requests. Identifying the client is also good-citizen practice for a read-only lookup (G1).
(def RDAP-USER-AGENT "yadori-rdap/0.1 (+https://etzhayyim.com/actor/yadori)")

#?(:clj
   (defn- live-rdap-status
     "Live RDAP GET — only reached when allow-live=true (G7)."
     [url]
     (let [conn (doto ^java.net.HttpURLConnection
                  (.openConnection (java.net.URL. url))
                  (.setRequestMethod "GET")
                  (.setConnectTimeout 10000)
                  (.setReadTimeout 10000)
                  (.setRequestProperty "Accept" "application/rdap+json")
                  (.setRequestProperty "User-Agent" RDAP-USER-AGENT))]
       (.getResponseCode conn))))

(defn check-availability
  "Classify a domain's availability.

  Offline-default: `fixtures` maps ascii_fqdn -> RDAP HTTP status. Live RDAP fetch only runs when
  `allow-live=true` (G7 outward-gated; operator + Council). With neither a fixture nor live mode,
  the result is STATUS-UNKNOWN with source \"none\"."
  [fqdn & {:keys [fixtures bootstrap allow-live]
           :or {fixtures nil bootstrap nil allow-live false}}]
  (let [ascii-fqdn (try (normalize fqdn)
                        (catch #?(:clj Exception :cljs js/Error) exc ::invalid))]
    (if (= ascii-fqdn ::invalid)
      (let [msg (try (normalize fqdn)
                     (catch #?(:clj Exception :cljs js/Error) exc
                       #?(:clj (.getMessage exc) :cljs (.-message exc))))]
        (availability-result fqdn fqdn "" STATUS-INVALID :note (str msg)))
      (let [tld (tld-of ascii-fqdn)
            url (rdap-url ascii-fqdn bootstrap)]
        (cond
          (nil? url)
          (availability-result
           fqdn ascii-fqdn tld STATUS-UNSUPPORTED-TLD
           :note (str "TLD '." tld "' not in :representative RDAP bootstrap (G8)"))

          (contains? (or fixtures {}) ascii-fqdn)
          (availability-result
           fqdn ascii-fqdn tld (classify-status (get fixtures ascii-fqdn))
           :rdap-url url :source "fixture")

          allow-live
          #?(:clj (availability-result
                   fqdn ascii-fqdn tld (classify-status (live-rdap-status url))
                   :rdap-url url :source "live" :representative false)
             :cljs (availability-result
                    fqdn ascii-fqdn tld STATUS-UNKNOWN
                    :rdap-url url :source "none"
                    :note "offline: live RDAP unavailable in cljs"))

          :else
          (availability-result
           fqdn ascii-fqdn tld STATUS-UNKNOWN :rdap-url url :source "none"
           :note "offline: no fixture and live RDAP is G7-gated (allow_live=False)"))))))

(defn suggest-alternatives
  "Generate cross-TLD candidate FQDNs for a second-level label, excluding known-taken names.

  Pure + deterministic (no network, no LLM — naming *brief* expansion is the name_suggest cell's
  Murakumo job; this is the mechanical TLD fan-out used when a preferred name is taken)."
  [sld & {:keys [tlds taken]
          :or {tlds ["com" "org" "dev" "io" "app"] taken nil}}]
  (let [taken (or taken #{})
        label (first (str/split (normalize (str sld ".com")) #"\."))]
    (vec
     (for [tld tlds
           :let [cand (str label "." tld)]
           :when (not (contains? taken cand))]
       cand))))
