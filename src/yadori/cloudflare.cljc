(ns yadori.cloudflare
  "Pure Cloudflare Registrar and DNS request surface for yadori.

  The namespace owns no credential and performs no HTTP by itself. Callers pass
  a `request!` function which accepts one request map and returns a decoded
  Cloudflare v4 envelope. This keeps the actor portable while making every
  outward effect explicit and testable.

  Registration is deliberately two-stage: Search is discovery, Check is the
  authoritative quote, and Register is never constructed from a search result.
  Cloudflare registration is billable and non-refundable, so callers must bind
  the exact checked price into a Passkey/member approval before calling the
  registration request."
  (:require [clojure.string :as str]))

(def api-base "https://api.cloudflare.com/client/v4")

(defn- value [m k]
  (or (get m k) (get m (name k))))

(defn- required-text [label x]
  (let [v (some-> x str str/trim not-empty)]
    (when-not v
      (throw (ex-info (str label " is required")
                      {:type :yadori/invalid-input :field label})))
    v))

(defn fqdn [x]
  (let [v (-> (required-text "domain" x) str/lower-case (str/replace #"\.$" ""))]
    (when-not (and (<= 3 (count v) 253)
                   (re-matches #"(?i)[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\.[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)+" v))
      (throw (ex-info "domain must be an ASCII FQDN"
                      {:type :yadori/invalid-domain :domain v})))
    v))

(defn- account-path [account-id suffix]
  (str "/accounts/" (required-text "account-id" account-id) suffix))

(defn search-request [account-id query limit]
  {:method :get
   :path (account-path account-id "/registrar/domain-search")
   :query {:q (required-text "query" query)
           :limit (long (min 50 (max 1 (or limit 10))))}})

(defn check-request [account-id domains]
  (let [names (mapv fqdn domains)]
    (when (empty? names)
      (throw (ex-info "at least one domain is required"
                      {:type :yadori/invalid-input :field "domains"})))
    {:method :post
     :path (account-path account-id "/registrar/domain-check")
     :body {:domains names}}))

(defn list-registrations-request [account-id]
  {:method :get
   :path (account-path account-id "/registrar/registrations")})

(defn registration-request [account-id domain]
  {:method :get
   :path (account-path account-id
                       (str "/registrar/registrations/" (fqdn domain)))})

(defn registration-status-request [account-id domain]
  {:method :get
   :path (account-path account-id
                       (str "/registrar/registrations/" (fqdn domain)
                            "/registration-status"))})

(defn create-registration-request [account-id domain]
  {:method :post
   :path (account-path account-id "/registrar/registrations")
   :body {:domain_name (fqdn domain)}})

(defn auto-renew-request [account-id domain enabled?]
  {:method :patch
   :path (account-path account-id
                       (str "/registrar/registrations/" (fqdn domain)))
   :body {:auto_renew (boolean enabled?)}})

(defn list-dns-records-request [zone-id]
  {:method :get
   :path (str "/zones/" (required-text "zone-id" zone-id) "/dns_records")})

(defn create-dns-record-request [zone-id record]
  (let [type (some-> (value record :type) str str/upper-case not-empty)
        name (required-text "name" (value record :name))
        content (required-text "content" (value record :content))]
    (when-not (contains? #{"A" "AAAA" "CAA" "CNAME" "MX" "NS" "SRV" "TXT"} type)
      (throw (ex-info "unsupported DNS record type"
                      {:type :yadori/dns-record-type :record-type type})))
    {:method :post
     :path (str "/zones/" (required-text "zone-id" zone-id) "/dns_records")
     :body (cond-> {:type type :name name :content content}
             (some? (value record :ttl)) (assoc :ttl (value record :ttl))
             (some? (value record :proxied)) (assoc :proxied (boolean (value record :proxied)))
             (some? (value record :priority)) (assoc :priority (value record :priority))
             (some? (value record :comment)) (assoc :comment (value record :comment))) }))

(defn update-dns-record-request [zone-id record-id record]
  (assoc (create-dns-record-request zone-id record)
         :method :patch
         :path (str "/zones/" (required-text "zone-id" zone-id)
                    "/dns_records/" (required-text "record-id" record-id))))

(defn delete-dns-record-request [zone-id record-id]
  {:method :delete
   :path (str "/zones/" (required-text "zone-id" zone-id)
              "/dns_records/" (required-text "record-id" record-id))})

(defn unwrap
  "Return a Cloudflare result or raise its bounded error."
  [envelope]
  (if (false? (value envelope :success))
    (let [errors (or (value envelope :errors) [])
          message (or (value (first errors) :message) "Cloudflare API refused the request")]
      (throw (ex-info message {:type :yadori/cloudflare-refused :errors errors})))
    (value envelope :result)))

(defn call! [request! request]
  (unwrap (request! request)))

(defn domain-results [result]
  (vec (or (value result :domains) [])))

(defn checked-domain [result domain]
  (let [wanted (fqdn domain)]
    (some #(when (= wanted (some-> (value % :name) str str/lower-case)) %) (domain-results result))))

(defn domain-quote
  "Create the exact, consent-bindable quote from a Check response."
  [check-result domain]
  (let [entry (or (checked-domain check-result domain)
                  (throw (ex-info "checked domain is absent from response"
                                  {:type :yadori/check-missing :domain (fqdn domain)})))
        pricing (value entry :pricing)
        tier (or (value entry :tier) "standard")]
    (when-not (true? (value entry :registrable))
      (throw (ex-info "domain is not registrable"
                      {:type :yadori/not-registrable
                       :domain (fqdn domain)
                       :reason (value entry :reason)})))
    (when-not (= "standard" tier)
      (throw (ex-info "premium domain registration is not supported"
                      {:type :yadori/premium-domain :domain (fqdn domain)})))
    {:domain (fqdn domain)
     :registrar :cloudflare
     :tier tier
     :currency (required-text "currency" (value pricing :currency))
     :registration-cost (required-text "registration-cost"
                                       (value pricing :registration_cost))
     :renewal-cost (required-text "renewal-cost" (value pricing :renewal_cost))
     :billable? true
     :refundable? false
     :authoritative-source :domain-check}))

(defn quote-material
  "Stable material bound into the app's Passkey approval digest."
  [{:keys [domain registrar tier currency registration-cost renewal-cost]}]
  (str "yadori-registration/v1"
       "|domain=" domain
       "|registrar=" (name registrar)
       "|tier=" tier
       "|registration=" registration-cost
       "|renewal=" renewal-cost
       "|currency=" currency))

(defn same-quote? [a b]
  (= (select-keys a [:domain :registrar :tier :currency
                     :registration-cost :renewal-cost])
     (select-keys b [:domain :registrar :tier :currency
                     :registration-cost :renewal-cost])))

(defn verify-current-quote!
  "Re-check immediately before registration and refuse a price/status change."
  [request! account-id approved-quote]
  (let [fresh (-> (call! request! (check-request account-id [(:domain approved-quote)]))
                  (domain-quote (:domain approved-quote)))]
    (when-not (same-quote? approved-quote fresh)
      (throw (ex-info "domain availability or price changed after approval"
                      {:type :yadori/quote-changed
                       :approved approved-quote :current fresh})))
    fresh))

(defn register-approved!
  "Register exactly one Passkey-approved quote after a fresh authoritative check.

  This performs a billable, non-refundable outward action. The caller is
  responsible for proving that the approval belongs to the member; this
  function ensures the approved terms still match immediately before the POST."
  [request! account-id approved-quote]
  (verify-current-quote! request! account-id approved-quote)
  (call! request! (create-registration-request account-id (:domain approved-quote))))

(def unsupported-operations
  {:manual-renew :cloudflare-registrar-api-beta
   :transfer :cloudflare-registrar-api-beta
   :contact-update :cloudflare-registrar-api-beta})
