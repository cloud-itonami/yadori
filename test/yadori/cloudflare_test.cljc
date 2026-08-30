(ns yadori.cloudflare-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [yadori.cloudflare :as cf]))

(deftest request-shapes
  (is (= {:method :get
          :path "/accounts/acct/registrar/domain-search"
          :query {:q "agent domain" :limit 3}}
         (cf/search-request "acct" "agent domain" 3)))
  (is (= {:method :post
          :path "/accounts/acct/registrar/domain-check"
          :body {:domains ["example.com"]}}
         (cf/check-request "acct" ["Example.COM."])))
  (is (= {:domain_name "example.com"}
         (:body (cf/create-registration-request "acct" "example.com"))))
  (is (= {:auto_renew true}
         (:body (cf/auto-renew-request "acct" "example.com" true)))))

(deftest invalid-domain-refuses-before-http
  (is (= :yadori/invalid-domain
         (try (cf/check-request "acct" ["not a domain"])
              nil
              (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                (:type (ex-data e)))))))

(def available-check
  {:domains [{:name "agent-home.dev" :registrable true :tier "standard"
              :pricing {:currency "USD" :registration_cost "10.11"
                        :renewal_cost "10.11"}}]})

(deftest quote-is-consent-bindable
  (let [q (cf/domain-quote available-check "agent-home.dev")]
    (is (= "10.11" (:registration-cost q)))
    (is (= "10.11" (:renewal-cost q)))
    (is (true? (:billable? q)))
    (is (false? (:refundable? q)))
    (is (= (cf/quote-material q) (cf/quote-material q)))))

(deftest unavailable-and-premium-refuse
  (is (= :yadori/not-registrable
         (try (cf/domain-quote {:domains [{:name "taken.dev" :registrable false
                                    :reason "domain_unavailable"}]}
                        "taken.dev")
              nil
              (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                (:type (ex-data e))))))
  (is (= :yadori/premium-domain
         (try (cf/domain-quote {:domains [{:name "gold.dev" :registrable true
                                    :tier "premium"
                                    :pricing {:currency "USD"
                                              :registration_cost "1000.00"
                                              :renewal_cost "100.00"}}]}
                        "gold.dev")
              nil
              (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                (:type (ex-data e)))))))

(deftest approved-registration-rechecks-before-billable-post
  (let [calls (atom [])
        request! (fn [request]
                   (swap! calls conj request)
                   (if (= :post (:method request))
                     (if (str/ends-with? (:path request) "/domain-check")
                       {:success true :result available-check}
                       {:success true :result {:state "succeeded" :completed true}})
                     {:success true :result {}}))
        q (cf/domain-quote available-check "agent-home.dev")]
    (is (= "succeeded" (:state (cf/register-approved! request! "acct" q))))
    (is (= ["/accounts/acct/registrar/domain-check"
            "/accounts/acct/registrar/registrations"]
           (mapv :path @calls)))))

(deftest price-change-stops-before-registration
  (let [calls (atom [])
        changed (assoc-in available-check [:domains 0 :pricing :registration_cost] "11.11")
        request! (fn [request]
                   (swap! calls conj request)
                   {:success true :result changed})
        q (cf/domain-quote available-check "agent-home.dev")]
    (is (= :yadori/quote-changed
           (try (cf/register-approved! request! "acct" q)
                nil
                (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                  (:type (ex-data e))))))
    (is (= 1 (count @calls)))))

(deftest dns-write-shapes
  (is (= {:method :post
          :path "/zones/z1/dns_records"
          :body {:type "A" :name "www.example.com" :content "192.0.2.1"
                 :ttl 300 :proxied true}}
         (cf/create-dns-record-request
          "z1" {:type "a" :name "www.example.com" :content "192.0.2.1"
                :ttl 300 :proxied true})))
  (is (= :delete (:method (cf/delete-dns-record-request "z1" "r1")))))
