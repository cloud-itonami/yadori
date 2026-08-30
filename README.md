# yadori — DNS availability and acquisition actor

`yadori` (宿) is the standalone Tier-B actor for domain availability,
member-principal acquisition, renewal policy, and DNS provisioning. Canonical
metadata, data, schemas, and lexicon contracts are EDN; externally served
identity JSON is isolated under `wire/`.

The Cloudflare adapter in `yadori.cloudflare` now supplies the agent-facing
request surface for Search → authoritative Check → Passkey-approved Register,
registration status, auto-renew policy, and DNS record CRUD. It owns no token
and performs no HTTP by itself: the Cloud Itonami host injects the credentialed
transport. Immediately before a billable registration it repeats Check and
refuses if availability or either price changed. Premium registrations and the
Registrar API beta's unsupported manual-renew, transfer, and contact-update
operations fail closed.

## Layout

- `src/yadori/` — availability, reservation, social, and Murakumo logic
- `test/yadori/` — deterministic offline tests
- `schema/` and `contracts/lexicon/` — canonical EDN contracts
- `data/` — representative registrar data, seed, and identity journal
- `wire/identity/` — DID and profile JSON projections
- `docs/adr/` — actor-owned decisions and gate-gap records

Run `bb test`. Live RDAP and registrar mutations remain operator/Council gated;
the repository test suite performs no outward writes.
