# yadori repository rules

- EDN is canonical for metadata, data, schemas, contracts, and ADRs.
- External JSON, JSON-LD, or BPMN belongs under `wire/` only.
- Production namespaces live in `src/yadori`; tests live in `test/yadori`.
- Do not restore monorepo-relative paths, Go/TinyGo ports, shell launchers, generated WASM, or JSON-LD metadata.
- Preserve no-squatting, member-principal, no-server-key, sourcing-honesty, consent, and outward-action gates.
- Run `kbb -M:test`, parse every EDN file, and audit artifacts before publishing.
