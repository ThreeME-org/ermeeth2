# ermeeth2

R package of functions used by the ThreeME team. A rewrite of `ermeeth`
(/Users/139804/Work/ermeeth), which had accumulated verbatim, unreviewed ports from
`ThreeME_V4/src/functions_src/`. The API overhaul was deferred out of that repo into
this one.

## Read first

- **`PLAN.md`** — the current plan and what's done. Check it at the start of a session
  and keep it updated as work lands.
- **`/Users/139804/Work/ermeeth2_handoff.md`** — inventory of what was ported, what is
  intentionally out of scope, bugs already fixed, bugs still open. **Check it before
  rewriting any function**: several src originals contain landmines that get silently
  reintroduced if src is used as the starting point (notably the `scenario` argument
  shadowing in `simple_plot`).

## Conventions

- roxygen2 **8.1.0** (the old package was built on 7.3.2 — do not reintroduce 7.x output).
- Every exported function gets a roxygen header with `@export`.
- `<- NULL` declarations at the top of functions to silence R CMD check notes on NSE
  column names.
- Doc text may be French or English; both are present in the codebase.

## Data shape

What `longer_data()` produces, and what the plot/table functions expect:
columns `year`, `variable`, `scenario`, `values`, `values_ref`, `index_scen`,
optionally `sector` / `commodity`. `index_scen` is 1 for the baseline, 0 otherwise.

## Versioning

- Version lives in `DESCRIPTION` and as the top heading of `NEWS.md`; keep them in sync.
- Format is `MAJOR.MINOR.PATCH`, e.g. `1.0.0`.
- **Bump the patch component on every minor change / correction**: `1.0.0` → `1.0.1`
  → `1.0.2`. Do this as part of the change itself, not as a separate step.
- Bump the minor component for new features, the major one for breaking API changes.
