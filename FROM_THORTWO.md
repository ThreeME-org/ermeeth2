# Notes from the thortwo session

Suggestions for ermeeth2 that came up while working in `~/Work/thortwo`. The
thortwo session only writes to this file and does not edit ermeeth2 itself;
sort, act on or delete items from the ermeeth2 side. Newest first.

## Open

(none)

## Done on the ermeeth2 side

- 2026-10-02: re-verified after the native-pipe conversion. `devtools::test()`
  passes and `R CMD check --no-manual` gives 0 errors, 0 warnings and 1 note
  (undefined globals in legacy functions, tracked in PLAN.md). The `.`
  placeholders were rewritten by hand and the affected functions compared
  before and after on a full v4 run (1.2.2). The cheap check notes are fixed in
  1.2.3. `magrittr` was never in Imports. The Rd braces were fixed by turning
  roxygen markdown on rather than by escaping them.
