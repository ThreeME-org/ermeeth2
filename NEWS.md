# ermeeth2 1.0.11

- `threeme_viewer()` opens on `GDP` alone. `Y` is selected only when the model
  has no `GDP`; before, both were selected.

# ermeeth2 1.0.10

- `threeme_viewer()` opens a full-size result at once. It now accepts a
  `.parquet` file and queries it instead of reading it: the list of variables
  comes from one small query, and only the rows of the variables selected are
  ever fetched. Pointed at an `.rds`, it uses the `.parquet` that
  `run_simulations()` wrote next to it, when there is one and it is not older
  than the `.rds`. On a 28x32 result (31 million rows, 88 000 variables) the
  data is ready in about 1 s, against 25 s to read the `.rds`. Needs the
  `arrow` package; without it, or without a parquet file, the `.rds` is read
  as before.
- `threeme_viewer(path)` no longer reads the file twice when it starts.

# ermeeth2 1.0.9

- EViews solver: a run no longer stops with `REAL() can only be applied to a
  'numeric', not a 'integer'` when the configuration sets `eviews_timeout` as
  an integer (`0L`). The timeout is now passed to EViews as a double.
- `config_addin()` writes whole numbers as `0`, `2019`, not `0L`, `2019L`. It
  was the addin that put the integer timeout in the file.

# ermeeth2 1.0.8

- `config_addin()`: the file lists of the Files tab now follow the Basics tab.
  After loading a configuration and changing its classification (or its
  country, or its base year), the lists and calibration files kept the names
  of the configuration as loaded -- `R_lists_FRA_c4_s4.mdl` after moving to
  `c28_s32` -- and editing any list then wrote those stale names into the
  file. The lists are now rebuilt from the configuration with the Basics as
  they stand, lines added by hand are kept, and a list that is not edited
  still keeps its code and comments.

# ermeeth2 1.0.7

- `threeme_viewer()` no longer freezes on a full-size model. The variable picker
  now searches server-side instead of sending every variable name to the browser
  (88 000 of them for a 28x32 result), and the plot and table are built from the
  rows of the selected variables only, so changing an option no longer rescans
  the whole result.
