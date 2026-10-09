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
