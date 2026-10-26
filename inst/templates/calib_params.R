## Parameter sheet {{identifier}} of the shock {{shock}}
## Created by ermeeth2::create_params() on {{date}}
##
## The values 2_calib_shock_{{shock}}.R is run with. Each combination of the
## values below is one scenario, {{shock}}_{{identifier}}_1, {{shock}}_{{identifier}}_2, ...
## in the order of the rows of the grid (the first parameter varies slowest).
## A parameter of the shock sheet left out here keeps its default.
##
## Run it with, in the configuration:
##
##   parametric_shocks = list({{shock}} = "{{identifier}}")
##
## The grid can be cut down before it ends the sheet, e.g.
##   shock_grid(a = c(0, 1), b = c(0, 1)) |> dplyr::filter(a + b <= 1)
##
## The values used by each scenario are written to
## data/output/<project_name>_params.csv with the results.

{{grid}}
