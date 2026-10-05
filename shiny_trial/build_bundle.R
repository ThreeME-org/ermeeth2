## Builds model_bundle_<classification>.rds, the file the trial app (app.R) runs on.
##
## The app itself imports nothing: the compiled model and its calibration are
## shipped next to it in this bundle. This script is what writes the bundle, and
## is only run again when the model or the calibration changes:
##
##   Rscript shiny_trial/build_bundle.R /path/to/ThreeME_V4 [classification]
##
## The classification defaults to c29_s33; c13_s13 is the other one built.
##
## ThreeME_V4 is not touched: the model sources are copied to a temporary folder
## and DynaMo is run there.

args <- commandArgs(trailingOnly = TRUE)
threeme_path   <- if (length(args) >= 1) args[1] else file.path("..", "ThreeME_V4")
iso3           <- "FRA"
classification <- if (length(args) >= 2) args[2] else "c29_s33"
lastyear       <- 2050L
shockyear      <- 2021L

app_dir <- if (file.exists("build_bundle.R")) "." else "shiny_trial"
pkg_dir <- normalizePath(file.path(app_dir, ".."))
threeme_path <- normalizePath(threeme_path, mustWork = TRUE)
out_file <- file.path(normalizePath(app_dir), paste0("model_bundle_", classification, ".rds"))

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
})
if (requireNamespace("ermeeth2", quietly = TRUE)) {
  library(ermeeth2)
} else {
  pkgload::load_all(pkg_dir, quiet = TRUE)
}

## ---- 1. a scratch copy of what DynaMo reads ---------------------------------
scratch <- file.path(tempfile("threeme_bundle_"))
dir.create(file.path(scratch, "src", "compiler"), recursive = TRUE)
file.copy(file.path(threeme_path, "src", c("model", "bridges", "data")),
          file.path(scratch, "src"), recursive = TRUE)
file.copy(list.files(file.path(threeme_path, "src", "compiler"),
                     pattern = "^dynamo", full.names = TRUE),
          file.path(scratch, "src", "compiler"), recursive = TRUE)
file.copy(file.path(threeme_path, "configuration"), scratch, recursive = TRUE)

## ---- 2. the standard configuration, moved to 13x13 --------------------------
config_in <- file.path(scratch, "configuration", "config_input_bundle.R")
readLines(file.path(threeme_path, "configuration", "config_input_threeme.R")) |>
  str_replace("^classification\\s*=.*$", str_c('classification = "', classification, '"')) |>
  str_replace("^iso3\\s*=.*$", str_c('iso3 = "', iso3, '"')) |>
  str_replace("^project_name\\s*=.*$", 'project_name = "shiny_trial"') |>
  str_replace("^lastyear\\s*=.*$", str_c("lastyear = ", lastyear)) |>
  str_replace("^shockyear\\s*=.*$", str_c("shockyear = ", shockyear)) |>
  writeLines(config_in)

old_wd <- setwd(scratch)
config <- readconfig(input_config_file  = file.path("configuration", "config_input_bundle.R"),
                     output_config_file = file.path("configuration", "config_output_threeme.R"))
stopifnot(config$input$classification == classification)

## ---- 3. compile, then translate for the R solver ----------------------------
run_dynamo(config_list = config)

baseyear  <- config$input$baseyear
firstyear <- config$input$firstyear
tr <- prg_to_thor(base.year = baseyear, first.year = firstyear, last.year = lastyear)
if (length(tr$warnings)) {
  translate_report(tr)
  stop("The model translation reported problems.")
}

lists <- get_sec_com(file.path("src", "model", config$input$model_folder,
                               str_c("R_lists_", iso3, "_", classification, ".mdl")))
names_env <- new.env()
sys.source(file.path("src", "bridges", str_c("codenames_", classification, ".R")),
           envir = names_env)
setwd(old_wd)

git_commit <- tryCatch(
  system2("git", c("-C", shQuote(threeme_path), "rev-parse", "--short", "HEAD"),
          stdout = TRUE, stderr = FALSE),
  error = function(e) NA_character_)
git_branch <- tryCatch(
  system2("git", c("-C", shQuote(threeme_path), "branch", "--show-current"),
          stdout = TRUE, stderr = FALSE),
  error = function(e) NA_character_)

## ---- 4. the bundle ----------------------------------------------------------
## `data` is the steady-state baseline (the calibration, untouched) with the
## `elem_*` coefficients of the translation already on it: what thor_solve()
## reads. A shock is a handful of its columns changed.
bundle <- list(
  equations   = tr$equations,
  endo        = tr$endo,
  exo         = tr$exo,
  coef        = tr$coef,
  data        = tr$data,
  solver_only = unique(tr$elem$name),
  years       = list(first = firstyear, base = baseyear, shock = shockyear, last = lastyear),
  commodities = tolower(lists$commodities),
  sectors     = tolower(lists$sectors),
  names_commodities = names_env$names_commodities,
  names_sectors     = names_env$names_sectors,
  info = list(iso3 = iso3, classification = classification,
              model_folder = config$input$model_folder,
              threeme_branch = git_branch, threeme_commit = git_commit,
              ermeeth2 = as.character(utils::packageVersion("ermeeth2")),
              built = Sys.time())
)
saveRDS(bundle, out_file, compress = "xz")
unlink(scratch, recursive = TRUE)

cat(sprintf("\nWrote %s (%.1f Mb): %d equations, %d exogenous, %d-%d.\n",
            out_file, file.size(out_file) / 1e6, length(bundle$equations),
            length(bundle$exo), firstyear, lastyear))
