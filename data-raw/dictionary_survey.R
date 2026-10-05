## Run with the package loaded: pkgload::load_all(); source("data-raw/dictionary_survey.R")

## Surveys the .mdl sources of ThreeME and writes data-raw/dictionary_survey.csv:
## one row per variable of the model, with what the sources and the packaged
## dictionary already say about it, and NA where nothing is known.
##
##   variable        root code, upper case, as in the results
##   label_fr        short French label          (packaged dictionary only)
##   label_long_fr   full French label           (dictionary, else the
##                                                translation of label_long_en
##                                                in dictionary_survey_fr.csv)
##   label_en        short English label         (packaged dictionary only)
##   label_long_en   full English label          (dictionary, else the `##!`
##                                                line above the equation)
##   index           what the variable is indexed by: none, sector, commodity...
##   index_forms     every form met in the equations, e.g. CID; CID[commodity]
##   kind            endogenous (an equation defines it), exogenous (declared
##                   in exogenous.mdl), calibrated (given a value by `:=` in an
##                   equation file) or undeclared (only met inside equations)
##   A `?` in index_forms is an index name that lists.mdl does not declare.
##   core            TRUE if met in the files of config_input_threeme.R
##   mdl_name        the name as the .mdl files write it
##   files           the files where it is defined, else where it is met
##
## The file is a survey to fill in, not the dictionary: data-raw/dictionary.csv
## is still what the package is built from.

mdl_path <- "../ThreeME_V4/src/model/threeme"
out_file <- "data-raw/dictionary_survey.csv"

## Files of the basic model, as listed in ThreeME_V4/configuration/config_input_threeme.R
core_files <- c("SU.mdl", "prices.mdl", "producer.mdl", "consumer.mdl", "government.mdl",
                "Trade_inter.mdl", "demography.mdl", "adjustments.mdl", "Verif.mdl",
                "ghg_emissions.mdl", "energybalance.mdl", "Exception_taxes_prices_new.mdl",
                "Exception_Other.mdl", "exogenous.mdl")

all_files <- list.files(mdl_path, pattern = "\\.mdl$")
lists_files <- c("lists.mdl", "lists_margins.mdl", grep("^R_lists_", all_files, value = TRUE))
not_equations <- c(lists_files, "ENDOFLINE.mdl", "model_files.mdl", "Intro_doc_eqs.mdl", "exogenous.mdl")
sources <- setdiff(all_files, not_equations)
sources <- c(intersect(core_files, sources), setdiff(sources, core_files))

## 1. Index names -------------------------------------------------------------
## lists.mdl says what every index ranges over: `%list_com_E := ccoa cfut ...`
## declares a list, `ce in %list_com_E` an index over it.

list_dimension <- function(list_name) {
  x <- sub("^%?list_", "", list_name)
  if (grepl("^sec|_sec$", x)) return("sector")
  if (grepl("^com|^ct1_ener|^chtrsp$", x)) return(if (x == "chtrsp") "transport mode" else "commodity")
  switch(x,
         F = "factor", GHG = "ghg", ener_class = "energy class",
         ener_sub_class = "energy class", buil_class = "building class",
         households = "household", tolower(x))
}

list_lines <- unlist(lapply(file.path(mdl_path, lists_files), readLines, warn = FALSE))
list_lines <- trimws(sub("#.*$", "", list_lines))

index_dimension <- character(0)   # index or element name -> dimension
add_dim <- function(names, dim) {
  new <- setdiff(names, names(index_dimension))
  index_dimension[new] <<- dim
}
## sectors and commodities first: an element such as `cele` is in several lists
decl <- regmatches(list_lines, regexec("^(%list_[A-Za-z0-9_]+)\\s*:=\\s*(.*)$", list_lines))
decl <- decl[lengths(decl) == 3]
decl <- decl[order(!vapply(decl, function(d) list_dimension(d[2]) %in% c("sector", "commodity"), logical(1)))]
for (d in decl) add_dim(strsplit(trimws(d[3]), "\\s+")[[1]], list_dimension(d[2]))
iter <- regmatches(list_lines, regexec("^([A-Za-z0-9_]+)\\s+in\\s+(%list_[A-Za-z0-9_]+)\\s*$", list_lines))
for (d in iter[lengths(iter) == 3]) index_dimension[d[2]] <- list_dimension(d[3])
## `O in D M`: domestic or imported
index_dimension[c("O", "D", "M")] <- "origin"
## indices the equation files use without lists.mdl declaring them
index_dimension[c("cc", "sss")] <- c("commodity", "sector")
index_dimension[c("m", "mm")] <- "commodity"   # margins, see Intro_doc_eqs.mdl
index_dimension["chtrspp"] <- "transport mode"

## 2. Equations and exogenous variables ---------------------------------------

doc <- suppressWarnings(mdl_document(sources = sources, exo = "exogenous.mdl", base.path = mdl_path))
doc <- doc[doc$kind %in% c("equation", "exovar"), ]

keywords <- c("sum", "on", "if", "where", "in", "log", "exp", "d", "dlog", "abs", "value",
              "elem", "over", "year", "left", "and", "or", "not", "include")

## Every `NAME` or `NAME[i, j]` of a statement, as name + index names
occurrences <- function(raw) {
  x <- gsub("#[^\n]*", " ", raw)
  x <- gsub("[%@][A-Za-z0-9_]+", " ", x)
  m <- gregexpr("(?<![A-Za-z0-9_.])[A-Za-z_][A-Za-z0-9_]*(\\s*\\[[^\\]]*\\])?", x, perl = TRUE)
  tok <- regmatches(x, m)[[1]]
  if (!length(tok)) return(NULL)
  name <- sub("\\s*\\[.*$", "", tok)
  idx <- ifelse(grepl("\\[", tok), gsub("\\s+", "", sub("^[^\\[]*\\[(.*)\\]$", "\\1", tok)), "")
  keep <- !tolower(name) %in% keywords & !(idx == "" & name %in% names(index_dimension))
  data.frame(name = name[keep], idx = idx[keep], stringsAsFactors = FALSE)
}

form_of <- function(idx) {
  vapply(strsplit(idx, ",", fixed = TRUE), function(i) {
    if (!length(i)) return("")
    dims <- unname(index_dimension[i])
    ## an element written in another case: `CCON` for `ccon`
    other_case <- match(tolower(i), tolower(names(index_dimension)))
    dims[is.na(dims)] <- unname(index_dimension[other_case[is.na(dims)]])
    dims[is.na(dims)] <- "?"
    paste(dims, collapse = ",")
  }, character(1))
}

occ <- do.call(rbind, lapply(seq_len(nrow(doc)), function(k) {
  o <- occurrences(if (doc$kind[k] == "exovar") doc$variable[k] else doc$raw[k])
  if (is.null(o) || !nrow(o)) return(NULL)
  ## the variable a statement defines: the parser's answer, else the first name
  defined <- if (!is.na(doc$variable[k])) sub("\\s*\\[.*$", "", doc$variable[k]) else o$name[1]
  ## `X := ...` gives a value once, as the calibration does: not an equation
  kind <- if (doc$kind[k] == "equation" && grepl(":=", doc$raw[k], fixed = TRUE)) "calibration" else doc$kind[k]
  cbind(o, form = form_of(o$idx), file = doc$file[k], kind = kind,
        defines = o$name == defined & !duplicated(o$name),
        description = doc$description[k], stringsAsFactors = FALSE)
}))
occ$variable <- toupper(occ$name)

## 3. One row per variable ----------------------------------------------------

clean_tex <- function(x) {
  x <- gsub("\\\\textit\\{([^}]*)\\}", "\\1", x)
  x <- gsub("\\\\\\\\", " ", x)
  x <- gsub("\\$", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  ifelse(nzchar(x), x, NA_character_)
}

dims_order <- c("commodity", "sector", "factor", "ghg", "household", "energy class",
                "building class", "transport mode", "origin")

rows <- lapply(split(occ, occ$variable), function(o) {
  forms <- unique(o$form)
  forms <- forms[order(lengths(strsplit(forms, ",")), forms)]
  dims <- unique(unlist(strsplit(forms, ",")))
  dims <- dims[dims != "?"]
  dims <- c(intersect(dims_order, dims), setdiff(dims, dims_order))
  defs <- o[o$defines & o$kind == "equation", , drop = FALSE]
  exo <- o[o$kind == "exovar", , drop = FALSE]
  calib <- o[o$defines & o$kind == "calibration", , drop = FALSE]
  ## the description of the most aggregate definition, from the basic model first
  described <- rbind(defs, exo, calib)
  described <- described[!is.na(described$description) & nzchar(described$description), , drop = FALSE]
  described <- described[order(!described$file %in% core_files,
                               lengths(strsplit(described$idx, ",", fixed = TRUE))), , drop = FALSE]
  where <- if (nrow(defs)) defs$file else if (nrow(exo)) exo$file else if (nrow(calib)) calib$file else o$file
  data.frame(
    variable = o$variable[1],
    description = if (nrow(described)) clean_tex(described$description[1]) else NA_character_,
    index = if (length(dims)) paste(dims, collapse = ",") else "none",
    index_forms = paste(ifelse(forms == "", o$variable[1], paste0(o$variable[1], "[", forms, "]")),
                        collapse = "; "),
    kind = if (nrow(defs)) "endogenous" else if (nrow(exo)) "exogenous"
           else if (nrow(calib)) "calibrated" else "undeclared",
    core = any(o$file %in% core_files),
    mdl_name = o$name[1],
    files = paste(unique(where), collapse = "; "),
    stringsAsFactors = FALSE)
})
survey <- do.call(rbind, rows)

## 4. Labels ------------------------------------------------------------------
## The packaged dictionary first; the description of the .mdl files gives the
## full English label of the others.

d <- threeme_dictionary()
at <- match(survey$variable, toupper(d$code))
survey$label_fr      <- d$label_short_fr[at]
survey$label_long_fr <- d$label_fr[at]
survey$label_en      <- d$label_short_en[at]
survey$label_long_en <- ifelse(is.na(d$label_en[at]), survey$description, d$label_en[at])

## French for the descriptions of the .mdl files: data-raw/dictionary_survey_fr.csv
## pairs each English description with its translation. The translations were
## written by Claude (2026-10-14) and have not been reviewed: to correct one,
## edit that file and run this script again. A description that changes in the
## sources no longer matches its line there and comes out as NA.
fr <- utils::read.csv("data-raw/dictionary_survey_fr.csv", stringsAsFactors = FALSE,
                      encoding = "UTF-8")
translated <- fr$label_long_fr[match(survey$label_long_en, fr$label_long_en)]
survey$label_long_fr <- ifelse(is.na(survey$label_long_fr), translated, survey$label_long_fr)

survey <- survey[order(survey$variable),
                 c("variable", "label_fr", "label_long_fr", "label_en", "label_long_en",
                   "index", "index_forms", "kind", "core", "mdl_name", "files")]
rownames(survey) <- NULL

utils::write.csv(survey, out_file, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

cli::cli_inform(c(
  "v" = "{nrow(survey)} variables written to {.file {out_file}}.",
  "i" = "{sum(survey$core)} in the basic model; {sum(!is.na(survey$label_long_en))} with a full English label, {sum(!is.na(survey$label_long_fr))} with a full French one, {sum(!is.na(survey$label_en))} with short ones.",
  "i" = "Dictionary codes not met in the sources: {.val {setdiff(toupper(d$code), survey$variable)}}"))
