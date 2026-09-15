## The ThreeME variable dictionary: one row per root variable code.
##
## The packaged copy is the default so that plots, tables and Quarto renders
## resolve labels offline and reproducibly. A project extends or overrides it
## through the `dict` argument or the `ermeeth2.dictionary` option, which is
## what absorbs variables added to the model between package releases.

#' Columns of a ThreeME dictionary
#'
#' The schema every dictionary must carry. `code` is the root variable code,
#' unindexed: sector and commodity live in their own columns of the long
#' format, so `variable` in the data joins straight onto `code` here.
#'
#' @returns a character vector of column names.
#' @keywords internal
dictionary_columns <- function() {
  c("code",
    "label_en", "label_fr",
    "label_short_en", "label_short_fr",
    "unit", "default_transformation", "group", "indexed_by",
    "definition_en", "definition_fr")
}

#' Languages the dictionary carries
#'
#' @returns a character vector of language codes.
#' @keywords internal
dictionary_languages <- function() c("en", "fr")

#' Index names to the dimension they stand for
#'
#' The ThreeME convention: `s` indexes sectors, `c` indexes commodities. Held
#' as data, like [mdl_symbols()], so a model using other letters can extend it
#' rather than fork the code.
#'
#' @param extra a named character vector merged over the defaults, e.g.
#'   `c(r = "region")`.
#'
#' @returns a named character vector mapping index name to dimension.
#' @export
#'
#' @examples
#' dictionary_index_map()
dictionary_index_map <- function(extra = NULL) {
  out <- c(s = "sector", c = "commodity")
  if (!is.null(extra)) out[names(extra)] <- unname(extra)
  out
}

#' Split an `indexed_by` field into its parts
#'
#' @param x character vector of `indexed_by` values.
#'
#' @returns a list of character vectors, empty where the variable is unindexed.
#' @keywords internal
index_split <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  lapply(strsplit(x, "\\s*,\\s*"), function(p) p[nzchar(p)])
}

#' Is a variable indexed?
#'
#' @param codes character vector of variable codes.
#' @param by `NULL` for "indexed at all", or `"sector"` / `"commodity"` to ask
#'   about one dimension.
#' @param dict passed to [threeme_dictionary()].
#'
#' @returns a logical vector as long as `codes`. `FALSE` where the dictionary
#'   does not know the variable, since an unknown index is not a claim of one.
#' @export
#'
#' @examples
#' dict_is_indexed(c("GDP", "PY", "PM"))
#' dict_is_indexed(c("GDP", "PY", "PM"), by = "commodity")
dict_is_indexed <- function(codes, by = NULL, dict = NULL) {
  parts <- index_split(dictionary_lookup(codes, "indexed_by", dict, fallback = "na"))
  if (is.null(by)) return(lengths(parts) > 0L)
  by <- match.arg(by, c("sector", "commodity"))
  vapply(parts, function(p) by %in% p, logical(1))
}

#' Check and normalise a dictionary
#'
#' Missing optional columns are added empty, so a project can supply a CSV with
#' just `code` and `label_fr` and still get a usable dictionary back.
#'
#' @param dict a data frame.
#' @param what what to call it in error messages.
#'
#' @returns the data frame, with every column of [dictionary_columns()].
#' @keywords internal
dictionary_validate <- function(dict, what = "dictionary") {
  if (!is.data.frame(dict)) {
    stop("`", what, "` must be a data frame, not a ", class(dict)[1], ".")
  }
  if (!"code" %in% names(dict)) {
    stop("`", what, "` has no `code` column. Columns found: ",
         paste(names(dict), collapse = ", "), ".")
  }
  dict$code <- as.character(dict$code)
  dupes <- unique(dict$code[duplicated(dict$code)])
  if (length(dupes)) {
    stop("`", what, "` has duplicated codes: ", paste(dupes, collapse = ", "), ".")
  }
  for (col in setdiff(dictionary_columns(), names(dict))) {
    dict[[col]] <- NA_character_
  }
  for (col in dictionary_columns()) {
    dict[[col]] <- as.character(dict[[col]])
  }
  known <- threeme_transformations()
  tr <- dict$default_transformation
  bad <- unique(tr[!is.na(tr) & !tr %in% names(known)])
  if (length(bad)) {
    stop("`", what, "` sets unknown `default_transformation`: ",
         paste(bad, collapse = ", "), ".\n",
         "Available: ", paste(names(known), collapse = ", "), ".")
  }
  bad_idx <- unique(unlist(index_split(dict$indexed_by)))
  bad_idx <- setdiff(bad_idx, c("sector", "commodity"))
  if (length(bad_idx)) {
    stop("`", what, "` sets unknown `indexed_by`: ", paste(bad_idx, collapse = ", "),
         ".\nAvailable: sector, commodity, or both as \"sector,commodity\". ",
         "Leave it empty for a variable that carries no index.")
  }
  dict[, c(dictionary_columns(),
           setdiff(names(dict), dictionary_columns())), drop = FALSE]
}

#' Read a dictionary from a CSV path or take it as given
#'
#' @param x a data frame, or a path to a CSV.
#' @param what what to call it in error messages.
#'
#' @returns a validated dictionary.
#' @keywords internal
dictionary_read <- function(x, what = "dict") {
  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) stop("`", what, "`: no such file: ", x)
    x <- utils::read.csv(x, stringsAsFactors = FALSE, encoding = "UTF-8",
                         na.strings = c("", "NA"))
  }
  dictionary_validate(x, what)
}

#' The ThreeME variable dictionary
#'
#' Returns the dictionary used to resolve variable codes to labels, units,
#' groups and default transformations.
#'
#' Resolution order, each layer overriding the one before it by `code`:
#' the packaged dictionary, then the `ermeeth2.dictionary` option, then the
#' `dict` argument. Layering is what lets a project name only the variables the
#' packaged copy does not know about yet, rather than restating all of it.
#'
#' @param dict a data frame or a path to a CSV, layered on top of the packaged
#'   dictionary. `NULL` (the default) uses whatever the option provides.
#' @param overlay `TRUE` (the default) layers `dict` over the packaged
#'   dictionary. `FALSE` uses `dict` alone, ignoring both other layers.
#'
#' @returns a data frame with the columns of [dictionary_columns()].
#' @export
#'
#' @examples
#' head(threeme_dictionary())
#'
#' # a project adding one variable of its own
#' mine <- data.frame(code = "CARBTAX", label_en = "Carbon tax revenue")
#' nrow(threeme_dictionary(mine)) == nrow(threeme_dictionary()) + 1
threeme_dictionary <- function(dict = NULL, overlay = TRUE) {
  if (!overlay) {
    if (is.null(dict)) stop("`overlay = FALSE` needs a `dict` to use instead.")
    return(dictionary_read(dict, "dict"))
  }

  out <- dictionary_validate(
    get("threeme_dictionary_data", envir = asNamespace("ermeeth2")),
    "the packaged dictionary"
  )

  opt <- getOption("ermeeth2.dictionary", NULL)
  for (layer in list(opt, dict)) {
    if (is.null(layer)) next
    layer <- dictionary_read(layer, "dict")
    out <- rbind(out[!out$code %in% layer$code, , drop = FALSE],
                 layer[, names(out), drop = FALSE])
  }
  out[order(out$code), , drop = FALSE]
}

#' Look one column of the dictionary up for a set of codes
#'
#' @param codes character vector of variable codes.
#' @param field the dictionary column to read.
#' @param dict passed to [threeme_dictionary()].
#' @param fallback value used where the dictionary has nothing. `"code"` falls
#'   back to the code itself, which is what keeps an unknown variable readable
#'   on a plot instead of turning into `NA`.
#'
#' @returns a character vector as long as `codes`.
#' @keywords internal
dictionary_lookup <- function(codes, field, dict = NULL, fallback = c("code", "na")) {
  fallback <- match.arg(fallback)
  d <- if (is.data.frame(dict) && all(dictionary_columns() %in% names(dict))) {
    dict
  } else {
    threeme_dictionary(dict)
  }
  if (!field %in% names(d)) {
    stop("`", field, "` is not a column of the dictionary.")
  }
  codes <- as.character(codes)
  out <- d[[field]][match(codes, d$code)]
  if (fallback == "code") out[is.na(out)] <- codes[is.na(out)]
  unname(out)
}

#' Label variable codes
#'
#' The replacement for `label()`: one lookup rather than a code-to-English hop
#' followed by a translation hop, and no dependence on global `language` /
#' `label_database` objects.
#'
#' @param codes character vector of variable codes.
#' @param lang `"en"` or `"fr"`.
#' @param short `TRUE` uses the short label, falling back to the long one where
#'   there is no short form. Use it for table rows and axis text.
#' @param dict passed to [threeme_dictionary()].
#' @param fallback `"code"` (the default) returns the code itself for a variable
#'   the dictionary does not know; `"na"` returns `NA`, which is what
#'   [dictionary_coverage()] uses to find the gaps.
#'
#' @returns a character vector as long as `codes`.
#' @export
#'
#' @examples
#' dict_label(c("GDP", "UNR"))
#' dict_label(c("GDP", "UNR"), lang = "fr")
#' dict_label("NOT_A_VARIABLE")
dict_label <- function(codes, lang = "en", short = FALSE, dict = NULL,
                       fallback = c("code", "na")) {
  fallback <- match.arg(fallback)
  lang <- match.arg(lang, dictionary_languages())
  d <- threeme_dictionary(dict)

  out <- if (short) {
    s <- dictionary_lookup(codes, paste0("label_short_", lang), d, fallback = "na")
    l <- dictionary_lookup(codes, paste0("label_", lang), d, fallback = "na")
    ifelse(is.na(s), l, s)
  } else {
    dictionary_lookup(codes, paste0("label_", lang), d, fallback = "na")
  }
  if (fallback == "code") out[is.na(out)] <- as.character(codes)[is.na(out)]
  out
}

#' Labels as a named vector, ready for `labels =`
#'
#' [simple_plot()] and [table_3me()] both take `labels` as a vector named by
#' variable code. This builds that vector from the dictionary, so labelling a
#' figure is one argument rather than a hand-written lookup.
#'
#' @inheritParams dict_label
#'
#' @returns a character vector named by code.
#' @export
#'
#' @examples
#' dict_labels(c("GDP", "CH", "UNR"), lang = "fr")
dict_labels <- function(codes, lang = "en", short = FALSE, dict = NULL) {
  codes <- as.character(codes)
  stats::setNames(dict_label(codes, lang = lang, short = short, dict = dict), codes)
}

#' Default transformations as a named vector, ready for `transformation =`
#'
#' [table_3me()] accepts a `transformation` named by variable, which is how one
#' table mixes relative differences for volumes with points of difference for
#' rates. The dictionary carries the sensible default per variable, so a mixed
#' table needs no hand-written vector.
#'
#' @param codes character vector of variable codes.
#' @param default transformation for variables the dictionary has no default
#'   for.
#' @param dict passed to [threeme_dictionary()].
#'
#' @returns a character vector named by code.
#' @export
#'
#' @examples
#' dict_transformations(c("GDP", "F_L", "UNR"))
dict_transformations <- function(codes, default = "reldiff", dict = NULL) {
  codes <- as.character(codes)
  out <- dictionary_lookup(codes, "default_transformation", dict, fallback = "na")
  out[is.na(out)] <- default
  stats::setNames(out, codes)
}

#' Variable groups as a named list, ready for `variables =`
#'
#' [table_3me()] takes `variables` as a named list to build row groups. This
#' turns a flat vector of codes into that list using the dictionary's `group`
#' column, preserving the order the codes came in.
#'
#' @param codes character vector of variable codes.
#' @param other group name for variables with no group in the dictionary.
#' @param dict passed to [threeme_dictionary()].
#'
#' @returns a named list of character vectors.
#' @export
#'
#' @examples
#' dict_groups(c("GDP", "CH", "UNR", "F_L"))
dict_groups <- function(codes, other = "Other", dict = NULL) {
  codes <- as.character(codes)
  grp <- dictionary_lookup(codes, "group", dict, fallback = "na")
  grp[is.na(grp)] <- other
  split(codes, factor(grp, levels = unique(grp)))
}

#' Units as a named vector
#'
#' The natural unit of the *level* of each variable, which is a different thing
#' from the unit symbol a transformation produces: `threeme_transformations()`
#' owns the `%` that comes out of a relative difference, while the dictionary
#' owns the `M€` a level is denominated in. Display only — override it per
#' project where the currency or the scale differs.
#'
#' @param codes character vector of variable codes.
#' @param dict passed to [threeme_dictionary()].
#'
#' @returns a character vector named by code.
#' @export
#'
#' @examples
#' dict_units(c("GDP", "F_L", "UNR"))
dict_units <- function(codes, dict = NULL) {
  codes <- as.character(codes)
  stats::setNames(dictionary_lookup(codes, "unit", dict, fallback = "na"), codes)
}

#' Which variables in a dataset the dictionary knows
#'
#' Run it after a model change to see what the dictionary has not caught up
#' with. The gaps are what [dictionary_skeleton()] then writes out for filling
#' in.
#'
#' @param data a long-format ThreeME data frame, or a character vector of codes.
#' @param lang language whose labels must be present to count as covered.
#' @param dict passed to [threeme_dictionary()].
#' @param quiet `TRUE` suppresses the summary message.
#'
#' @returns invisibly, a data frame with one row per code and columns `code`,
#'   `known` (the code is in the dictionary) and `labelled` (it has a label in
#'   `lang`).
#' @export
#'
#' @examples
#' dictionary_coverage(c("GDP", "CH", "NOT_A_VARIABLE"))
dictionary_coverage <- function(data, lang = "en", dict = NULL, quiet = FALSE) {
  lang <- match.arg(lang, dictionary_languages())
  codes <- if (is.data.frame(data)) {
    if (!"variable" %in% names(data)) {
      stop("`data` has no `variable` column. Pass a long-format ThreeME ",
           "data frame, or a character vector of codes.")
    }
    unique(as.character(data$variable))
  } else {
    unique(as.character(data))
  }
  codes <- sort(codes)

  d <- threeme_dictionary(dict)
  out <- data.frame(
    code     = codes,
    known    = codes %in% d$code,
    labelled = !is.na(dict_label(codes, lang = lang, dict = d, fallback = "na")),
    stringsAsFactors = FALSE
  )
  if (!quiet) {
    missing <- out$code[!out$labelled]
    msg <- paste0(sum(out$labelled), "/", nrow(out), " variables have a `",
                  lang, "` label.")
    if (length(missing)) {
      msg <- paste0(msg, "\nMissing: ", paste(utils::head(missing, 20), collapse = ", "),
                    if (length(missing) > 20) paste0(" ... and ", length(missing) - 20, " more"),
                    "\nRun `dictionary_skeleton()` to write them out for filling in.")
    }
    message(msg)
  }
  invisible(out)
}

#' Write a dictionary skeleton for the variables of a model or a dataset
#'
#' Emits a CSV carrying every variable found, with the columns of the
#' dictionary — prefilled where the current dictionary already knows the
#' variable, blank where it does not. The point is that nobody types variable
#' codes by hand: they come from the `.mdl` sources, which
#' [mdl_document()] already parses, or from a dataset that has been run.
#'
#' @param sources,exo,base.path passed to [mdl_document()] to take the variable
#'   list from the model itself. `sources` and `data` are alternatives.
#' @param data a long-format ThreeME data frame, or a character vector of codes,
#'   to take the variable list from instead.
#' @param file path of the CSV to write. `NULL` returns the data frame without
#'   writing.
#' @param index_map how index names map to dimensions, see
#'   [dictionary_index_map()]. This is what fills `indexed_by`: `Y[c, s]` in the
#'   sources becomes `commodity,sector`, so nobody has to know by heart which
#'   variables carry a sector.
#' @param missing_only `TRUE` (the default) writes only the variables the
#'   dictionary does not already label, which is what you want after a model
#'   change. `FALSE` writes the full list.
#' @param dict passed to [threeme_dictionary()].
#'
#' @returns invisibly, the data frame written.
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_skeleton(
#'   sources = c("producer.mdl", "consumer.mdl"),
#'   exo     = "exogenous.mdl",
#'   file    = "data-raw/dictionary-new.csv"
#' )
#' }
dictionary_skeleton <- function(sources = NULL, exo = character(0),
                                base.path = "src/model", data = NULL,
                                file = NULL, missing_only = TRUE,
                                index_map = dictionary_index_map(), dict = NULL) {
  if (is.null(sources) && is.null(data)) {
    stop("Give either `sources` (the .mdl files) or `data` (a dataset) to take ",
         "the variable list from.")
  }
  if (!is.null(sources) && !is.null(data)) {
    stop("Give `sources` or `data`, not both.")
  }

  descriptions <- character(0)
  derived_index <- character(0)
  if (!is.null(sources)) {
    items <- mdl_document(sources = sources, exo = exo, base.path = base.path)
    items <- items[!is.na(items$variable) & nzchar(items$variable), , drop = FALSE]
    ## An exovar row names the variable as `ADJUST[s]`; the dictionary is keyed
    ## on the root code, so drop the bracket.
    ## Index names come from walking every equation, not from the dependent
    ## variable alone: `CH[c]` is determined through `PCH[c] * CH[c] = ...`,
    ## and most prices appear only on right-hand sides, so a dependent-only
    ## scan reports almost everything as unindexed.
    refs <- lapply(items$raw[items$kind == "equation"], function(r) {
      e <- tryCatch(mdl_parse(r), error = function(e) NULL)
      if (is.null(e)) NULL else mdl_variables(e)
    })
    ## An exovar row names the variable as `ADJUST[c, s]`; split it back into
    ## the root code and one row per index.
    exovars <- items$variable[items$kind == "exovar"]
    exovars <- unique(exovars[!is.na(exovars)])
    if (length(exovars)) {
      roots <- sub("\\[.*$", "", exovars)
      idx <- ifelse(grepl("\\[", exovars),
                    sub("^.*\\[(.*)\\]$", "\\1", exovars), NA_character_)
      idx <- strsplit(idx, "\\s*,\\s*")
      refs <- c(refs, list(data.frame(
        name  = rep(roots, lengths(idx)),
        index = unlist(idx, use.names = FALSE),
        stringsAsFactors = FALSE
      )))
    }
    refs <- unique(do.call(rbind, refs[!vapply(refs, is.null, logical(1))]))
    refs$index <- trimws(refs$index)
    refs <- unique(refs)

    codes <- sort(unique(refs$name))

    unmapped <- setdiff(unique(refs$index[!is.na(refs$index)]), names(index_map))
    if (length(unmapped)) {
      warning(length(unmapped), " index name(s) are not in `index_map`, so they ",
              "do not appear in `indexed_by`: ",
              paste(sort(unique(unmapped)), collapse = ", "),
              ".\nExtend it with `index_map = dictionary_index_map(c(",
              sort(unmapped)[1], " = \"...\"))`.", call. = FALSE)
    }

    order_of <- unique(unname(index_map))
    by_code <- split(refs$index, factor(refs$name, levels = codes))
    derived_index <- vapply(by_code, function(idx) {
      dims <- unique(unname(index_map[idx[!is.na(idx)]]))
      dims <- dims[!is.na(dims)]
      if (!length(dims)) return(NA_character_)
      paste(dims[order(match(dims, order_of))], collapse = ",")
    }, character(1))

    ## the prose comment above the equation that defines the variable
    defs <- items[items$kind %in% c("equation", "exovar"), , drop = FALSE]
    defs$variable <- sub("\\[.*$", "", defs$variable)
    defs <- defs[!is.na(defs$variable) & defs$variable %in% codes &
                   !is.na(defs$description) & nzchar(defs$description), , drop = FALSE]
    defs <- defs[!duplicated(defs$variable), , drop = FALSE]
    descriptions <- stats::setNames(defs$description, defs$variable)
  } else {
    codes <- if (is.data.frame(data)) unique(as.character(data$variable))
             else unique(as.character(data))
  }
  codes <- sort(codes)

  d <- threeme_dictionary(dict)
  if (missing_only) {
    codes <- codes[is.na(dict_label(codes, dict = d, fallback = "na"))]
  }

  out <- data.frame(code = codes, stringsAsFactors = FALSE)
  for (col in setdiff(dictionary_columns(), "code")) {
    out[[col]] <- dictionary_lookup(codes, col, d, fallback = "na")
  }
  # the prose comment above an equation is the best first guess at a definition
  if (length(descriptions)) {
    guess <- unname(descriptions[codes])
    guess[!is.na(guess) & !nzchar(guess)] <- NA_character_
    out$definition_en[is.na(out$definition_en)] <- guess[is.na(out$definition_en)]
  }
  # `indexed_by` comes from the model's own index names, never from typing
  if (length(derived_index)) {
    out$indexed_by <- unname(derived_index[codes])
  }

  if (!is.null(file)) {
    utils::write.csv(out, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    message(nrow(out), " variable(s) written to ", file,
            ".\nFill in the label columns, then rerun data-raw/dictionary.R.")
  }
  invisible(out)
}

#' ThreeME variable dictionary (packaged data)
#'
#' One row per root variable code. Edit `data-raw/dictionary.csv` and rerun
#' `data-raw/dictionary.R` to rebuild; read it through [threeme_dictionary()]
#' rather than using this object directly, so that project overrides apply.
#'
#' @format A data frame with one row per variable and the columns of
#'   [dictionary_columns()]:
#' \describe{
#'   \item{code}{root variable code, unindexed.}
#'   \item{label_en, label_fr}{full label.}
#'   \item{label_short_en, label_short_fr}{short label for table rows and axes.}
#'   \item{unit}{natural unit of the level. Display only.}
#'   \item{default_transformation}{one of [threeme_transformations()].}
#'   \item{group}{row group for [table_3me()].}
#'   \item{indexed_by}{which index columns the code can populate: `sector`,
#'     `commodity`, both as `"sector,commodity"`, or empty for none. Derived
#'     from the `.mdl` sources by [dictionary_skeleton()], not typed by hand.
#'     It is not a claim that every row is indexed: most ThreeME variables
#'     exist at the aggregate level too (`GDP` as well as `GDP[c]`), and those
#'     rows carry `NA` in the index column.}
#'   \item{definition_en, definition_fr}{prose definition, for tooltips and the
#'     documentation site.}
#' }
#' @name threeme_dictionary_data
#' @docType data
#' @keywords datasets
#' @usage data(threeme_dictionary_data)
"threeme_dictionary_data"
