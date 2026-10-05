## Reading and editing ThreeME configuration files.
##
## A configuration file is R code, not data: `calib_files` carries a long vector
## threaded with `# ALL VERSIONS` comments and commented-out alternatives, and
## the solver section can hold live code such as `if (...) Sys.setenv(...)`.
## Regenerating such a file from a template would silently throw all of that
## away, so edits here replace one assignment at a time and leave every other
## byte alone.

#' Locate the top-level assignments in an R script
#'
#' @param lines the file, as a character vector of lines.
#'
#' @returns a data frame with `name`, `first` and `last` (line numbers). Where a
#'   name is assigned more than once the last assignment wins, since that is the
#'   one that takes effect.
#' @keywords internal
config_assignments <- function(lines) {
  exprs <- parse(text = lines, keep.source = TRUE)
  refs <- utils::getSrcref(exprs)
  out <- list()
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (!is.call(e) || length(e) < 3L) next
    op <- as.character(e[[1]])
    if (!op %in% c("<-", "=", "<<-")) next
    target <- e[[2]]
    if (!is.name(target)) next
    r <- refs[[i]]
    out[[length(out) + 1L]] <- data.frame(
      name  = as.character(target),
      first = r[1L],
      last  = r[3L],
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) {
    return(data.frame(name = character(0), first = integer(0),
                      last = integer(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, out)
  out[!duplicated(out$name, fromLast = TRUE), , drop = FALSE]
}

#' Replace the value of one assignment, leaving the rest of the file alone
#'
#' @param lines the file, as a character vector of lines.
#' @param name the variable assigned.
#' @param value_code the replacement, as R source text. May span lines.
#' @param add `TRUE` appends the assignment when the file has none. `FALSE`
#'   returns the lines unchanged.
#'
#' @returns the edited lines.
#' @keywords internal
config_set <- function(lines, name, value_code, add = TRUE) {
  asg <- config_assignments(lines)
  new <- paste0(name, " = ", value_code)
  new <- strsplit(paste(new, collapse = "\n"), "\n", fixed = TRUE)[[1]]

  hit <- asg[asg$name == name, , drop = FALSE]
  if (!nrow(hit)) {
    if (!add) return(lines)
    return(c(lines, "", new))
  }
  before <- if (hit$first > 1L) lines[seq_len(hit$first - 1L)] else character(0)
  after  <- if (hit$last < length(lines)) lines[(hit$last + 1L):length(lines)] else character(0)
  c(before, new, after)
}

#' Mark a vector of file names as a file list to edit in place
#'
#' `lists_files`, `calib_files` and `model_files` are written one file per
#' line, with a comment on most lines and the alternatives of the other model
#' versions commented out. Passed to [config_edit()] as a plain vector, such a
#' list is replaced by `c("a", "b")` and all of that is lost. Wrapped in
#' `config_files()`, the assignment in the file is edited line by line instead:
#'
#' * a file that is no longer wanted has its line commented out;
#' * a file that is added is uncommented when the list holds it as a
#'   commented-out alternative, and written on a new line otherwise, after the
#'   file that precedes it;
#' * every other line, comments included, is left as it is.
#'
#' An entry that is code, such as `str_c("R_lists_", iso3, ".mdl")` or
#' `lists_files`, is kept as code for as long as the files it gives are wanted.
#'
#' This needs the list to be a `c()` call with one entry per line, in the order
#' wanted. Otherwise the list is written afresh, one file per line, and the
#' commented lines of the original are kept below the files.
#'
#' @param files character vector of file names, in the order wanted.
#'
#' @returns `files`, with class `config_files`.
#' @export
#'
#' @examples
#' \dontrun{
#' config_edit("configuration/config_input_threeme.R",
#'             list(model_files = config_files(c("lists.mdl", "SU.mdl", "ENDOFLINE.mdl"))))
#' }
config_files <- function(files) {
  structure(as.character(files), class = "config_files")
}

# The environment a configuration is evaluated in: the defaults of
# config_defaults() and the two functions the configs call unqualified.
config_base_env <- function(with = list()) {
  base <- list2env(config_defaults(), envir = new.env(parent = globalenv()))
  base$set_names <- rlang::set_names
  base$str_c <- stringr::str_c
  for (nm in names(with)) assign(nm, with[[nm]], envir = base)
  base
}

# Replace the file list `name` by `files`, keeping the comments of the
# assignment. See config_files().
config_set_files <- function(lines, name, files) {
  files <- as.character(files)
  asg <- config_assignments(lines)
  hit <- asg[asg$name == name, , drop = FALSE]
  plain <- function(comments = character(0)) {
    if (!length(files)) return(c(paste0(name, " = c("), comments, ")"))
    sep <- c(rep(",", length(files) - 1L), "")
    c(paste0(name, " = c("),
      paste0("  ", vapply(files, function(f) deparse(f), character(1), USE.NAMES = FALSE), sep),
      comments, ")")
  }
  if (!nrow(hit)) return(c(lines, "", plain()))

  seg <- lines[hit$first:hit$last]
  new <- tryCatch({
    env <- new.env(parent = config_base_env())
    eval(parse(text = lines, keep.source = FALSE), env)
    config_files_inplace(seg, files, env)
  }, error = function(e) NULL)
  if (is.null(new)) new <- plain(grep("^\\s*#", seg, value = TRUE))

  before <- if (hit$first > 1L) lines[seq_len(hit$first - 1L)] else character(0)
  after  <- if (hit$last < length(lines)) lines[(hit$last + 1L):length(lines)] else character(0)
  c(before, new, after)
}

# The line by line edit of config_set_files(). `seg` holds the lines of the
# assignment, `env` what its entries are evaluated in. Errors when the
# assignment is not laid out one entry per line, or cannot give `files` in
# their order: the caller then writes the list afresh.
config_files_inplace <- function(seg, files, env) {
  expr <- parse(text = seg, keep.source = TRUE)
  rhs <- expr[[1]][[3]]
  if (!is.call(rhs) || !identical(rhs[[1]], quote(c))) stop("not a c() call")
  args <- as.list(rhs)[-1]
  if (!is.null(names(args))) stop("named entries")

  pd <- utils::getParseData(expr)
  pd <- pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2), ]
  fun <- pd[pd$token == "SYMBOL_FUNCTION_CALL", ][1, ]
  if (fun$text != "c") stop("not a c() call")
  call_id <- pd$parent[pd$id == fun$parent]
  kids <- pd[pd$parent == call_id, ]
  open <- kids[kids$token == "'('", ][1, ]
  close <- kids[kids$token == "')'", ]
  close <- close[nrow(close), ]
  where <- kids[kids$token == "expr" & kids$id != fun$parent, ]
  if (nrow(where) != length(args)) stop("entries not found")
  if (any(where$line1 != where$line2) || anyDuplicated(where$line1)) stop("not one entry per line")

  quoted <- function(x) vapply(x, function(f) deparse(f), character(1), USE.NAMES = FALSE)
  indent0 <- sub("^(\\s*).*$", "\\1", seg[1])

  ## One row per line of the assignment. An entry row is `pre`, the entry, then
  ## `post` (what follows its comma); the other rows are kept as text.
  rows <- list()
  add_row <- function(...) rows[[length(rows) + 1L]] <<- list(...)
  for (i in seq_along(seg)) {
    k <- match(i, where$line1)
    if (is.na(k)) { add_row(type = "text", text = seg[i]); next }
    elem <- substring(seg[i], where$col1[k], where$col2[k])
    if (!identical(parse(text = elem)[[1]], args[[k]])) stop("entry not located")
    pre <- substring(seg[i], 1L, where$col1[k] - 1L)
    post <- substring(seg[i], where$col2[k] + 1L)
    head <- NULL
    if (i == open$line1) {
      ## `name = c(` goes on a line of its own, so that its entry can be moved;
      ## the two are joined again at the end if the entry is still the first
      add_row(type = "text", text = sub("\\s+$", "", pre), head = TRUE)
      head <- pre
      pre <- strrep(" ", nchar(pre))
    } else if (nzchar(trimws(pre))) stop("code before an entry")
    tail <- NULL
    if (i == close$line1) {
      ## and so does the closing bracket
      cut <- close$col1 - where$col2[k]
      tail <- paste0(indent0, substring(post, cut))
      post <- substring(post, 1L, cut - 1L)
    }
    post <- sub("^\\s*,", "", post)
    if (nzchar(trimws(post)) && !grepl("^\\s*#", post)) stop("code after an entry")
    add_row(type = "entry", pre = pre, elem = elem, post = post, head = head,
            vals = as.character(eval(args[[k]], env)), on = TRUE)
    if (!is.null(tail)) add_row(type = "text", text = tail)
  }
  is_entry <- function() vapply(rows, function(r) r$type == "entry", logical(1))
  pres <- vapply(rows[is_entry()], function(r) r$pre, character(1))
  indent <- if (length(pres)) pres[length(pres)] else paste0(indent0, "  ")

  ## Entries no longer wanted are switched off; one that gives several files,
  ## of which some are wanted, is replaced by those.
  for (j in which(is_entry())) {
    v <- rows[[j]]$vals
    if (!length(v) || all(v %in% files)) next
    if (any(v %in% files)) {
      rows[[j]]$vals <- v[v %in% files]
      rows[[j]]$elem <- paste(quoted(rows[[j]]$vals), collapse = ", ")
    } else {
      rows[[j]]$on <- FALSE
    }
  }

  ## Files to add: uncomment the line that holds one, or write a new line.
  have <- function() unlist(lapply(rows[is_entry()], function(r) if (r$on) r$vals))
  to_place <- list()
  for (f in setdiff(files, have())) {
    found <- FALSE
    for (j in which(!is_entry())) {
      m <- regmatches(rows[[j]]$text, regexec("^(\\s*)#+\\s*(\"[^\"]*\"|'[^']*')\\s*,?(.*)$", rows[[j]]$text))[[1]]
      if (!length(m) || !identical(substring(m[3], 2L, nchar(m[3]) - 1L), f)) next
      if (nzchar(trimws(m[4])) && !grepl("^\\s*#", m[4])) next
      rows[[j]] <- list(type = "entry", pre = m[2], elem = m[3], post = sub("\\s+$", "", m[4]),
                        vals = f, on = TRUE)
      found <- TRUE
      break
    }
    if (!found) to_place <- c(to_place, list(list(type = "entry", pre = indent, elem = quoted(f),
                                                  post = "", vals = f, on = TRUE)))
  }

  ## The entries already in the order wanted stay where they are (the longest
  ## run of them); the others are taken out and placed again with the new ones.
  key <- function(r) min(match(r$vals, files))
  placed <- which(vapply(rows, function(r) r$type == "entry" && r$on && length(r$vals) > 0L, logical(1)))
  if (length(placed) > 1L) {
    k <- vapply(rows[placed], key, numeric(1))
    len <- rep(1L, length(k)); from <- rep(0L, length(k))
    for (i in seq_along(k)[-1]) for (h in seq_len(i - 1L)) {
      if (k[h] < k[i] && len[h] + 1L > len[i]) { len[i] <- len[h] + 1L; from[i] <- h }
    }
    stay <- integer(0); i <- which.max(len)
    while (i > 0L) { stay <- c(i, stay); i <- from[i] }
    move <- placed[-stay]
    to_place <- c(to_place, rows[move])
    if (length(move)) rows <- rows[-move]
  }
  ## each one after the entry that gives the file before it, or first
  for (r in to_place[order(vapply(to_place, key, numeric(1)))]) {
    prev <- files[seq_len(key(r) - 1L)]
    at <- 0L
    for (j in which(is_entry())) if (rows[[j]]$on && any(rows[[j]]$vals %in% prev)) at <- j
    if (at == 0L) at <- which(vapply(rows, function(x) x$type == "text", logical(1)))[1]
    rows <- append(rows, list(r), after = at)
  }
  if (!identical(as.character(have()), files)) stop("not in the order wanted")

  on <- which(vapply(rows, function(r) r$type == "entry" && r$on, logical(1)))
  last_on <- if (length(on)) on[length(on)] else 0L
  out <- character(0)
  for (j in seq_along(rows)) {
    r <- rows[[j]]
    if (isTRUE(r$head) && j < length(rows) && isTRUE(rows[[j + 1L]]$on) &&
        !is.null(rows[[j + 1L]]$head)) {
      rows[[j + 1L]]$pre <- rows[[j + 1L]]$head
      next
    }
    out <- c(out, if (r$type == "text") r$text
             else if (!r$on) paste0(r$pre, "# ", r$elem, ",", r$post)
             else paste0(r$pre, r$elem, if (j != last_on) ",", r$post))
  }
  parse(text = out)
  out
}

#' Apply several edits to a configuration file
#'
#' @param file path to the configuration file.
#' @param values a named list of replacements. Each element is either R source
#'   text (wrap it in [I()] to say "this is already code") or a value to be
#'   deparsed. A list of files wrapped in [config_files()] is edited in place
#'   instead, so that its comments and commented-out alternatives survive.
#' @param out where to write. Defaults to `file`, editing in place.
#'
#' @returns invisibly, the path written.
#' @export
#'
#' @examples
#' \dontrun{
#' config_edit("configuration/config_input_threeme.R",
#'             list(project_name = "my_run", shockyear = 2025))
#' }
config_edit <- function(file, values, out = file) {
  if (!file.exists(file)) cli::cli_abort("no such file: {.file {file}}")
  lines <- readLines(file, warn = FALSE)
  for (nm in names(values)) {
    v <- values[[nm]]
    if (inherits(v, "config_files")) {
      lines <- config_set_files(lines, nm, unclass(v))
      next
    }
    code <- if (inherits(v, "AsIs")) as.character(v) else paste(deparse(v), collapse = "\n")
    lines <- config_set(lines, nm, code)
  }
  ## a last parse check, so a bad edit fails here rather than at run time
  ok <- tryCatch({ parse(text = lines); TRUE }, error = function(e) conditionMessage(e))
  if (!isTRUE(ok)) cli::cli_abort(c("the edit would produce a file that does not parse:", " " = "{ok}"))
  writeLines(lines, out)
  invisible(out)
}

#' The configuration fields this package knows how to edit
#'
#' Held as data so the addin builds its controls from it rather than repeating
#' the list. Anything not named here is carried through an edit untouched.
#'
#' The solver options are split in three sections, one tab each in the addin:
#' `Solver` for what applies to both solvers, `R solver` and `EViews solver`
#' for what only one of them reads. Their defaults are in
#' [config_solver_defaults()].
#'
#' Together with the fields the addin handles with controls of its own
#' (`config_fields_custom()`: the scenarios, their folders, the variables to
#' keep and the file lists), this covers every option of [config_required()]
#' and [config_defaults()]. A test holds the two in step, so an option added
#' to the configuration cannot be left out of the addin.
#'
#' @returns a data frame with `name`, `section`, `type` (`text`, `number`,
#'   `bool`, `choice` or `multi`), `label` and `choices`. `choices` is `NA`
#'   except for a `choice` field (one value) or a `multi` one (any number),
#'   where it holds the allowed values separated by `|`.
#' @export
#'
#' @examples
#' config_fields()
config_fields <- function() {
  f <- function(name, section, type, label, choices = NA_character_) {
    data.frame(name = name, section = section, type = type, label = label,
               choices = choices, stringsAsFactors = FALSE)
  }
  rbind(
    f("project_name",     "Basics", "text",   "Project name"),
    f("iso3",             "Basics", "text",   "Country (ISO3)"),
    f("classification",   "Basics", "text",   "Classification"),
    f("model_folder",     "Basics", "text",   "Model folder"),
    f("baseyear",         "Basics", "number", "Base year"),
    f("lastyear",         "Basics", "number", "Last year"),
    f("shockyear",        "Basics", "number", "Shock year"),
    f("max_lags",         "Basics", "number", "Maximum lag"),
    f("automated_shocks", "Basics", "bool",   "Automated shocks"),

    f("Rsolver",               "Solver", "bool",   "Solve in R (off = EViews, Windows only)"),
    f("warning",               "Solver", "bool",   "Solver warning messages"),
    f("tolerance_calib_check", "Solver", "number", "Calibration check tolerance"),
    f("skip_dynamo",           "Solver", "bool",
      "Advanced, leave unticked: skip DynaMo and reuse the model.prg and calib.csv in src/compiler (changes to the .mdl files are then ignored)"),
    f("save_files_res",        "Solver", "bool",   "Save all result files"),
    f("output_saved",          "Solver", "multi",  "Aggregated databases to save",
      "com|sec|sec_com"),

    f("Rcpp",              "R solver", "bool",
      "Compiled solver, Rcpp (recommended). Off = pure R, no compiler needed"),
    f("recompile_model_R", "R solver", "bool",
      "Rebuild the model even if its equations have not changed"),
    f("Rsolver_decompose", "R solver", "bool",
      "Split the model into prologue, heart and epilogue"),
    f("Rsolver_sequential", "R solver", "bool",
      "Solve the prologue and the epilogue one equation at a time (not faster; names the equation that fails)"),
    f("Rsolver_reuse_jacobian", "R solver", "choice",
      "Reuse the jacobian between Newton iterations", "auto|TRUE|FALSE"),
    f("Rsolver_rtol",      "R solver", "number", "Relative tolerance"),
    f("Rsolver_atol",      "R solver", "number", "Absolute tolerance"),
    f("Rsolver_max_iter",  "R solver", "number", "Maximum Newton iterations per period"),
    f("Rsolver_damping",   "R solver", "bool",   "Damp the Newton steps"),
    f("Rsolver_verbose",   "R solver", "bool",   "Print the solver's progress"),
    f("Rsolver_timings",   "R solver", "bool",
      "Print how long the translation, the build and each solve took"),

    f("path_eviews_exe",  "EViews solver", "text",   "Path to EViews.exe"),
    f("eviews_timeout",   "EViews solver", "number", "EViews timeout (s, 0 = none)"),
    f("recompile_model_eviews", "EViews solver", "bool",
      "Reload the model, the calibration and the baseline (untick only to reopen an unchanged workfile)"),
    f("eviews_algorithm", "EViews solver", "choice", "Solution algorithm",
      paste(eviews_algorithms(), collapse = "|")),
    f("eviews_digits",    "EViews solver", "number", "Digits the solution is rounded to"),
    f("eviews_max_iter",  "EViews solver", "number", "Maximum number of iterations")
  )
}

#' @rdname config_fields
#' @returns `config_fields_custom()` returns the names of the options the
#'   addin edits outside the field table.
#' @export
config_fields_custom <- function() {
  c("scenario_baseline", "scenario", "baseline_scenario_folder",
    "shock_scenario_folder", "variables_to_keep",
    "lists_files", "calib_files", "model_files")
}

#' Read the values a configuration file assigns
#'
#' Sources the file in a throwaway environment, so the values come back exactly
#' as the pipeline would see them -- `firstyear` computed from `baseyear`,
#' `calib_files` built from `iso3`, and so on. The defaults of
#' [config_defaults()] are in scope while it is sourced, so an incomplete file
#' that uses an option it does not set still reads; only what the file
#' assigns itself is returned.
#'
#' @param file path to a configuration input file.
#' @param fields which names to return. Defaults to everything the file assigns.
#' @param with a named list injected before sourcing. An **output**
#'   configuration is not self-contained -- `quartos_parameters` refers to
#'   `project_name`, `lastyear` and `scenario`, which the *input* configuration
#'   defines -- so reading one on its own fails unless the input values are
#'   passed in here.
#'
#' @returns a named list.
#' @export
#'
#' @examples
#' \dontrun{
#' vals <- read_config_values("configuration/config_input_threeme.R")
#' read_config_values("configuration/config_output_threeme.R", with = vals)
#' }
read_config_values <- function(file, fields = NULL, with = list()) {
  if (!file.exists(file)) cli::cli_abort("no such file: {.file {file}}")
  ## The configs call set_names() and str_c() without qualifying them.
  env <- new.env(parent = config_base_env(with))
  sys.source(file, envir = env, keep.source = FALSE)
  nms <- if (is.null(fields)) ls(env) else fields
  out <- lapply(nms, function(n) if (exists(n, envir = env, inherits = FALSE)) get(n, envir = env) else NULL)
  stats::setNames(out, nms)
}

#' Find the configuration files of a project
#'
#' @param path the configuration folder.
#'
#' @returns a data frame with `name`, `kind` (`input` or `output`) and `file`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_configs()
#' }
list_configs <- function(path = "configuration") {
  if (!dir.exists(path)) {
    return(data.frame(name = character(0), kind = character(0),
                      file = character(0), stringsAsFactors = FALSE))
  }
  files <- list.files(path, pattern = "^config_(input|output)_.*\\.R$")
  if (!length(files)) {
    return(data.frame(name = character(0), kind = character(0),
                      file = character(0), stringsAsFactors = FALSE))
  }
  kind <- ifelse(grepl("^config_input_", files), "input", "output")
  name <- sub("^config_(input|output)_(.*)\\.R$", "\\2", files)
  data.frame(name = name, kind = kind, file = file.path(path, files),
             stringsAsFactors = FALSE)
}

#' Is this a ThreeME project?
#'
#' The addins edit files at fixed paths inside a ThreeME v4 project --
#' `configuration/`, `configuration/scenarii_calib/`, `src/model/` -- so opening
#' one from an unrelated working directory can only misfire. This is the check
#' they run before doing anything.
#'
#' @param path the project root.
#'
#' @returns a list with `ok` (logical) and `missing` (the expected folders that
#'   are not there).
#' @export
#'
#' @examples
#' is_threeme_project(tempdir())
is_threeme_project <- function(path = ".") {
  expected <- c("configuration",
                file.path("configuration", "scenarii_calib"),
                "src")
  missing <- expected[!dir.exists(file.path(path, expected))]
  list(ok = length(missing) == 0L, missing = missing)
}

#' Stop unless the working directory is a ThreeME project
#'
#' @param path the project root.
#' @param what the addin name, for the message.
#'
#' @returns invisibly `TRUE`, or an error.
#' @keywords internal
check_threeme_project <- function(path = ".", what = "This addin") {
  chk <- is_threeme_project(path)
  if (chk$ok) return(invisible(TRUE))
  cli::cli_abort(c("{what} only works inside a ThreeME v4 project.",
                   "x" = "Missing from {.path {normalizePath(path, mustWork = FALSE)}}: {.file {chk$missing}}.",
                   "i" = "Open the ThreeME project (its {.file .Rproj}) and try again."),
                 call = NULL)
}

#' A warning banner for an addin opened outside a ThreeME project
#'
#' @param path the project root.
#'
#' @returns a shiny tag, or `NULL` when the project looks right.
#' @keywords internal
project_banner <- function(path = ".") {
  chk <- is_threeme_project(path)
  if (chk$ok) return(NULL)
  shiny::div(
    class = "alert alert-danger",
    shiny::tags$b("This is not a ThreeME v4 project."),
    shiny::br(),
    "These addins read and write files at fixed paths inside a ThreeME ",
    "project. Missing here: ",
    shiny::tags$code(paste(chk$missing, collapse = ", ")), ".",
    shiny::br(),
    shiny::tags$small("Open the ThreeME project (its .Rproj) and launch the addin again.")
  )
}

#' Stop an addin when its window is closed
#'
#' An addin runs inside [shiny::runGadget()], which only returns once the app
#' is stopped. Closing the window with its cross instead of the addin's own
#' button ends the browser session but not the app: R stays busy, waiting on a
#' window that is gone, until it is interrupted or restarted.
#'
#' @param session the shiny session.
#'
#' @returns called for its side effect.
#' @keywords internal
stop_on_close <- function(session) {
  session$onSessionEnded(function() {
    if (shiny::isRunning()) shiny::stopApp(invisible(NULL))
  })
}
