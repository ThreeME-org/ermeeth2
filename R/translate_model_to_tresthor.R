## Translating a compiled model into a thoR model.
##
## The ThreeME compiler emits two files: `model.prg`, a list of
## `<model>.append <equation>` lines in EViews syntax, and `calib.csv`, the
## calibration database whose `year` column is relative to the base year.
## `prg_to_thor()` turns them into the pieces the solver needs -- the equation
## list, the endogenous/exogenous/coefficient split, and a database with the
## `@elem` coefficients already broadcast onto it as constant columns.
##
## ---------------------------------------------------------------------------
## What changed relative to the first implementation
##
## The output conventions are unchanged, so a model translated either way lands
## in the same place: `@elem` becomes a coefficient named
## `elem_<expression>_<year>`, and an equation's endogenous variable is the
## first one on its left-hand side. Five things behave differently, each
## because the original has a failure mode on real compiler output:
##
##  1. `@elem` is found by scanning balanced parentheses rather than by three
##     regexes for the three shapes seen so far. The original matched
##     `@elem(<word>,<year>)`, `@elem(<word>(-<k>),<year>)` and
##     `@elem(<word><op><word>,<year>)`; anything else -- two operators, a
##     function call, a parenthesised sub-expression -- was silently left in
##     place and reached the solver as an undefined variable.
##
##  2. The value comes from *evaluating* the inner expression against the
##     calibration row, not from a switch over the four arithmetic operators.
##     Any expression the compiler can emit therefore works, through one code
##     path instead of three.
##
##  3. Occurrences are keyed on (expression, year) and substituted as literal
##     text, longest first. The original keyed on the variable name alone, so
##     two `@elem` of the same variable at different years collided and both
##     took the value of whichever was seen last.
##
##  4. `rbind(elem_table_1, elem_table_2)` errored outright on a model that
##     used lagged `@elem` but no plain ones, or the reverse, because the
##     missing table was never created. There is one table here.
##
##  5. Comparisons. EViews lets an equation contain a logical test that
##     evaluates to 0 or 1; the solver has no comparison operators. The
##     original handled this with three hard-coded ThreeME substitutions that
##     pin a named test to a literal 1 or 0, plus a generic rewrite matching
##     only `<word><op><word><cmp><number>`. Every test now goes through one
##     generic rewrite, so none is silently pinned to a constant and no shape
##     is missed. See [prg_rewrite_comparisons()].
## ---------------------------------------------------------------------------


#' Functions a model equation may contain
#'
#' @description The same list the solvers support. Kept here so that
#'   [prg_variables()] does not have to reach into another package for it.
#'
#' @returns A character vector of function names.
#' @keywords internal
prg_functions <- function() {
  c("abs", "acos", "acosh", "asin", "asinh", "atan", "atanh", "cos", "cosh",
    "exp", "expm1", "log", "log10", "log1p", "log2", "logb", "sign", "sin",
    "sinh", "sqrt", "tan", "tanh")
}


#' Every variable name occurring in an expression
#'
#' @description Deliberately a local implementation rather than a call to
#'   `tresthor::get_variables_from_string()`, which cannot be called qualified:
#'   it reads `thor_functions_supported`, a `LazyData` dataset that is only
#'   visible once `tresthor` is *attached*, so `tresthor::` access fails with
#'   "object 'thor_functions_supported' not found". Attaching a package from
#'   inside another one is not an option, and the logic is a dozen lines.
#'
#'   It matches `thortwo::get_variables_from_string()`, which does work
#'   qualified, so a model classified here is classified the same way by the
#'   solver.
#'
#' @param string a single character string.
#'
#' @returns A character vector of the names found.
#' @keywords internal
prg_variables <- function(string) {
  string <- gsub("\\s+", "", string)
  fn <- c(prg_functions(), toupper(prg_functions()),
          "delta", "newdiff", "lag", "mylg")
  function_pattern <- paste(paste0(fn, "\\("), collapse = "|")
  symbol_pattern   <- "(\\+|\\*|,|-|/|\\^|\\(|\\)|=)|\\\\"

  x <- gsub(function_pattern, "@", string)
  x <- gsub(symbol_pattern, "@", x)
  x <- gsub("@+", ",", x)
  x <- gsub(",[0-9]+(\\.[0-9]+)?", ",", x)
  x <- gsub("^[0-9]+(\\.[0-9]+)?", "", x)
  x <- gsub("^,|,$", "", x)

  out <- unique(strsplit(x, ",", fixed = TRUE)[[1L]])
  out[nzchar(out)]
}


#' Rewrite EViews logical tests as smooth indicators
#'
#' @description `(A < B)` evaluates to 1 or 0 in EViews. The solver has no
#'   comparison operators -- and a step function has no useful derivative --
#'   so each test becomes the algebraic indicator
#'
#'   \preformatted{
#'   A > B   ->   0.5 * (1 + (A-B) / (|A-B| + eps))
#'   A < B   ->   0.5 * (1 - (A-B) / (|A-B| + eps))
#'   }
#'
#'   which is exactly 1 or 0 away from the crossing, and whose derivative there
#'   is `eps/(|A-B|+eps)^2`, i.e. numerically zero. `>=` and `<=` are treated as
#'   `>` and `<`; they differ only on the measure-zero set `A == B`, where this
#'   form returns 0.5 rather than the 0/0 produced by the difference-quotient
#'   version used previously.
#'
#'   Right at the crossing the indicator is not differentiable, so a model
#'   sitting exactly on a threshold can stall. Nothing in a translation can fix
#'   that: it is a property of writing a discontinuity into a model that is then
#'   solved by Newton.
#'
#'   Each test must be parenthesised on its own, which is how the compiler emits
#'   them; anything else raises an error rather than being guessed at.
#'
#' @param eqs character vector of equations.
#'
#' @returns A list with `eqs`, the rewritten equations, and `n`, the number of
#'   tests rewritten.
#' @keywords internal
prg_rewrite_comparisons <- function(eqs) {

  eps <- "1e-30"
  n <- 0L

  for (k in seq_along(eqs)) {
    repeat {
      m <- regexpr("(<=|>=|<|>)", eqs[k])
      if (m == -1L) break
      op  <- regmatches(eqs[k], m)
      pos <- as.integer(m)
      len <- attr(m, "match.length")
      s   <- eqs[k]

      ## innermost enclosing parentheses
      left <- 0L; depth <- 0L
      for (i in seq(pos - 1L, 1L)) {
        ch <- substr(s, i, i)
        if (ch == ")") depth <- depth + 1L
        else if (ch == "(") {
          if (depth == 0L) { left <- i; break }
          depth <- depth - 1L
        }
      }
      right <- 0L; depth <- 0L
      for (i in seq(pos + len, nchar(s))) {
        ch <- substr(s, i, i)
        if (ch == "(") depth <- depth + 1L
        else if (ch == ")") {
          if (depth == 0L) { right <- i; break }
          depth <- depth - 1L
        }
      }
      if (left == 0L || right == 0L) {
        cli::cli_abort(c("A comparison is not enclosed in its own parentheses, so its operands cannot be identified:",
                         " " = "{s}"), call = NULL)
      }

      A <- substr(s, left + 1L, pos - 1L)
      B <- substr(s, pos + len, right - 1L)
      sgn <- if (substr(op, 1L, 1L) == ">") "+" else "-"
      diff <- paste0("((", A, ")-(", B, "))")
      new  <- sprintf("(0.5*(1.0%s%s/(abs%s+%s)))", sgn, diff, diff, eps)

      eqs[k] <- paste0(substr(s, 1L, left - 1L), new, substring(s, right + 1L))
      n <- n + 1L
    }
  }
  list(eqs = eqs, n = n)
}


#' Expand scientific notation into plain decimals
#'
#' @description `1e-05` survives R's parser but not the solver's variable
#'   extractor, which reads the stray `e` as a variable name.
#'
#' @param eqs character vector of equations.
#'
#' @returns The equations, with every numeric literal written out in full.
#' @keywords internal
prg_expand_scientific <- function(eqs) {
  pattern <- "[0-9]+(\\.[0-9]+)?e[-+]?[0-9]+"
  out <- eqs
  for (k in seq_along(out)) {
    lits <- regmatches(out[k], gregexpr(pattern, out[k], perl = TRUE))[[1]]
    for (lit in unique(lits)) {
      out[k] <- gsub(lit, format(as.numeric(lit), scientific = FALSE, digits = 17),
                     out[k], fixed = TRUE)
    }
  }
  out
}


#' Find every `@elem()` occurrence in a set of equations
#'
#' @description A regular expression cannot do this:
#'   `@elem(pk_scon(-1), 2019)` closes on its second `)`, not its first.
#'
#' @param x character vector of equations.
#'
#' @returns A character vector of the distinct occurrences, as written.
#' @keywords internal
prg_find_elem <- function(x) {
  out <- character(0)
  for (line in x) {
    start <- gregexpr("@elem(", line, fixed = TRUE)[[1]]
    if (start[1] == -1L) next
    for (s in start) {
      depth <- 0L
      i <- s + 5L                       # position of the opening "("
      n <- nchar(line)
      repeat {
        ch <- substr(line, i, i)
        if (ch == "(") depth <- depth + 1L
        if (ch == ")") {
          depth <- depth - 1L
          if (depth == 0L) break
        }
        i <- i + 1L
        if (i > n) cli::cli_abort(c("Unbalanced parentheses in:", " " = "{line}"), call = NULL)
      }
      out <- c(out, substr(line, s, i))
    }
  }
  unique(out)
}


#' Split an `@elem()` occurrence into its expression and its year
#'
#' @param e a single occurrence, as returned by [prg_find_elem()].
#'
#' @returns A list with `expr` and `year`.
#' @keywords internal
prg_parse_elem <- function(e) {
  inner <- substr(e, 7L, nchar(e) - 1L)          # strip "@elem(" and ")"
  at <- max(gregexpr(",", inner, fixed = TRUE)[[1]])
  if (at == -1L) cli::cli_abort(c("{.code @elem} without a year:", " " = "{e}"), call = NULL)
  list(expr = trimws(substr(inner, 1L, at - 1L)),
       year = as.integer(trimws(substring(inner, at + 1L))))
}


#' Name the coefficient an `@elem()` becomes
#'
#' @description Follows the convention already present in the translated
#'   models: `@elem(chd_cind/chm_cind, 2015)` becomes
#'   `elem_chd_cind_chm_cind_2015`, and a lag is folded into the year, so
#'   `@elem(pk_sind(-1), 2019)` becomes `elem_pk_sind_2018`.
#'
#' @param expr the inner expression.
#' @param year the year.
#'
#' @returns A valid lower-case variable name.
#' @keywords internal
prg_elem_name <- function(expr, year) {
  e <- gsub("\\s+", "", expr)
  lag_only <- regmatches(e, regexec("^([a-z][a-z0-9_]*)\\(-([0-9]+)\\)$", e))[[1]]
  if (length(lag_only) == 3L) {
    return(sprintf("elem_%s_%d", lag_only[2], year - as.integer(lag_only[3])))
  }
  slug <- gsub("_+", "_", gsub("[^a-z0-9_]", "_", tolower(e)))
  sprintf("elem_%s_%d", gsub("^_|_$", "", slug), year)
}


#' Evaluate an EViews expression against one row of the calibration
#'
#' @description Variable reads become lookups by year, so a lag inside the
#'   expression is honoured: in `@elem(a/b(-1), 2019)`, `a` is read at 2019 and
#'   `b` at 2018.
#'
#' @param expr the inner expression.
#' @param year the year to read at.
#' @param calib the calibration data.frame, with an absolute `year` column.
#'
#' @returns A numeric scalar, or `NA` if a variable is absent from `calib`.
#' @keywords internal
prg_eval_elem <- function(expr, year, calib) {

  e <- gsub("\\s+", "", expr)
  ## `x(-k)` -> `.v("x", year - k)`
  e <- gsub("([a-z][a-z0-9_]*)\\(-([0-9]+)\\)", '.v("\\1",.y-\\2)', e)
  ## remaining bare names -> `.v("x", year)`, skipping function calls and
  ## anything already rewritten
  e <- gsub('(?<![."a-z0-9_])([a-z][a-z0-9_]*)(?!\\s*\\(|["a-z0-9_])',
            '.v("\\1",.y)', e, perl = TRUE)

  .v <- function(nm, yr) {
    if (!nm %in% names(calib)) return(NA_real_)
    row <- which(calib$year == yr)
    if (length(row) != 1L) return(NA_real_)
    as.numeric(calib[[nm]][row])
  }
  env <- list2env(list(.v = .v, .y = year), parent = baseenv())

  tryCatch(eval(parse(text = e, keep.source = FALSE), envir = env),
           error = function(err) NA_real_)
}


#' Translate a compiled model into a thoR model
#'
#' @description Reads the compiled `model.prg` and its calibration csv and
#'   turns them into the pieces the solver needs: the equation list, the
#'   endogenous / exogenous / coefficient split, and the data with the `@elem`
#'   coefficients broadcast onto it as constant columns.
#'
#'   The result feeds `thortwo::thor_model()` by way of `out_file`, or
#'   `tresthor::create_model()` directly from `endo` / `exo` / `coef` /
#'   `equations`. Nothing here depends on either package.
#'
#'   Supersedes [translate_modelprg()]; see the file header for what differs.
#'
#' @param modfile character. Path to the compiled model program.
#' @param calibfile character. Path to the calibration csv, whose `year` column
#'   is relative to `base.year`.
#' @param base.year numeric. The calendar year that relative year 0 is.
#'   Required: unlike [translate_modelprg()], it does not default to a global.
#' @param last.year numeric. Optional upper bound on the database.
#' @param first.year numeric. Optional lower bound on the database.
#' @param out_file character. Optional path to also write the model out as a
#'   `.txt`, in the four-section format the solver's parser reads.
#' @param model_prefix character. Regular expression matching the EViews model
#'   object name prefixed to each `.append` line.
#' @param verbose logical. Report progress.
#'
#' @returns A list with `equations`, `endo`, `exo`, `coef`, `data`, `elem` (the
#'   coefficient table), `warnings` and `file`.
#' @export
#'
#' @examples
#' \dontrun{
#' tr <- prg_to_thor("src/compiler/model.prg", "src/compiler/calib.csv",
#'                   base.year = 2019)
#' translate_report(tr)
#' }
prg_to_thor <- function(modfile = file.path("src", "compiler", "model.prg"),
                        calibfile = file.path("src", "compiler", "calib.csv"),
                        base.year,
                        last.year = NULL,
                        first.year = NULL,
                        out_file = NULL,
                        model_prefix = "^[a-z_0-9]+\\.append",
                        verbose = TRUE) {

  step   <- function(...) if (verbose) cli::cli_alert_info(cli_escape(paste0(...)))
  detail <- function(...) if (verbose) cli::cli_verbatim(paste0("  ", ...))
  warnings_out <- character(0)
  warn <- function(msg) {
    warnings_out <<- c(warnings_out, msg)
    if (verbose) cli::cli_alert_warning("{msg}")
  }

  if (missing(base.year)) cli::cli_abort("{.arg base.year} is required.", call = NULL)
  stopifnot(file.exists(modfile), file.exists(calibfile))

  ## ---- 1. equations -------------------------------------------------------
  step("Reading ", basename(modfile))
  eqs <- tolower(readLines(modfile, warn = FALSE))
  eqs <- sub(model_prefix, "", eqs)
  eqs <- gsub("'.*$", "", eqs)                    # EViews end-of-line comments
  eqs <- gsub("\\s+", "", eqs)
  eqs <- eqs[nzchar(eqs)]
  detail("", length(eqs), " equations")

  ## ---- 2. calibration -----------------------------------------------------
  step("Reading ", basename(calibfile))
  ## fread, not read.csv: the calibration is all columns (85,000 on FRA 29x33),
  ## and read.csv's cost grows with their number -- 169 s against 0.9 s there.
  ## `integer64 = "double"` keeps a large whole number a plain numeric.
  calib <- data.table::fread(calibfile, data.table = FALSE, integer64 = "double")
  names(calib) <- tolower(names(calib))
  calib <- calib[, names(calib) != "baseyear", drop = FALSE]
  if (!"year" %in% names(calib)) {
    cli::cli_abort("The calibration file has no {.field year} column.", call = NULL)
  }
  calib$year <- as.integer(round(calib$year)) + base.year
  if (!is.null(first.year)) calib <- calib[calib$year >= first.year, , drop = FALSE]
  if (!is.null(last.year))  calib <- calib[calib$year <= last.year,  , drop = FALSE]
  rownames(calib) <- NULL
  detail("", nrow(calib), " periods (", min(calib$year), "-", max(calib$year),
      "), ", ncol(calib) - 1L, " variables")

  ## ---- 3. @elem -> coefficients ------------------------------------------
  step("Resolving @elem")
  occ <- prg_find_elem(eqs)
  elem <- NULL

  if (length(occ)) {
    parsed <- lapply(occ, prg_parse_elem)
    elem <- data.frame(
      original = occ,
      expr     = vapply(parsed, `[[`, character(1), "expr"),
      year     = vapply(parsed, `[[`, integer(1),   "year"),
      stringsAsFactors = FALSE
    )
    elem$name  <- mapply(prg_elem_name, elem$expr, elem$year, USE.NAMES = FALSE)
    elem$value <- mapply(prg_eval_elem, elem$expr, elem$year,
                         MoreArgs = list(calib = calib), USE.NAMES = FALSE)

    detail("", nrow(elem), " distinct @elem -> ",
        length(unique(elem$name)), " coefficients")

    bad <- elem[is.na(elem$value), , drop = FALSE]
    if (nrow(bad)) {
      warn(sprintf("%d @elem could not be evaluated against the calibration (e.g. %s)",
                   nrow(bad), paste(utils::head(bad$original, 3), collapse = ", ")))
    }

    ## Two occurrences reducing to the same name must reduce to the same
    ## number, or the name is ambiguous.
    dup <- tapply(elem$value, elem$name, function(v) length(unique(round(v, 12))) > 1L)
    if (any(dup, na.rm = TRUE)) {
      warn(sprintf("coefficient name(s) map to more than one value: %s",
                   paste(names(dup)[which(dup)], collapse = ", ")))
    }

    ## Substituted as literal text, longest first, so no occurrence can be
    ## rewritten inside another one.
    for (k in order(nchar(elem$original), decreasing = TRUE)) {
      eqs <- gsub(elem$original[k], elem$name[k], eqs, fixed = TRUE)
    }

    left <- sum(grepl("@elem", eqs, fixed = TRUE))
    if (left) warn(sprintf("%d @elem left in the equations after substitution", left))

    ## onto the database, as constant columns
    coeff_tbl <- elem[!duplicated(elem$name), c("name", "value")]
    calib <- calib[, setdiff(names(calib), coeff_tbl$name), drop = FALSE]
    calib <- cbind(calib,
                   as.data.frame(matrix(rep(coeff_tbl$value, each = nrow(calib)),
                                        nrow = nrow(calib),
                                        dimnames = list(NULL, coeff_tbl$name))))
  } else {
    detail("none")
  }

  ## ---- 4. EViews -> thoR syntax ------------------------------------------
  step("Rewriting lags and differences")
  ## `x(-1)` -> `lag(x,1)`, before `d(` so a lag inside a difference is
  ## already in thoR form
  eqs <- gsub("([a-z][a-z0-9_]*)\\(-([0-9]+)\\)", "lag(\\1,\\2)", eqs)
  ## `d(...)` -> `delta(1,...)`, but not the `d` ending an identifier
  eqs <- gsub("(?<![a-z0-9_])d\\(", "delta(1,", eqs, perl = TRUE)
  if (any(grepl("delta(1,og(", eqs, fixed = TRUE))) {
    cli::cli_abort("{.fn dlog} is not handled by this translator.", call = NULL)
  }
  eqs <- gsub("+-", "-", eqs, fixed = TRUE)

  n_cmp <- sum(grepl("[<>]", eqs))
  if (n_cmp) {
    rc <- prg_rewrite_comparisons(eqs)
    eqs <- rc$eqs
    detail("rewrote ", rc$n, " logical test(s) in ", n_cmp,
        " equation(s) as indicators")
  }
  eqs <- prg_expand_scientific(eqs)

  leftover <- grep("@|<|>", eqs)
  if (length(leftover)) {
    warn(sprintf("%d equation(s) still contain @, < or > and will not parse (e.g. %s)",
                 length(leftover), utils::head(eqs[leftover], 1)))
  }

  ## ---- 5. classify the variables -----------------------------------------
  step("Classifying variables")
  ## The endogenous variable of an equation is the first one on its left-hand
  ## side: the compiler emits equations already normalised that way.
  lhs <- sub("=.*$", "", eqs)
  endo <- vapply(lhs, function(s) {
    v <- prg_variables(s)
    if (length(v) == 0L) NA_character_ else v[1L]
  }, character(1), USE.NAMES = FALSE)

  if (anyNA(endo)) {
    warn(sprintf("%d equation(s) have no variable on the left-hand side",
                 sum(is.na(endo))))
    endo <- endo[!is.na(endo)]
  }
  if (anyDuplicated(endo)) {
    twice <- unique(endo[duplicated(endo)])
    warn(sprintf("%d variable(s) are the left-hand side of more than one equation (e.g. %s)",
                 length(twice), paste(utils::head(twice, 3), collapse = ", ")))
  }

  all_vars <- prg_variables(paste(eqs, collapse = "+"))
  coef <- if (is.null(elem)) character(0) else sort(unique(elem$name))
  coef <- intersect(coef, all_vars)
  endo <- sort(unique(endo))
  exo  <- sort(setdiff(all_vars, c(endo, coef)))

  detail("", length(endo), " endogenous, ", length(exo), " exogenous, ",
      length(coef), " coefficients")
  if (length(endo) != length(eqs)) {
    warn(sprintf("%d equations for %d endogenous variables: the model is not square",
                 length(eqs), length(endo)))
  }

  missing_vars <- setdiff(c(endo, exo, coef), names(calib))
  if (length(missing_vars)) {
    warn(sprintf("%d model variable(s) are absent from the calibration (e.g. %s)",
                 length(missing_vars),
                 paste(utils::head(missing_vars, 5), collapse = ", ")))
  }

  ## ---- 6. optionally write the model file --------------------------------
  if (!is.null(out_file)) {
    step("Writing ", basename(out_file))
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(c(
      "endogenous variables :", paste(endo, collapse = ","), "##############",
      "exogenous variables :",  paste(exo,  collapse = ","), "##############",
      "coefficients :",         paste(coef, collapse = ","), "##############",
      "equations :",            eqs), out_file)
  }

  list(equations = eqs, endo = endo, exo = exo, coef = coef,
       data = calib, elem = elem, warnings = warnings_out, file = out_file)
}


#' Report what a translation did and what it could not do
#'
#' @param tr the value of [prg_to_thor()].
#'
#' @returns `tr`, invisibly. Called for the report it prints.
#' @export
translate_report <- function(tr) {
  cli::cli_h3("Translation report")
  cli::cli_dl(c(
    equations    = length(tr$equations),
    endogenous   = length(tr$endo),
    exogenous    = length(tr$exo),
    coefficients = length(tr$coef),
    database     = paste(nrow(tr$data), "periods x", ncol(tr$data) - 1L, "variables")))
  if (length(tr$warnings) == 0L) {
    cli::cli_alert_success("No warnings.")
  } else {
    for (w in tr$warnings) cli::cli_alert_warning("{w}")
  }
  invisible(tr)
}


#' Translate a compiled model into a solver model
#'
#' @description Superseded by [prg_to_thor()], which this calls. Kept so that
#'   existing scripts keep working; new code should call [prg_to_thor()], whose
#'   `base.year` is a required argument rather than one defaulting to a global.
#'
#' @param modfile character. Path to the compiled model program.
#' @param calibfile character. Path to the calibration csv.
#' @param base.year numeric. First year of the model. `NULL` falls back to a
#'   `baseyear` in the calling scope, which is how this was always called.
#' @param last.year numeric. Last year of the model. `NULL` falls back to a
#'   `lastyear` in the calling scope.
#'
#' @returns A list with elements `endo`, `exo`, `coef`, `equations`, `data` and
#'   `errors`.
#' @export
translate_modelprg <- function(
    modfile = file.path("src", "compiler", "model.prg"),
    calibfile = file.path("src", "compiler", "calib.csv"),
    base.year = NULL,
    last.year = NULL) {

  ## The original wrote these as `base.year = baseyear`, picking the values out
  ## of whatever scope the caller happened to have. The fallback is kept, but
  ## made explicit: it is now visible in the body, it says so when it fires, and
  ## it no longer leaves two undefined globals in R CMD check.
  if (is.null(base.year)) {
    base.year <- get0("baseyear", envir = parent.frame(), ifnotfound = NULL)
    if (is.null(base.year)) {
      cli::cli_abort(c("{.arg base.year} was not given and no {.code baseyear} was found in the calling scope.",
                       "i" = "Pass it explicitly, as {.fn prg_to_thor} requires."),
                     call = NULL)
    }
  }
  if (is.null(last.year)) {
    last.year <- get0("lastyear", envir = parent.frame(), ifnotfound = NULL)
  }

  tr <- prg_to_thor(modfile = modfile, calibfile = calibfile,
                    base.year = base.year, last.year = last.year)

  ## The original seeded `errors` with NA and appended to it; callers test its
  ## length rather than its contents, so the shape is preserved.
  tr$errors <- c(NA_character_, tr$warnings)
  tr
}
