## LaTeX rendering of the .mdl AST ----------------------------------------
##
## One pass over the tree produced by mdl_parse(). Parentheses come from the
## precedence table, not from the source, so the rendered maths is correct
## whatever the author did or did not bracket.

#' Symbol table used when rendering variable names to LaTeX
#'
#' @description The ThreeME naming conventions, as data rather than as a
#'   chain of `str_replace()` calls. A name is split on `_`: the head is
#'   looked up here, the remaining chunks become the superscript, so
#'   `PROD_L[f, s]` renders as `PROD^{L}_{f, s}` and `ES_NRJ` as
#'   `\\eta^{NRJ}`. Trailing digits on the head are moved into the
#'   superscript (`ADJUST0_F` gives `\\alpha^{0,F}`).
#'
#'   Lookup is exact on the head chunk, so unlike the old implementation
#'   `tau` no longer matches inside `Beta_tauX`, and a name that is not in
#'   the table is left alone.
#'
#' @param extra named character vector merged over the defaults, to add or
#'   override symbols for a model that uses other conventions.
#'
#' @returns a named character vector, names are `.mdl` name heads, values
#'   are LaTeX.
#' @export
#'
#' @examples
#' head(mdl_symbols())
#' mdl_symbols(extra = c(kappa = "\\kappa"))[["kappa"]]
mdl_symbols <- function(extra = NULL) {
  defaults <- c(
    delta  = "\\delta",
    Rdep   = "\\delta",
    sigma  = "\\sigma",
    SIGMA  = "\\sigma",
    rho    = "\\rho",
    RHO    = "\\rho",
    tau    = "\\tau",
    TAU    = "\\tau",
    theta  = "\\theta",
    nu     = "\\nu",
    eta    = "\\zeta",
    ES     = "\\eta",
    es     = "\\eta",
    phi    = "\\varphi",
    Phi    = "\\varphi",
    PHI    = "\\varphi",
    ADJUST = "\\alpha",
    MARKUP = "\\mu",
    GR     = "g",
    eps    = "\\varepsilon",
    `@year`     = "t",
    `%baseyear` = "t_0",
    t_0         = "t_0"
  )
  if (!is.null(extra)) {
    stopifnot(is.character(extra), !is.null(names(extra)))
    defaults[names(extra)] <- extra
  }
  defaults
}

#' Render a .mdl variable name as LaTeX
#'
#' @param name character(1), the raw name, e.g. `"PROD_L"`.
#' @param symbols the symbol table, see [mdl_symbols()].
#'
#' @returns character(1) of LaTeX, without the surrounding `$`.
#' @export
#'
#' @examples
#' mdl_name_latex("PROD_L")
#' mdl_name_latex("ADJUST0_F")
mdl_name_latex <- function(name, symbols = mdl_symbols()) {
  lookup <- function(key) {
    hit <- symbols[match(key, names(symbols))]
    if (is.na(hit)) NULL else unname(hit)
  }
  direct <- lookup(name)
  if (!is.null(direct)) return(direct)
  chunks <- strsplit(name, "_", fixed = TRUE)[[1]]
  head <- chunks[1]
  tail <- chunks[-1]
  ## Trailing digits on the head belong in the superscript: ADJUST0 -> alpha^0
  digits <- sub("^.*?([0-9]*)$", "\\1", head)
  if (nzchar(digits) && nchar(digits) < nchar(head)) {
    head <- substring(head, 1, nchar(head) - nchar(digits))
    tail <- c(digits, tail)
  }
  mapped <- lookup(head)
  if (is.null(mapped)) {
    ## No exact match: a name may glue the symbol to its qualifier, as in
    ## PhiY for the production shares. Take the longest matching prefix.
    prefixes <- names(symbols)[startsWith(head, names(symbols)) &
                                 nchar(names(symbols)) < nchar(head)]
    if (length(prefixes)) {
      pre <- prefixes[which.max(nchar(prefixes))]
      mapped <- unname(symbols[[pre]])
      tail <- c(substring(head, nchar(pre) + 1L), tail)
    }
  }
  base <- if (!is.null(mapped)) mapped else escape_latex_text(head)
  if (!length(tail)) return(base)
  paste0(base, "^{", paste(vapply(tail, escape_latex_text, ""), collapse = ","), "}")
}

escape_latex_text <- function(x) {
  x <- gsub("%", "\\\\%", x)
  gsub("&", "\\\\&", x)
}

## Precedence of a rendered node, used to decide whether it needs brackets
## inside its parent. Anything atomic is at the top.
node_prec <- function(n) {
  switch(
    n$type,
    binop = mdl_binary_ops()[[n$op]],
    unop = 4L,
    qualified = node_prec(n$expr),
    9L
  )
}

#' Render a parsed .mdl node as LaTeX
#'
#' @param n a node from [mdl_parse()] or [mdl_parse_var()].
#' @param symbols the symbol table, see [mdl_symbols()].
#' @param show_conditions `TRUE` appends the `if ...` selector an equation
#'   carries (`if Y[c, s] <> 0`) to the rendered maths. These select which
#'   instances of an indexed equation exist rather than saying anything
#'   about the economics, so they are dropped by default.
#'
#' @returns character(1) of LaTeX, without delimiters.
#' @export
#'
#' @examples
#' mdl_latex(mdl_parse("d(K) = I{-1} - delta * K{-1}"))
mdl_latex <- function(n, symbols = mdl_symbols(), show_conditions = FALSE) {
  render <- function(n, parent_prec = 0L) {
    out <- switch(
      n$type,
      num = n$value,
      var = render_var(n),
      unop = paste0(n$op, render(n$arg, 4L)),
      binop = render_binop(n),
      fun = render_fun(n),
      qualified = render(n$expr, parent_prec),
      stop("cannot render a node of type '", n$type, "'", call. = FALSE)
    )
    if (node_prec(n) < parent_prec) {
      out <- paste0("\\left( ", out, " \\right)")
    }
    out
  }

  render_var <- function(n, time = NULL) {
    base <- mdl_name_latex(n$name, symbols)
    subs <- c(n$indices, time %||% (if (!is.null(n$lag)) paste0("t-", n$lag)))
    if (!length(subs)) return(base)
    paste0(base, "_{", paste(subs, collapse = ", "), "}")
  }

  render_binop <- function(n) {
    prec <- mdl_binary_ops()[[n$op]]
    if (n$op == "/") {
      ## \frac brackets its own arguments.
      return(paste0("\\frac{", render(n$lhs, 0L), "}{", render(n$rhs, 0L), "}"))
    }
    if (n$op == "^") {
      return(paste0("{", render(n$lhs, prec + 1L), "} ^ {", render(n$rhs, 0L), "}"))
    }
    op <- switch(n$op,
                 "*" = "\\;",
                 "<>" = "\\neq",
                 "<=" = "\\leq",
                 ">=" = "\\geq",
                 n$op)
    ## Left associative: the right operand needs brackets at equal precedence.
    paste0(render(n$lhs, prec), " ", op, " ", render(n$rhs, prec + 1L))
  }

  render_fun <- function(n) {
    name <- n$name
    args <- n$args
    if (tolower(name) == "sum") {
      a <- args[[1]]
      index <- if (inherits(a, "mdl_qualified")) a$over else NULL
      inner <- render(a, 0L)
      sub <- if (is.null(index)) "" else paste0("_{", index, "}")
      return(paste0("\\sum", sub, " ", brace_if_compound(a, inner)))
    }
    if (name == "d") {
      return(paste0("\\varDelta \\left( ", render(args[[1]], 0L), " \\right)"))
    }
    if (tolower(name) == "exp") {
      return(paste0("e^{", render(args[[1]], 0L), "}"))
    }
    if (name == "@elem") {
      return(render_elem(args))
    }
    ## Functions with a LaTeX operator of their own.
    known <- c(log = "\\log", Log = "\\log", ln = "\\ln",
               min = "\\min", max = "\\max")
    if (name %in% names(known)) {
      return(paste0(known[[name]], " \\left( ", render(args[[1]], 0L), " \\right)"))
    }
    rendered <- vapply(args, function(a) render(a, 0L), "")
    paste0("\\operatorname{", escape_latex_text(name), "} \\left( ",
           paste(rendered, collapse = ", "), " \\right)")
  }

  ## @elem(X, %baseyear) is X evaluated at the base year: fold the year into
  ## the subscript rather than printing it as a function call.
  render_elem <- function(args) {
    x <- args[[1]]
    year <- if (length(args) > 1) args[[2]] else NULL
    year_sub <- if (is.null(year)) "t_0" else {
      if (identical(year$type, "var")) mdl_name_latex(year$name, symbols) else render(year, 0L)
    }
    if (identical(x$type, "var")) {
      lagged <- if (!is.null(x$lag)) paste0(year_sub, "-", x$lag) else year_sub
      return(render_var(x, time = lagged))
    }
    paste0("\\left. ", render(x, 0L), " \\right|_{", year_sub, "}")
  }

  brace_if_compound <- function(nd, rendered) {
    inner <- if (inherits(nd, "mdl_qualified")) nd$expr else nd
    if (identical(inner$type, "binop") && mdl_binary_ops()[[inner$op]] <= 2L) {
      paste0("\\left( ", rendered, " \\right)")
    } else {
      rendered
    }
  }

  if (!inherits(n, "mdl_eq")) return(render(n, 0L))

  out <- paste0(render(n$lhs, 0L), " = ", render(n$rhs, 0L))
  if (show_conditions && !is.null(n$cond)) {
    out <- paste0(out, " \\quad \\text{if } ", render(n$cond, 0L))
  }
  if (!is.null(n$over)) {
    set <- if (is.null(n$set)) NULL else escape_latex_text(n$set)
    excluded <- if (is.null(n$excluded)) NULL else escape_latex_text(n$excluded)
    domain <- if (is.null(set)) n$over else {
      paste0(n$over, " \\in ", set,
             if (is.null(excluded)) "" else paste0(" \\setminus ", excluded))
    }
    out <- paste0(out, " \\quad \\forall ", domain)
  }
  out
}
