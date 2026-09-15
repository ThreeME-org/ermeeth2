## Parser for the .mdl equation language ---------------------------------
##
## Replaces the `pegr` grammar that used to live in 5_texdoc.R. A tokenizer
## feeds a precedence-climbing recursive descent parser, which returns an AST.
## Rendering (LaTeX) is a separate pass, see R/mdl_latex.R.
##
## Grammar, as found in ThreeME_V4/src/model:
##
##   equation   := "@over"? expr "=" qualified
##   qualified  := expr ("if" expr)? (("on"|"where") name ("in" name)?
##                                    ("\" name)?)?
##   expr       := precedence climbing over  <>, <=, >=, <, >  (1)
##                                           +, -              (2)
##                                           *, /              (3)
##                                           unary -, +        (4)
##                                           ^  (right assoc)  (5)
##   primary    := number | name "(" qualified ("," qualified)* ")"
##                        | name index? lag? | "(" expr ")"
##   index      := "[" name ("," name)* "]"
##   lag        := "{" "-" number "}"

mdl_binary_ops <- function() {
  list(
    "<>" = 1L, "<=" = 1L, ">=" = 1L, "<" = 1L, ">" = 1L,
    "+" = 2L, "-" = 2L,
    "*" = 3L, "/" = 3L,
    "^" = 5L
  )
}

#' Tokenize one line of .mdl source
#'
#' @param x character(1), a single (already stitched) source line.
#'
#' @returns a data frame with columns `type` (`number`, `name`, `op`,
#'   `punct`) and `value`, whitespace dropped.
#' @keywords internal
mdl_tokenize <- function(x) {
  patterns <- c(
    space  = "^[ \t]+",
    number = "^[0-9]*\\.?[0-9]+",
    name   = "^[@%]?[A-Za-z_][A-Za-z0-9._]*",
    op     = "^(<>|<=|>=|\\+|-|\\*|/|\\^|<|>|=)",
    punct  = "^[][(){},\\\\]"
  )
  type <- character(0)
  value <- character(0)
  rest <- x
  while (nzchar(rest)) {
    hit <- FALSE
    for (nm in names(patterns)) {
      m <- regexpr(patterns[[nm]], rest, perl = TRUE)
      if (m != -1L) {
        tok <- regmatches(rest, m)
        rest <- substring(rest, attr(m, "match.length") + 1L)
        if (nm != "space") {
          type <- c(type, nm)
          value <- c(value, tok)
        }
        hit <- TRUE
        break
      }
    }
    if (!hit) {
      stop("unexpected character '", substring(rest, 1, 1), "'", call. = FALSE)
    }
  }
  data.frame(type = type, value = value, stringsAsFactors = FALSE)
}

## Parser state is one environment threaded through the parse_* functions,
## which keeps them readable without passing an index back and forth.
mdl_parser <- function(tokens) {
  structure(new.env(parent = emptyenv()), class = "mdl_parser") -> p
  p$tokens <- tokens
  p$i <- 1L
  p
}

mdl_peek <- function(p, ahead = 0L) {
  j <- p$i + ahead
  if (j > nrow(p$tokens)) return(NULL)
  as.list(p$tokens[j, ])
}

mdl_at <- function(p, type, value = NULL, ahead = 0L) {
  tok <- mdl_peek(p, ahead)
  if (is.null(tok)) return(FALSE)
  tok$type == type && (is.null(value) || tok$value %in% value)
}

mdl_next <- function(p) {
  tok <- mdl_peek(p)
  if (is.null(tok)) stop("unexpected end of equation", call. = FALSE)
  p$i <- p$i + 1L
  tok
}

mdl_expect <- function(p, type, value = NULL) {
  tok <- mdl_next(p)
  if (tok$type != type || (!is.null(value) && !tok$value %in% value)) {
    stop("expected ", paste(value %||% type, collapse = " or "),
         ", found '", tok$value, "'", call. = FALSE)
  }
  tok
}

mdl_keywords <- c("if", "on", "where", "in")

node <- function(type, ...) {
  structure(list(type = type, ...), class = c(paste0("mdl_", type), "mdl_node"))
}

parse_primary <- function(p) {
  if (mdl_at(p, "punct", "(")) {
    mdl_next(p)
    e <- parse_expr(p, 1L)
    mdl_expect(p, "punct", ")")
    return(e)
  }
  if (mdl_at(p, "number")) {
    return(node("num", value = mdl_next(p)$value))
  }
  if (!mdl_at(p, "name")) {
    stop("expected a variable, number or '(', found '",
         (mdl_peek(p) %||% list(value = "<end>"))$value, "'", call. = FALSE)
  }
  name <- mdl_next(p)$value
  if (name %in% mdl_keywords) {
    stop("'", name, "' used where an expression was expected", call. = FALSE)
  }
  ## Function call
  if (mdl_at(p, "punct", "(")) {
    mdl_next(p)
    args <- list()
    if (!mdl_at(p, "punct", ")")) {
      repeat {
        args <- c(args, list(parse_qualified(p)))
        if (mdl_at(p, "punct", ",")) mdl_next(p) else break
      }
    }
    mdl_expect(p, "punct", ")")
    return(node("fun", name = name, args = args))
  }
  ## Variable, with optional indices and lag
  indices <- character(0)
  if (mdl_at(p, "punct", "[")) {
    mdl_next(p)
    repeat {
      indices <- c(indices, mdl_expect(p, "name")$value)
      if (mdl_at(p, "punct", ",")) mdl_next(p) else break
    }
    mdl_expect(p, "punct", "]")
  }
  lag <- NULL
  if (mdl_at(p, "punct", "{")) {
    mdl_next(p)
    mdl_expect(p, "op", "-")
    lag <- as.integer(mdl_expect(p, "number")$value)
    mdl_expect(p, "punct", "}")
  }
  node("var", name = name, indices = indices, lag = lag)
}

parse_unary <- function(p) {
  if (mdl_at(p, "op", c("-", "+"))) {
    op <- mdl_next(p)$value
    return(node("unop", op = op, arg = parse_expr(p, 4L)))
  }
  parse_primary(p)
}

parse_expr <- function(p, min_prec = 1L) {
  lhs <- parse_unary(p)
  ops <- mdl_binary_ops()
  repeat {
    tok <- mdl_peek(p)
    if (is.null(tok) || tok$type != "op" || is.null(ops[[tok$value]])) break
    prec <- ops[[tok$value]]
    if (prec < min_prec) break
    mdl_next(p)
    ## '^' is right associative, everything else left associative.
    next_min <- if (tok$value == "^") prec else prec + 1L
    rhs <- parse_expr(p, next_min)
    lhs <- node("binop", op = tok$value, lhs = lhs, rhs = rhs)
  }
  lhs
}

## expr with the trailing `if` / `on` / `where` qualifiers. Used both for the
## right-hand side of an equation and for the arguments of sum().
parse_qualified <- function(p) {
  e <- parse_expr(p, 1L)
  cond <- NULL
  over <- NULL
  set <- NULL
  excluded <- NULL
  repeat {
    if (mdl_at(p, "name", "if")) {
      mdl_next(p)
      cond <- parse_expr(p, 1L)
    } else if (mdl_at(p, "name", c("on", "where"))) {
      mdl_next(p)
      over <- mdl_expect(p, "name")$value
      if (mdl_at(p, "name", "in")) {
        mdl_next(p)
        set <- mdl_expect(p, "name")$value
        if (mdl_at(p, "punct", "\\")) {
          mdl_next(p)
          excluded <- mdl_expect(p, "name")$value
        }
      }
    } else {
      break
    }
  }
  if (is.null(cond) && is.null(over)) return(e)
  node("qualified", expr = e, cond = cond, over = over,
       set = set, excluded = excluded)
}

#' Parse one .mdl equation into an abstract syntax tree
#'
#' @description The replacement for the `pegr` grammar the old `teXdoc()`
#'   used. Unlike that grammar this one has operator precedence, so
#'   `a / b * c` parses as `(a / b) * c` rather than `a / (b * c)`, and it
#'   raises an error on input it cannot parse instead of returning nothing.
#'
#' @param x character(1), one equation, comments already stripped and
#'   continuation lines already stitched.
#'
#' @returns an `mdl_eq` node: a list with `lhs`, `rhs`, `cond`, `over`,
#'   `set`, `excluded` and `over_keyword` (`TRUE` when the equation was
#'   prefixed with `@over`).
#' @export
#'
#' @examples
#' eq <- mdl_parse("Y[c, s] = PhiY[c, s] * YQ[c] if Y[c, s] <> 0")
#' eq$lhs$name
mdl_parse <- function(x) {
  p <- mdl_parser(mdl_tokenize(x))
  over_keyword <- FALSE
  if (mdl_at(p, "name", "@over")) {
    mdl_next(p)
    over_keyword <- TRUE
  }
  lhs <- parse_expr(p, 1L)
  mdl_expect(p, "op", "=")
  rhs <- parse_qualified(p)
  if (p$i <= nrow(p$tokens)) {
    stop("unexpected '", mdl_peek(p)$value, "' after the end of the equation",
         call. = FALSE)
  }
  cond <- NULL; over <- NULL; set <- NULL; excluded <- NULL
  if (inherits(rhs, "mdl_qualified")) {
    cond <- rhs$cond; over <- rhs$over; set <- rhs$set; excluded <- rhs$excluded
    rhs <- rhs$expr
  }
  node("eq", lhs = lhs, rhs = rhs, cond = cond, over = over,
       set = set, excluded = excluded, over_keyword = over_keyword)
}

#' Parse a lone variable declaration, as found in an exovar .mdl file
#'
#' @param x character(1), e.g. `"ADJUST_MARKUP[s]"`.
#'
#' @returns an `mdl_var` node.
#' @export
#'
#' @examples
#' mdl_parse_var("ADJUST_MARKUP[s]")$indices
mdl_parse_var <- function(x) {
  p <- mdl_parser(mdl_tokenize(x))
  v <- parse_primary(p)
  if (p$i <= nrow(p$tokens)) {
    stop("unexpected '", mdl_peek(p)$value, "' after the variable name",
         call. = FALSE)
  }
  v
}

#' The variable an equation defines
#'
#' @description The left-hand side stripped of its `d()` / `log()` wrappers.
#'   The old implementation did this by deleting every `d`, `log`, `(` and
#'   `)` character from the line, which turned `delta` into `elta`; this one
#'   walks the parsed tree instead.
#'
#' @param eq an `mdl_eq` node, from [mdl_parse()].
#'
#' @returns an `mdl_var` node, or `NULL` if the left-hand side holds no
#'   variable at all.
#' @export
#'
#' @examples
#' mdl_dependent_var(mdl_parse("d(log(F_n[f, s])) = d(log(Y[s]))"))$name
mdl_dependent_var <- function(eq) {
  ## The defined variable is the first one met walking the left-hand side,
  ## skipping numeric factors: `PK[s] * F[K, s] = ...` defines PK.
  walk <- function(n) {
    if (is.null(n)) return(NULL)
    switch(
      n$type,
      var = n,
      num = NULL,
      unop = walk(n$arg),
      binop = walk(n$lhs) %||% walk(n$rhs),
      fun = {
        found <- NULL
        for (a in n$args) {
          found <- found %||% walk(a)
          if (!is.null(found)) break
        }
        found
      },
      qualified = walk(n$expr),
      NULL
    )
  }
  walk(eq$lhs)
}

#' The name of an equation's dependent variable, indices included
#'
#' @param eq an `mdl_eq` node, from [mdl_parse()].
#'
#' @returns character(1), e.g. `"F_n[f, s]"`, or `NA_character_`.
#' @export
#'
#' @examples
#' mdl_dependent_name(mdl_parse("F[K, s] = (1 - Rdep[s]) * F[K, s]{-1}"))
mdl_dependent_name <- function(eq) {
  v <- mdl_dependent_var(eq)
  if (is.null(v)) return(NA_character_)
  if (length(v$indices)) {
    paste0(v$name, "[", paste(v$indices, collapse = ", "), "]")
  } else {
    v$name
  }
}


#' Every variable referenced in a parsed equation
#'
#' Walks the whole tree, not just the left-hand side. A ThreeME variable often
#' never appears as the dependent variable of any equation -- `CH[c]` is
#' determined through `PCH[c] * CH[c] = ...`, and prices appear mostly on
#' right-hand sides -- so collecting only dependent variables misses most of
#' the model. [dictionary_skeleton()] uses this to work out which variables
#' carry which indices.
#'
#' @param node an `mdl_node`, as returned by [mdl_parse()] or [mdl_parse_var()].
#'
#' @returns a data frame with one row per variable *reference*, columns `name`
#'   and `index` (one row per index, `NA` for an unindexed reference).
#' @export
#'
#' @examples
#' mdl_variables(mdl_parse("PCH[c] * CH[c] = PCHD[c, s] * CHD[c, s]"))
mdl_variables <- function(node) {
  found <- list()
  walk <- function(x) {
    if (inherits(x, "mdl_var")) {
      idx <- if (length(x$indices)) as.character(unlist(x$indices)) else NA_character_
      found[[length(found) + 1L]] <<- data.frame(
        name = x$name, index = idx, stringsAsFactors = FALSE
      )
      return(invisible(NULL))
    }
    if (is.list(x)) for (el in x) if (!is.null(el)) walk(el)
    invisible(NULL)
  }
  walk(node)
  if (!length(found)) {
    return(data.frame(name = character(0), index = character(0),
                      stringsAsFactors = FALSE))
  }
  unique(do.call(rbind, found))
}
