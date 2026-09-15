## Model documentation from .mdl sources ----------------------------------
##
## Reads the .mdl files, parses every equation once into an AST, and renders
## the result to Quarto (the default) or LaTeX. The old two-stage pipeline
## (teXdoc() writing .tex, make_eq_qmd() regex-parsing that .tex back into
## .qmd) is gone: both formats are now rendered from the same parsed
## document.

#' Read a .mdl file, stitching continuation lines
#'
#' @description Lines ending in ` _` continue on the next line.
#'
#' @param path path to a `.mdl` file.
#'
#' @returns a data frame with columns `line` (the number of the *first*
#'   source line of the stitched statement) and `text`.
#' @keywords internal
mdl_read_lines <- function(path) {
  raw <- readLines(path, warn = FALSE)
  text <- character(0)
  line <- integer(0)
  stitching <- FALSE
  for (i in seq_along(raw)) {
    l <- trimws(raw[i])
    if (stitching) {
      text[length(text)] <- paste0(sub(" _$", " ", text[length(text)]), l)
    } else {
      text <- c(text, l)
      line <- c(line, i)
    }
    stitching <- grepl(" _$", l)
  }
  data.frame(line = line, text = text, stringsAsFactors = FALSE)
}

## Prose conversions must not touch inline maths: `$a*b$` is multiplication,
## not emphasis. Split the string on $...$ spans and map only the rest.
map_outside_math <- function(x, f) {
  vapply(x, function(one) {
    parts <- strsplit(one, "(?<=\\$)|(?=\\$)", perl = TRUE)[[1]]
    in_math <- FALSE
    for (i in seq_along(parts)) {
      if (identical(parts[i], "$")) {
        in_math <- !in_math
      } else if (!in_math) {
        parts[i] <- f(parts[i])
      }
    }
    paste0(parts, collapse = "")
  }, "", USE.NAMES = FALSE)
}

## LaTeX prose in ## comments, lightly converted for markdown. Inline maths
## between $ is left untouched, which is where most of the LaTeX lives.
mdl_text_to_md <- function(x) {
  x <- map_outside_math(x, function(s) {
    s <- gsub("\\\\(begin|end)\\{[^}]*\\}", "", s)
    s <- gsub("\\\\textbf\\{([^}]*)\\}", "**\\1**", s)
    gsub("\\\\(emph|textit)\\{([^}]*)\\}", "*\\2*", s)
  })
  x <- gsub("\\\\\\\\\\s*$", "", x)
  trimws(x)
}

slugify <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  x <- gsub("(^-+)|(-+$)", "", x)
  tolower(x)
}

#' Parse a set of .mdl files into a documentation table
#'
#' @description The single parsing pass behind [model_doc()]. Useful on its
#'   own to check a model parses, or to build some other output from it.
#'
#' @param sources character vector of `.mdl` files holding the equations.
#' @param exo character vector of `.mdl` files holding the exogenous
#'   variable declarations.
#' @param base.path directory the file names are relative to.
#' @param symbols symbol table, see [mdl_symbols()].
#' @param show_conditions passed to [mdl_latex()].
#' @param overrides optional named character vector of hand-written LaTeX,
#'   keyed by variable name (e.g. `c("PCH_CES" = "PCH^{CES} = ...")`), used
#'   instead of the parsed rendering. Replaces the hardcoded `explicit` list
#'   that used to sit inside `teXdoc()`.
#'
#' @returns a data frame with one row per item, columns `file`, `line`,
#'   `kind` (`heading`, `text`, `equation`, `exovar`), `level`, `title`,
#'   `description`, `raw`, `latex`, `variable`, `id`, `error` and `indices`
#'   (the index names of the left-hand side, comma separated, `NA` when the
#'   variable is not indexed).
#' @export
#'
#' @examples
#' items <- mdl_document(
#'   sources   = "model_test.mdl",
#'   exo       = "model_test-exovar.mdl",
#'   base.path = system.file(package = "ermeeth2")
#' )
#' items[items$kind == "equation", c("variable", "latex")]
mdl_document <- function(sources,
                         exo = character(0),
                         base.path = "src/model",
                         symbols = mdl_symbols(),
                         show_conditions = FALSE,
                         overrides = NULL) {
  files <- c(sources, exo)
  if (!length(files)) stop("`sources` is empty: nothing to document.")
  missing <- files[!file.exists(file.path(base.path, files))]
  if (length(missing)) {
    stop("file(s) not found under '", base.path, "': ",
         paste(missing, collapse = ", "))
  }

  items <- list()
  ## `indices` carries the index names of the left-hand side (`Y[c, s]` ->
  ## "c, s"), which is what `dictionary_skeleton()` reads to work out whether a
  ## variable is indexed by sector or by commodity. The `on ... in ...`
  ## qualifier is not enough on its own: it records only the last clause, so a
  ## two-index equation loses one of them.
  add <- function(...) {
    row <- data.frame(..., stringsAsFactors = FALSE)
    if (!"indices" %in% names(row)) row$indices <- NA_character_
    items[[length(items) + 1L]] <<- row[, c(setdiff(names(row), "indices"), "indices"),
                                        drop = FALSE]
  }

  for (f in files) {
    is_exo <- f %in% exo
    src <- mdl_read_lines(file.path(base.path, f))
    description <- ""
    for (k in seq_len(nrow(src))) {
      l <- src$text[k]
      ln <- src$line[k]
      if (!nzchar(l)) next

      if (grepl("^#####\\s", l)) {
        description <- ""
        add(file = f, line = ln, kind = "heading", level = 1L,
            title = mdl_text_to_md(sub("^#####\\s*", "", l)),
            description = "", raw = l, latex = NA_character_,
            variable = NA_character_, id = NA_character_, error = NA_character_)
      } else if (grepl("^####\\s", l)) {
        description <- ""
        add(file = f, line = ln, kind = "heading", level = 2L,
            title = mdl_text_to_md(sub("^####\\s*", "", l)),
            description = "", raw = l, latex = NA_character_,
            variable = NA_character_, id = NA_character_, error = NA_character_)
      } else if (grepl("^###\\s", l)) {
        description <- ""
        add(file = f, line = ln, kind = "heading", level = 3L,
            title = mdl_text_to_md(sub("^###\\s*", "", l)),
            description = "", raw = l, latex = NA_character_,
            variable = NA_character_, id = NA_character_, error = NA_character_)
      } else if (grepl("^##!\\s", l)) {
        ## The caption of the equation that follows.
        description <- mdl_text_to_md(sub("^##!\\s*", "", l))
      } else if (grepl("^##($|\\s)", l)) {
        add(file = f, line = ln, kind = "text", level = NA_integer_,
            title = NA_character_,
            description = mdl_text_to_md(sub("^##\\s*", "", l)),
            raw = l, latex = NA_character_, variable = NA_character_,
            id = NA_character_, error = NA_character_)
      } else if (grepl("^#", l)) {
        ## A plain comment: not documentation.
        next
      } else {
        parsed <- tryCatch(
          if (is_exo) mdl_parse_var(l) else mdl_parse(l),
          error = function(e) e
        )
        if (inherits(parsed, "error")) {
          add(file = f, line = ln, kind = if (is_exo) "exovar" else "equation",
              level = NA_integer_, title = NA_character_,
              description = description, raw = l, latex = NA_character_,
              variable = NA_character_, id = NA_character_,
              error = conditionMessage(parsed))
          description <- ""
          next
        }
        idx <- if (is_exo) parsed$indices else parsed$lhs$indices
        idx <- if (length(idx)) paste(unlist(idx), collapse = ", ") else NA_character_
        variable <- if (is_exo) {
          if (length(parsed$indices)) {
            paste0(parsed$name, "[", paste(parsed$indices, collapse = ", "), "]")
          } else {
            parsed$name
          }
        } else {
          mdl_dependent_name(parsed)
        }
        latex <- if (!is.null(overrides) && !is.na(variable) &&
                     variable %in% names(overrides)) {
          unname(overrides[[variable]])
        } else {
          mdl_latex(parsed, symbols = symbols, show_conditions = show_conditions)
        }
        add(file = f, line = ln, kind = if (is_exo) "exovar" else "equation",
            level = NA_integer_, title = NA_character_,
            description = description, raw = l, latex = latex,
            variable = variable, id = NA_character_, error = NA_character_,
            indices = idx)
        description <- ""
      }
    }
  }

  out <- do.call(rbind, items)
  rownames(out) <- NULL

  ## Identifiers, unique across the whole document.
  eq <- which(out$kind %in% c("equation", "exovar"))
  ids <- paste0("eq-", slugify(sub("\\.mdl$", "", out$file[eq])), "-",
                slugify(ifelse(is.na(out$variable[eq]),
                               paste0("line-", out$line[eq]),
                               out$variable[eq])))
  dup <- duplicated(ids)
  while (any(dup)) {
    ids[dup] <- paste0(ids[dup], "-", ave_seq(ids)[dup] + 1L)
    dup <- duplicated(ids)
  }
  out$id[eq] <- ids
  out
}

ave_seq <- function(x) {
  ## How many times each value has already been seen, in order.
  seen <- integer(0)
  vapply(x, function(v) {
    n <- sum(seen == v)
    seen <<- c(seen, v)
    n
  }, integer(1), USE.NAMES = FALSE)
}

## Glossary rows: one per documented variable, in case-insensitive order.
mdl_glossary <- function(items, symbols = mdl_symbols()) {
  g <- items[items$kind %in% c("equation", "exovar") &
               !is.na(items$variable) & nzchar(items$description), ,
             drop = FALSE]
  if (!nrow(g)) return(g[0, , drop = FALSE])
  g <- g[!duplicated(g$variable), , drop = FALSE]
  g$symbol <- vapply(
    g$variable,
    function(v) tryCatch(mdl_latex(mdl_parse_var(v), symbols = symbols),
                         error = function(e) escape_latex_text(v)),
    "", USE.NAMES = FALSE
  )
  g[order(tolower(g$variable)), , drop = FALSE]
}

#' Document a model's equations from its .mdl sources
#'
#' @description Reads the `.mdl` files a ThreeME-style model is written in
#'   and writes the equations, the exogenous variables and a glossary as a
#'   Quarto document, with every equation cross-referenceable
#'   (`@eq-<file>-<variable>`).
#'
#'   This replaces `teXdoc()` and `make_eq_qmd()`. The `.mdl` parser is now
#'   part of the package (see [mdl_parse()]) rather than a `pegr` grammar,
#'   equations that fail to parse are reported instead of silently dropped,
#'   and the LaTeX output is a second renderer of the same parsed document
#'   rather than the source the Quarto was scraped from.
#'
#' @param sources character vector of `.mdl` files holding the equations.
#' @param exo character vector of `.mdl` files holding the exogenous
#'   variable declarations.
#' @param base.path directory `sources` and `exo` are relative to.
#' @param out output file name, without extension.
#' @param out.path output directory, created if needed.
#' @param format `"qmd"` (default), `"tex"`, or `"both"`.
#' @param title document title. Used in the YAML header when `standalone`,
#'   and as the LaTeX `\\title`.
#' @param standalone `TRUE` (default) writes a document that renders on its
#'   own: a YAML header for Quarto, a preamble and `\\begin{document}` for
#'   LaTeX. `FALSE` writes a fragment meant to be pulled into a larger
#'   document with `{{< include >}}` or `\\input`.
#' @param heading_level the markdown level the `#####` headings of the
#'   sources map to. Defaults to 2 (`##`), so the document title stays `#`.
#' @param exo_title,glossary_title headings of the two generated sections.
#'   Set either to `NA` to leave that section out.
#' @param symbols symbol table, see [mdl_symbols()].
#' @param show_conditions `TRUE` prints the `if ...` selectors alongside the
#'   equations. See [mdl_latex()].
#' @param overrides named character vector of hand-written LaTeX keyed by
#'   variable name, used instead of the parsed rendering.
#' @param compile_pdf `TRUE` runs [tools::texi2pdf()] on the LaTeX output.
#'   Ignored unless a `.tex` file is produced.
#' @param quiet `TRUE` suppresses the summary message.
#'
#' @returns invisibly, a list with `files` (the paths written) and `items`
#'   (the parsed document, as returned by [mdl_document()]).
#' @export
#'
#' @examples
#' \dontrun{
#' model_doc(
#'   sources   = c("producer.mdl", "consumer.mdl"),
#'   exo       = "exogenous.mdl",
#'   base.path = file.path("src", "model", "threeme"),
#'   out       = "equations",
#'   out.path  = file.path("results", "quarto_render")
#' )
#' }
model_doc <- function(sources,
                      exo = character(0),
                      base.path = "src/model",
                      out = "model_doc",
                      out.path = getwd(),
                      format = c("qmd", "tex", "both"),
                      title = "Model equations",
                      standalone = TRUE,
                      heading_level = 2L,
                      exo_title = "Exogenous variables",
                      glossary_title = "Glossary of variables",
                      symbols = mdl_symbols(),
                      show_conditions = FALSE,
                      overrides = NULL,
                      compile_pdf = FALSE,
                      quiet = FALSE) {
  format <- match.arg(format)
  out <- basename(tools::file_path_sans_ext(out))

  items <- mdl_document(sources = sources, exo = exo, base.path = base.path,
                        symbols = symbols, show_conditions = show_conditions,
                        overrides = overrides)

  failed <- items[!is.na(items$error), , drop = FALSE]
  if (nrow(failed)) {
    warning(
      nrow(failed), " statement(s) could not be parsed and are shown verbatim:\n",
      paste0("  ", failed$file, ":", failed$line, "  ", failed$raw,
             "\n    -> ", failed$error, collapse = "\n"),
      call. = FALSE
    )
  }

  if (!dir.exists(out.path)) dir.create(out.path, recursive = TRUE)

  written <- character(0)
  if (format %in% c("qmd", "both")) {
    path <- file.path(out.path, paste0(out, ".qmd"))
    writeLines(
      model_doc_qmd(items, title = title, standalone = standalone,
                    heading_level = heading_level, exo_title = exo_title,
                    glossary_title = glossary_title, symbols = symbols),
      path
    )
    written <- c(written, path)
  }
  if (format %in% c("tex", "both")) {
    path <- file.path(out.path, paste0(out, ".tex"))
    writeLines(
      model_doc_tex(items, title = title, standalone = standalone,
                    exo_title = exo_title, glossary_title = glossary_title,
                    symbols = symbols),
      path
    )
    written <- c(written, path)
    if (compile_pdf) {
      wd <- setwd(out.path)
      on.exit(setwd(wd), add = TRUE)
      tools::texi2pdf(basename(path), clean = TRUE)
      written <- c(written, file.path(out.path, paste0(out, ".pdf")))
    }
  }

  if (!quiet) {
    message(sum(items$kind == "equation"), " equations, ",
            sum(items$kind == "exovar"), " exogenous variables, ",
            nrow(failed), " parse failures -> ",
            paste(basename(written), collapse = ", "))
  }
  invisible(list(files = written, items = items))
}

## --- Quarto renderer ------------------------------------------------------

model_doc_qmd <- function(items, title, standalone, heading_level,
                          exo_title, glossary_title, symbols) {
  hash <- function(level) strrep("#", level)
  lines <- character(0)
  if (standalone) {
    lines <- c("---", paste0("title: \"", title, "\""), "---", "")
  } else if (!is.null(title) && !is.na(title)) {
    lines <- c(paste(hash(max(heading_level - 1L, 1L)), title), "")
  }

  eqs <- items[items$kind != "exovar", , drop = FALSE]
  for (i in seq_len(nrow(eqs))) {
    it <- eqs[i, ]
    if (it$kind == "heading") {
      lines <- c(lines, paste(hash(heading_level + it$level - 1L), it$title), "")
    } else if (it$kind == "text") {
      lines <- c(lines, it$description, "")
    } else {
      if (nzchar(it$description)) lines <- c(lines, paste0("**", it$description, "**"), "")
      lines <- c(lines, qmd_equation(it), "")
    }
  }

  exo <- items[items$kind == "exovar", , drop = FALSE]
  if (nrow(exo) && !is.na(exo_title)) {
    lines <- c(lines, paste(hash(heading_level), exo_title), "")
    for (i in seq_len(nrow(exo))) {
      it <- exo[i, ]
      if (nzchar(it$description)) lines <- c(lines, paste0("**", it$description, "**"), "")
      lines <- c(lines, qmd_equation(it), "")
    }
  }

  if (!is.na(glossary_title)) {
    g <- mdl_glossary(items, symbols = symbols)
    if (nrow(g)) {
      lines <- c(
        lines,
        paste(hash(heading_level), glossary_title), "",
        "| Variable | Definition | Equation |",
        "|:---------|:-----------|---------:|",
        paste0("| $", g$symbol, "$ | ", gsub("|", "\\|", g$description, fixed = TRUE),
               " | [-@", g$id, "] |"),
        ""
      )
    }
  }
  lines
}

## A cross-referenceable display equation. Quarto needs the {#eq-...} on the
## line that closes the display maths.
qmd_equation <- function(it) {
  if (is.na(it$latex)) {
    ## Unparsed: show the source rather than dropping it silently.
    return(c("``` {.mdl}", it$raw, "```"))
  }
  c("$$", it$latex, paste0("$$ {#", it$id, "}"))
}

## --- LaTeX renderer -------------------------------------------------------

model_doc_tex <- function(items, title, standalone, exo_title,
                          glossary_title, symbols) {
  lines <- character(0)
  if (standalone) {
    lines <- c(
      "\\documentclass[12pt]{article}",
      "\\usepackage{amsmath}",
      "\\usepackage{breqn}",
      "\\usepackage{longtable}",
      "\\usepackage{booktabs}",
      "\\usepackage{array}",
      "\\usepackage{ragged2e}",
      "\\usepackage{hyperref}",
      "\\numberwithin{equation}{section}",
      paste0("\\title{", title, "}"),
      "\\begin{document}",
      "\\maketitle",
      ""
    )
  }

  sec <- c("section", "subsection", "subsubsection")
  eqs <- items[items$kind != "exovar", , drop = FALSE]
  for (i in seq_len(nrow(eqs))) {
    it <- eqs[i, ]
    if (it$kind == "heading") {
      lines <- c(lines, paste0("\\", sec[min(it$level, 3L)], "{",
                               md_to_tex(it$title), "}"), "")
    } else if (it$kind == "text") {
      lines <- c(lines, md_to_tex(it$description), "")
    } else {
      if (nzchar(it$description)) {
        lines <- c(lines, paste0("\\noindent \\textbf{", md_to_tex(it$description), "}"), "")
      }
      lines <- c(lines, tex_equation(it), "")
    }
  }

  exo <- items[items$kind == "exovar", , drop = FALSE]
  if (nrow(exo) && !is.na(exo_title)) {
    lines <- c(lines, "\\newpage", paste0("\\section{", exo_title, "}"), "")
    for (i in seq_len(nrow(exo))) {
      it <- exo[i, ]
      if (nzchar(it$description)) {
        lines <- c(lines, paste0("\\noindent \\textbf{", md_to_tex(it$description), "}"), "")
      }
      lines <- c(lines, tex_equation(it), "")
    }
  }

  if (!is.na(glossary_title)) {
    g <- mdl_glossary(items, symbols = symbols)
    if (nrow(g)) {
      lines <- c(
        lines, "\\newpage", paste0("\\section{", glossary_title, "}"), "\\small",
        "\\begin{longtable}{@{}p{2.75cm}p{8.5cm}p{1.2cm}@{}}",
        paste0("$", g$symbol, "$ & ", md_to_tex(g$description),
               " & \\RaggedLeft \\ref{", g$id, "} \\\\"),
        "\\end{longtable}"
      )
    }
  }
  if (standalone) lines <- c(lines, "", "\\end{document}")
  lines
}

tex_equation <- function(it) {
  if (is.na(it$latex)) {
    return(c("\\begin{verbatim}", it$raw, "\\end{verbatim}"))
  }
  c("\\begin{dmath}", paste0("\\label{", it$id, "}"), it$latex, "\\end{dmath}")
}

## The descriptions were converted to markdown when the sources were read;
## put the little that changed back for the LaTeX output.
md_to_tex <- function(x) {
  map_outside_math(x, function(s) {
    s <- gsub("\\*\\*([^*]*)\\*\\*", "\\\\textbf{\\1}", s)
    s <- gsub("(^|[^*])\\*([^*]+)\\*", "\\1\\\\emph{\\2}", s)
    ## Bare & % # in prose would end a cell / start a comment. Backslashed
    ## ones are the author's own LaTeX and are left alone.
    gsub("(?<!\\\\)([&%#])", "\\\\\\1", s, perl = TRUE)
  })
}
