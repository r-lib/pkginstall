#' @importFrom glue single_quote evaluate collapse
collapse_quote_transformer <- function(code, envir, data) {
  collapse_re <- "[*]$"
  quote_re <- "^[|]"
  should_collapse <- grepl(collapse_re, code)
  should_quote <- !grepl(quote_re, code)
  code <- sub(collapse_re, "",
    sub(quote_re, "", code))
  res <- evaluate(code, envir, data)
  if (should_quote) {
    res <- single_quote(res)
  }
  if (should_collapse) {
    res <- collapse(res, sep = ", ", last = " and ")
  }
  res
}

#' @importFrom rlang abort
#' @importFrom glue glue
abort <- function(msg, type = NULL, .envir = parent.frame()) {
  rlang::abort(glue(msg,
      .envir = parent.frame(),
      .transformer = collapse_quote_transformer),
    type = type)
}

#' @importFrom rlang warn
warn <- function(msg, type = NULL, .envir = parent.frame()) {
  rlang::warn(glue(msg,
      .envir = parent.frame(),
      .transformer = collapse_quote_transformer),
    type = type)
}

`%!in%` <- function(x, y) {
  !x %in% y
}

is_loaded <- function(package) {
  package %in% loadedNamespaces()
}

create_temp_dir <- function(...) {
  f <- tempfile(...)
  dir.create(f)
  f
}
