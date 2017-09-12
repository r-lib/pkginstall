#' @importFrom glue single_quote evaluate collapse
collapse_quote_transformer <- function(regex = "[*]$", ...) {
  function(code, envir, data) {
    if (grepl(regex, code)) {
        code <- sub(regex, "", code)
    }
    res <- evaluate(code, envir, data)
    collapse(single_quote(res), ...)
  }
}

#' @importFrom rlang abort
#' @importFrom glue glue
abort <- function(msg, type = NULL, .envir = parent.frame()) {
  rlang::abort(glue(msg,
      .envir = parent.frame(),
      .transformer = collapse_quote_transformer(sep = ", ", last = " and ")),
    type = type)
}

`%!in%` <- function(x, y) {
  !x %in% y
}

#' @importFrom glue glue collapse
regex_escape <- function(x) {
  chars <- collapse(c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[", "]", "{", "}", "\\"), "\\")
  re <- glue("([{chars}])")
  gsub(re, "\\\\\\1", x, perl = TRUE)
}
