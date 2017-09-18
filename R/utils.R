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

create_temp_dir <- function(..., tmpdir = tempdir()) {
  f <- tempfile(tmpdir = tmpdir, ...)
  dir.create(f)
  f
}

library_cache <- function(lib, pkg_name, lock = TRUE) {
  lib_cache <- file.path(lib, "_cache")
  dir.create(lib_cache, recursive = TRUE, showWarnings = FALSE)

  use_lock <- !identical(lock, FALSE)
  if (use_lock) {
    lockfile <- file.path(lib_cache, glue("{pkg_name}.lock"))
    # TODO: timeout and fail?
    my_lock <- lock(lockfile)
    withr::defer(unlock(my_lock))
  }
  lib_cache
}

sysname <- function() {
  res <- tolower(Sys.info()[["sysname"]])
  map <- c(darwin = "mac", "sunos" = "solaris")[res]
  res[!is.na(map)] <- map
  res
}
