#' @importFrom glue single_quote evaluate collapse
collapse_quote_transformer <- function(code, envir) {
  collapse_re <- "[*]$"
  quote_re <- "^[|]"
  should_collapse <- grepl(collapse_re, code)
  should_quote <- !grepl(quote_re, code)
  code <- sub(collapse_re, "",
    sub(quote_re, "", code))
  res <- evaluate(code, envir)
  if (should_quote) {
    res <- single_quote(res)
  }
  if (should_collapse) {
    res <- collapse(res, sep = ", ", last = " and ")
  }
  res
}

#' @importFrom rlang error_cnd
#' @importFrom glue glue
abort <- function(msg, type = NULL, ..., .envir = parent.frame()) {
  stop(
    error_cnd(
      type = type, ...,
      .msg = glue(msg,
        .envir = parent.frame(),
        .transformer = collapse_quote_transformer),
      ))
}

#' @importFrom rlang warning_cnd
warn <- function(msg, type = NULL, ..., .envir = parent.frame()) {
  warning(
    warning_cnd(
      type = type, ...,
      .msg = glue(msg,
        .envir = parent.frame(),
        .transformer = collapse_quote_transformer),
      ))
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

library_cache <- function(lib) {
  lib_cache <- file.path(lib, "_cache")
  dir.create(lib_cache, recursive = TRUE, showWarnings = FALSE)
  lib_cache
}

lock_cache <- function(cache, pkg_name, lock = TRUE) {
  use_lock <- !identical(lock, FALSE)
  my_lock <- NULL
  if (use_lock) {
    lockfile <- file.path(cache, glue("{pkg_name}.lock"))
    # TODO: timeout and fail?
    my_lock <- lock(lockfile)
  }
  my_lock
}

unlock <- function(lock) {
  if (is.null(lock)) {
    return()
  }
  filelock::unlock(lock)
}


sysname <- function() {
  res <- tolower(Sys.info()[["sysname"]])
  map <- c(darwin = "mac", "sunos" = "solaris")[res]
  res[!is.na(map)] <- map
  res
}

map_lgl <- get("map_lgl", asNamespace("rlang"))

map_chr <- get("map_chr", asNamespace("rlang"))

map_int <- get("map_int", asNamespace("rlang"))

lengths <- function(x) vapply(x, length, integer(1))

#' @importFrom rlang %||%
is_verbose <- function() {
  getOption("pkg.show_progress") %||% interactive()
}

mkdirp <- function(x) {
  dir.create(x, showWarnings = FALSE, recursive = TRUE)
}

str_trim <- function(x) {
  sub("\\s$", "", sub("^\\s+", "", x))
}

rep_list <- function(n, expr) {
  lapply(integer(n), eval.parent(substitute(function(...) expr)))
}

drop_nulls <- function(x) {
  is_null <- vapply(x, is.null, logical(1))
  x[!is_null]
}

#' @importFrom callr r_process r_process_options

make_dummy_worker_process <- function(n_iter = 10, sleep = 1, status = 0) {
  r_process$new(r_process_options(
    func = function(n_iter, sleep, status) {
      for (i in seq_len(n_iter)) {
        cat("out ", i, "\n", sep = "")
        message("err ", i)
        Sys.sleep(sleep)
      }
      status
      .GlobalEnv$.Last <- function() {
        rm(list = ".Last", envir = .GlobalEnv)
        quit(save = "no", status = status)
      }
    },
    args = list(n_iter = n_iter, sleep = sleep, status = status)
  ))
}

cut_into_lines <- function(x) {
  x <- do.call(paste0, as.list(x))
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(x)) x else ""
}
