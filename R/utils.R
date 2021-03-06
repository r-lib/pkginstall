#' @importFrom glue single_quote glue_collapse
collapse_quote_transformer <- function(code, envir) {
  collapse_re <- "[*]$"
  quote_re <- "^[|]"
  should_collapse <- grepl(collapse_re, code)
  should_quote <- !grepl(quote_re, code)
  code <- sub(collapse_re, "",
    sub(quote_re, "", code))
  res <- eval(parse(text = code, keep.source = FALSE), envir = envir)
  if (should_quote) {
    res <- single_quote(res)
  }
  if (should_collapse) {
    res <- glue_collapse(res, sep = ", ", last = " and ")
  }
  res
}

#' @importFrom rlang error_cnd
#' @importFrom glue glue
abort <- function(msg, type = NULL, ..., .envir = parent.frame()) {
  stop(
    error_cnd(
      .subclass = type, ...,
      message = glue(msg,
        .envir = parent.frame(),
        .transformer = collapse_quote_transformer),
      ))
}

#' @importFrom rlang warning_cnd
warn <- function(msg, type = NULL, ..., .envir = parent.frame()) {
  warning(
    warning_cnd(
      .subclass = type, ...,
      message = glue(msg,
        .envir = parent.frame(),
        .transformer = collapse_quote_transformer),
      ))
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

#' @importFrom rlang %||%

is_verbose <- function() {
  env <- Sys.getenv("R_PKG_SHOW_PROGRESS", "")
  if (env != "") {
    tolower(env) == "true"
  } else {
    opt <- getOption("pkg.show_progress")
    if (!is.null(opt)) {
      isTRUE(opt)
    } else {
      interactive()
    }
  }
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
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

cut_into_lines <- function(x) {
  x <- do.call(paste0, as.list(x))
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(x)) x else ""
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_count <- function(x, min = 0L)  {
  is.numeric(x) && length(x) == 1 && !is.na(x) &&
    as.integer(x) == x && x >= min
}

all_named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
}


is_rstudio_version <- function(ver) {
  tryCatch(
    rstudioapi::getVersion() >= ver,
    error = function(e) FALSE
  )
}

have_rstudio_bug_2387 <- function() {
  if (!is.null(r <- pkg_data$rstudio_bug_2387)) return(r)
  r <- pkg_data$rstudio_bug_2387 <-
    Sys.getenv("RSTUDIO", "") != "" &&
    Sys.getenv("RSTUDIO_TERM", "") == "" &&
    !is_rstudio_version("1.2.128")
  r
}
