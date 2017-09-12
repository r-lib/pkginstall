local_binary_package <- function(pkgname, ..., extension = "tgz", envir = parent_or_pkg_env()) {

  # All arguments must be named
  args <- list(...)
  stopifnot(length(args) == 0 || rlang::is_named(args))

  d <- tempfile()
  dir.create(d)
  pkgdir <- file.path(d, pkgname)
  dir.create(pkgdir)
  nms <- names(args)
  for (i in seq_along(args)) {
    dir.create(file.path(pkgdir, dirname(nms[[i]])), showWarnings = FALSE, recursive = TRUE)
    writeLines(args[[i]], file.path(pkgdir, nms[[i]]))
  }

  filename <- file.path(d, glue("{pkgname}.{extension}"))
  archive_write_dir(filename, d)
  defer(unlink(d), envir = envir)
  filename
}

# For debugging if the parent frame is the global environment (as it would be
# when run directly on the R REPL) make it the package environment instead.
parent_or_pkg_env <- function(env = parent.frame()) {
  if (identical(env, globalenv())) {
    return(asNamespace("pkginstall"))
  }
  env
}
