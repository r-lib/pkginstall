local_binary_package <- function(pkgname, ..., extension = "tgz", envir = parent.frame()) {

  # All arguments must be named
  args <- list(...)
  stopifnot(length(args) == 0 || rlang::is_named(args))

  d <- create_temp_dir()
  pkgdir <- file.path(d, pkgname)
  dir.create(pkgdir)
  nms <- names(args)
  for (i in seq_along(args)) {
    dir.create(file.path(pkgdir, dirname(nms[[i]])), showWarnings = FALSE, recursive = TRUE)
    withr::with_connection(con = file(file.path(pkgdir, nms[[i]]), open = "wb"), {
      writeLines(args[[i]], con, sep = "\n")
    })
  }

  filename <- file.path(d, glue("{pkgname}.{extension}"))
  archive_write_dir(filename, d)

  # We do not want to unlink files if we are calling this from the R console,
  # useful when debugging.
  is_globalenv <- identical(envir, globalenv())
  if (!is_globalenv) {
    defer(unlink(d, recursive = TRUE), envir = envir)
  }
  filename
}

expect_error_free <- function(...) {
  expect_error(..., regexp = NA)
}
