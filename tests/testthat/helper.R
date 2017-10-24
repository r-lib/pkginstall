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
    withr::with_connection(list(con = file(file.path(pkgdir, nms[[i]]), open = "wb")), {
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

binary_test_package <- function(name) {

  binary <- switch(sysname(),
    windows = glue("{name}.zip"),
    linux = glue("{name}_R_x86_64-pc-linux-gnu.tar.gz"),
    mac = glue("{name}.tgz"),
    skip(glue("Cannot test on {sysname()}"))
    )
  if (!file.exists(binary)) {
    pkgbuild::build(sub("_.*$", "", name), binary = TRUE, quiet = TRUE)
  }
  binary
}

expect_error_free <- function(...) {
  expect_error(..., regexp = NA)
}

if (is_loaded("foo")) {
  pkgload::unload("foo")
}
