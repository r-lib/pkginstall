context("install_binary")

test_that("verify_binary", {
  f1 <- local_binary_package("test1")
  expect_error(verify_binary(f1),
    "'.*test1[.]tgz' is not a valid binary, it does not contain a `DESCRIPTION` file")

  f2 <- local_binary_package("test2", "foo/DESCRIPTION" = character())
  expect_error(verify_binary(f2),
    "'.*test2[.]tgz' is not a valid binary, the `DESCRIPTION` file is nested more than 1 level deep")

  # handle mulltiple description files
  f3 <- local_binary_package("test3",
    "DESCRIPTION" = c("Package: foo", "Built: 2017-01-01"),
    "tests/testthat/DESCRIPTION" = character(),
    "Meta/package.rds" = character())
  expect_is(verify_binary(f3), "description")

  f4 <- local_binary_package("test4",
    "pkgdir/DESCRIPTION" = c("Package: foo", "Built: 2017-01-01"),
    "Meta/package.rds" = character())
  expect_error(verify_binary(f4),
    "'.*test4[.]tgz' is not a valid binary, the `DESCRIPTION` file is nested more than 1 level deep")

  f5 <- local_binary_package("test5", "DESCRIPTION" = character())
  expect_error(verify_binary(f5),
    "'.*test5[.]tgz' is not a valid binary, it does not contain 'test5/Meta/package[.]rds'")

  f6 <- local_binary_package("test6", "DESCRIPTION" = character(), "Meta/package.rds" = character())
  expect_error(verify_binary(f6),
    "'.*test6[.]tgz' is not a valid binary, 'test6/DESCRIPTION' is empty")

  f7 <- local_binary_package("test7", "DESCRIPTION" = c("Package: foo"), "Meta/package.rds" = character())
  expect_error(verify_binary(f7),
    "'.*test7[.]tgz' is not a valid binary, no 'Built' entry in 'test7/DESCRIPTION'")
})

test_that("install_binary_macos", {
  skip_on_os("windows")
  # even though this is a MacOS binary it still works on the
  # other OSs, as it is just R code.

  libpath <- create_temp_dir()
  on.exit({
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_binary("foo_0.0.0.9000.tgz", lib = libpath))

  # Installing a loaded package should be an error.
  library(foo, lib.loc = libpath)
  expect_equal(foo::foo(), NULL)

  expect_error(
    install_binary("foo_0.0.0.9000.tgz", lib = libpath),
    "Package '.*' is already loaded and cannot be installed[.]")

  detach("package:foo", unload = TRUE, character.only = TRUE)
})

test_that("install_binary_windows", {
  # even though this is a Windows binary it still works on the
  # other OSs, as it is just R code.

  libpath <- create_temp_dir()
  on.exit({
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_binary("foo_0.0.0.9000.zip", lib = libpath))

  # Installing a loaded package should be an error.
  library(foo, lib.loc = libpath)
  expect_equal(foo::foo(), NULL)

  expect_error(
    install_binary("foo_0.0.0.9000.zip", lib = libpath),
    "Package '.*' is already loaded and cannot be installed[.]")

  detach("package:foo", unload = TRUE, character.only = TRUE)
})

test_that("install_binary works for simultaneous installs", {
  skip_on_cran()

  switch (sysname(),
          windows = { pkg <- "foo_0.0.0.9000.zip" },
          linux = ,
          mac = { pkg <- "foo_0.0.0.9000.tgz" },
          skip(glue("Cannot test on {sysname()}"))
          )

  libpath <- create_temp_dir()

  processes <- list()
  num <- 5

  # install and load foo here to test loaded DLLs in another process
  pkginstall::install_binary(pkg, lib = libpath)
  library(foo)

  expect_equal(foo::foo(), NULL)
  processes <- replicate(num, simplify = FALSE,
    callr::r_bg(args = list(pkg, libpath),
      function(pkg, libpath) pkginstall::install_binary(pkg, lib = libpath))
  )

  for (i in seq_len(num)) {
    processes[[i]]$wait(timeout = 3000L)
    expect_identical(processes[[i]]$get_exit_status(), 0L)
    expect_identical(processes[[i]]$read_all_output(), "")
    expect_identical(processes[[i]]$read_all_error(), "")
  }
})
