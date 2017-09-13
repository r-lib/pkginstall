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
  # even though this is a MacOS binary it still works on the
  # other OSs, as it is just R code.

  libpath <- tempfile()
  dir.create(libpath)
  on.exit({
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_binary("foo_0.0.0.9000.tgz", lib = libpath))

  # Load the package (and make sure it is unloaded) after installing again.
  library(foo, lib.loc = libpath)
  expect_error_free(
    install_binary("foo_0.0.0.9000.tgz", lib = libpath))
  expect_false(is_loaded("foo"))


  # Should error if there is already a lock
  dir.create(file.path(libpath, "00LOCK-foo"))
  expect_error(
    install_binary("foo_0.0.0.9000.tgz", lib = libpath),
    "Installing 'foo' failed, lock found at .*00LOCK-foo")

  # But not if `lock = FALSE`
  expect_error_free(
    install_binary("foo_0.0.0.9000.tgz", lib = libpath, lock = FALSE))
})

test_that("install_binary_windows", {
  # even though this is a Windows binary it still works on the
  # other OSs, as it is just R code.

  libpath <- tempfile()
  dir.create(libpath)
  on.exit({
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_binary("foo_0.0.0.9000.zip", lib = libpath))

  # Load the package (and make sure it is unloaded) after installing again.
  library(foo, lib.loc = libpath)
  expect_error_free(
    install_binary("foo_0.0.0.9000.zip", lib = libpath))
  expect_false(is_loaded("foo"))

  # Should error if there is already a lock
  dir.create(file.path(libpath, "00LOCK-foo"))
  expect_error(
    install_binary("foo_0.0.0.9000.zip", lib = libpath),
    "Installing 'foo' failed, lock found at .*00LOCK-foo")

  # But not if `lock = FALSE`
  expect_error_free(
    install_binary("foo_0.0.0.9000.zip", lib = libpath, lock = FALSE))
})
