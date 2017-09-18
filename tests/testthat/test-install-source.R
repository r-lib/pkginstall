context("install_source")

test_that("install_source works with source packages in directories", {
  pkg <- "foo"

  libpath <- create_temp_dir()
  on.exit({
    detach("package:foo", character.only = TRUE, unload = TRUE)
    remove.packages(pkg, lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_source(pkg, lib = libpath))
  expect_error_free(
    library("foo", lib.loc = libpath))

  expect_equal(foo::foo(), NULL)
  expect_error(
    install_source(pkg, lib = libpath),
    "Package '.*' is already loaded and cannot be installed[.]")
})

test_that("install_source works with source packages in tarballs", {
  pkg <- "foo_0.0.0.9000.tar.gz"

  libpath <- create_temp_dir()
  on.exit({
    detach("package:foo", character.only = TRUE, unload = TRUE)
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_source("foo_0.0.0.9000.tar.gz", lib = libpath))
  expect_error_free(
    library("foo", lib.loc = libpath))

  expect_equal(foo::foo(), NULL)
  expect_error(
    install_source(pkg, lib = libpath),
    "Package '.*' is already loaded and cannot be installed[.]")
})

