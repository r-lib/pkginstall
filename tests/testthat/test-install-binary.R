context("install_binary")

test_that("install_binary", {

  pkg <- binary_test_package("foo_0.0.0.9000")

  libpath <- create_temp_dir()
  on.exit({
    detach("package:foo", character.only = TRUE, unload = TRUE)
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_binary(pkg, lib = libpath))
  expect_error_free(
    library("foo", lib.loc = libpath))

  # Installing a loaded package should be an error.

  expect_equal(foo::foo(), NULL)

  expect_error(
    install_binary(pkg, lib = libpath),
    "Package '.*' is already loaded and cannot be installed[.]")
})

test_that("install_binary works for simultaneous installs", {
  skip_on_cran()

  pkg <- binary_test_package("foo_0.0.0.9000")
  on.exit({
    detach("package:foo", character.only = TRUE, unload = TRUE)
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  libpath <- create_temp_dir()

  processes <- list()
  num <- 5

  # install and load foo here to test loaded DLLs in another process
  expect_error_free(
    install_binary(pkg, lib = libpath))
  expect_error_free(
    library("foo", lib.loc = libpath))

  expect_equal(foo::foo(), NULL)
  processes <- replicate(num, simplify = FALSE,
    callr::r_bg(args = list(pkg, libpath),
      function(pkg, libpath) pkginstall::install_binary(pkg, lib = libpath))
  )

  repeat {
    Sys.sleep(.1)
    done <- all(!map_lgl(processes, function(x) x$is_alive()))
    if (done) { break }
  }

  for (i in seq_len(num)) {
    expect_identical(processes[[i]]$get_result(), file.path(libpath, "foo"))
  }
})
