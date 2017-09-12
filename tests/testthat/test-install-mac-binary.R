context("install_mac_binary")

test_that("install_mac_binary", {
  filename <- "test.tar"
  tar(filename)
  on.exit(file.remove(filename))
  expect_error(install_mac_binary(filename),
    regex_escape("'test.tar' is not a valid mac binary, it does not contain 'test/Meta/package.rds' and 'test/DESCRIPTION'"))
})
