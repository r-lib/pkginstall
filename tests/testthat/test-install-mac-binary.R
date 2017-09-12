context("install_mac_binary")

#' @importFrom archive archive_create
test_that("verify_mac_binary", {
  f1 <- local_binary_package("test1")
  expect_error(verify_mac_binary(f1),
    "'.*test1[.]tgz' is not a valid mac binary, it does not contain 'test1/Meta/package[.]rds' and 'test1/DESCRIPTION'")

  f2 <- local_binary_package("test2", "DESCRIPTION" = character())
  expect_error(verify_mac_binary(f2),
    "'.*test2[.]tgz' is not a valid mac binary, it does not contain 'test2/Meta/package[.]rds'")

  f3 <- local_binary_package("test3", "DESCRIPTION" = character(), "Meta/package.rds" = character())
  expect_error(verify_mac_binary(f3),
    "'.*test3[.]tgz' is not a valid mac binary, 'test3/DESCRIPTION' is empty")

  f4 <- local_binary_package("test4", "DESCRIPTION" = c("Package: foo"), "Meta/package.rds" = character())
  expect_error(verify_mac_binary(f4),
    "'.*test4[.]tgz' is not a valid mac binary, no 'Built' entry in 'test4/DESCRIPTION'")
})
