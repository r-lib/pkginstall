
context("archive")

test_that("archive CRUD", {

  tmp <- create_temp_dir()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  cat("foobar\n", file = file.path(tmp, "foobar"))
  dir.create(file.path(tmp, "dir"))
  cat("foo\nbar\n", file = file.path(tmp, "dir", "foo"))
  dir.create(file.path(tmp, "empty"))

  tar_file <- tempfile(fileext = ".tar.gz")
  zip_file <- sub("[.]tar[.]gz$", ".zip", tar_file)
  on.exit(unlink(c(tar_file, zip_file)), add = TRUE)

  expect_silent(archive_write_dir(tar_file, tmp))
  expect_silent(archive_write_dir(zip_file, tmp))

  expect_true(file.exists(tar_file))
  expect_true(file.exists(zip_file))

  expect_equal(sort(archive_files(tar_file)), sort(c("dir/foo", "foobar")))
  expect_equal(sort(archive_files(zip_file)), sort(c("dir/foo", "foobar")))

  dir.create(tmp2 <- tempfile())
  dir.create(tmp3 <- tempfile())
  on.exit(unlink(c(tmp2, tmp3), recursive = TRUE), add = TRUE)
  expect_silent(archive_extract(tar_file, tmp2))
  expect_silent(archive_extract(zip_file, tmp3))

  expect_true(file.exists(file.path(tmp2, "foobar")))
  expect_true(file.exists(file.path(tmp2, "dir", "foo")))
  expect_true(file.exists(file.path(tmp3, "foobar")))
  expect_true(file.exists(file.path(tmp3, "dir", "foo")))

  unlink(tmp, recursive = TRUE)
  expect_equal(archive_read(tar_file, "foobar"), "foobar")
  unlink(tmp, recursive = TRUE)
  expect_equal(archive_read(tar_file, "dir/foo"), c("foo", "bar"))

  unlink(tmp, recursive = TRUE)
  expect_equal(archive_read(zip_file, "foobar"), "foobar")
  unlink(tmp, recursive = TRUE)
  expect_equal(archive_read(zip_file, "dir/foo"), c("foo", "bar"))
})

test_that("is_windows_archive", {
  good <- c("foo/bar.zip", "bar.zip", "c:/foo/bar.zip", "c:\\foo.zip",
            "c:\\foo\\bar.zip", "\\\\share\\dir\\bar.zip")

  bad <- c("foobar", "foo.zip1", "x.zip.tar", "foo/zip.tar",
           "c:/foo/bar.tar.gz", "c:\\foo.tar.gz", "c:\\foo\\bar.zipp",
           "\\\\share\\dir\\bar.zi")

  for (x in good) expect_true (is_windows_archive(x))
  for (x in bad)  expect_false(is_windows_archive(x))
})
