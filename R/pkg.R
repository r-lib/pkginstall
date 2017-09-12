# unload dll windows
# code available on all platforms
# better error messaging
# parallel installs

#' Install a R binary package
#'
#' @param filename filename of built binary package to install
#' @param lib library to install packages into
#' @param lock If any value but `FALSE`, use per-package locking when
#'   installing. It defaults to using `getOption("install.lock")` for
#'   compatibility with `utils::install.packages()`.
#' @importFrom archive archive archive_extract
install_binary <- function(filename, lib = .libPaths()[[1L]],
                               lock = getOption("install.lock", TRUE)) {

  desc <- verify_binary(filename)
  pkg_name <- desc$get("Package")

  if (is_loaded(pkg_name)) {
    # unload the package, so any DLLs will be unloaded
    pkgload::unload(pkg_name)
  }

  use_lock <- !identical(lock, FALSE)
  if (use_lock) {
    lockdir <- file.path(lib, glue('00LOCK-{pkg_name}'))
  } else {
    lockdir <- tempfile(tmpdir = lib)
  }
  # Need to check for existing lock _before_ adding the on.exit
  if (file.exists(lockdir)) {
    abort("Installing {pkg_name} failed, lock found at {lockdir}")
  }
  on.exit(unlink(lockdir, recursive = TRUE))

  archive_extract(filename, dir = lockdir)

  installed_path <- file.path(lib, pkg_name)
  if (file.exists(installed_path)) {
    ret <- unlink(installed_path, recursive = TRUE, force = TRUE)
    if (ret != 0L) {
      abort("Failed to remove installed package at {installed_path}")
    }
  }
  ret <- file.rename(file.path(lockdir, pkg_name), installed_path)
  if (!ret) {
    abort("Unable to move package from {lockdir} to {installed_path}")
  }

  installed_path
}

#' @importFrom archive archive archive_read archive_write_dir
#' @importFrom desc desc
#' @importFrom withr local_connection defer local_libpaths
verify_binary <- function(filename) {

  tarball <- archive(filename)

  description_path <- grep("DESCRIPTION$", tarball$path, value = TRUE)

  # If there is more than one DESCRIPTION in the tarball use the shortest one,
  # which should always be the top level DESCRIPTION file.
  # This may happen if there are test packages in the package tests for instance.
  description_path <- head(description_path[order(nchar(description_path))], n = 1)

  if (length(description_path) == 0) {
    abort(type = "invalid_input", "
      {filename} is not a valid binary, it does not contain a `DESCRIPTION` file.
      ")
  }

  pkg <- dirname(description_path)

  nested_directory <- dirname(pkg) != "."
  if (nested_directory) {
    abort(type = "invalid_input", "
      {filename} is not a valid binary, the `DESCRIPTION` file is nested more than 1 level deep {description_path}.
      ")
  }

  binary_archive_files <- c(
    file.path(pkg, "Meta", "package.rds"),
    file.path(pkg, "DESCRIPTION")
  )

  valid_binary_archive <- all(binary_archive_files %in% tarball$path)

  if (!valid_binary_archive) {
    missing_files <- binary_archive_files[binary_archive_files %!in% tarball$path]
    abort(type = "invalid_input", "
      {filename} is not a valid binary, it does not contain {missing_files*}.
      ")
  }
  desc_path <- file.path(pkg, "DESCRIPTION")
  desc_lines <- readLines(local_connection(archive_read(tarball, desc_path)))
  if (length(desc_lines) == 0) {
    abort(type = "invalid_input", "
      {filename} is not a valid binary, {desc_path} is empty.
      ")
  }
  desc <- desc(text = desc_lines)

  if (is.na(desc$get("Built"))) {
    abort(type = "invalid_input", "
      {filename} is not a valid binary, no 'Built' entry in {desc_path}.
      ")
  }

  desc
}
