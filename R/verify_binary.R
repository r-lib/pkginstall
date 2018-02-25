#' Verify a R binary archive is valid
#'
#' @inheritParams install_binary
#' @return A [desc::desc()] object of the DESCRIPTION file from the binary
#'   archive.
#' @importFrom archive archive archive_read archive_write_dir
#' @importFrom desc desc
#' @importFrom withr local_connection
#' @importFrom utils head
#' @export
verify_binary <- function(filename) {

  tarball <- archive(filename)

  pkg <- get_pkg_name(tarball)

  binary_archive_files <- c(
    file.path(pkg, "Meta", "package.rds"),
    file.path(pkg, "DESCRIPTION")
  )

  valid_binary_archive <- all(binary_archive_files %in% tarball$path)

  if (!valid_binary_archive) {
    missing_files <- binary_archive_files[binary_archive_files %!in% tarball$path]
    abort(type = "invalid_input", "
      {filename} is not a valid binary, it does not contain {missing_files*}.
      ",
      package = pkg)
  }

  desc_path <- file.path(pkg, "DESCRIPTION")
  desc_lines <- readLines(local_connection(archive_read(tarball, desc_path)))
  if (length(desc_lines) == 0) {
    abort(type = "invalid_input", "
      {filename} is not a valid binary, {desc_path} is empty.
      ",
      package = pkg)
  }
  desc <- desc(text = desc_lines)

  if (is.na(desc$get("Built"))) {
    abort(type = "invalid_input", "
      {filename} is not a valid binary, no 'Built' entry in {desc_path}.
      ",
      package = pkg)
  }

  desc
}

#' @importFrom desc desc

verify_extracted_package <- function(filename, parent_path) {

  pkg_name <- dir(parent_path)
  pkg_path <- file.path(parent_path, pkg_name)

  if (length(pkg_name) == 0) {
    abort(type = "invalid_input",
      "{filename} is not a valid R package, it is an empty archive")

  } else if (length(pkg_name) > 1) {
    abort(type = "invalid_input",
      "{filename} is not a valid R package, it should contain a
      single directory")
  }

  rel_package_files <- c(
    file.path(pkg_name, "Meta", "package.rds"),
    file.path(pkg_name, "DESCRIPTION")
  )
  package_files <- file.path(parent_path, rel_package_files)

  has_files <- file.exists(package_files)
  if (!all(has_files)) {
    miss <- rel_package_files[! has_files]
    abort(type = "invalid_input",
      "{filename} is not a valid binary, it does not contain {miss*}.",
      package = pkg_name)
  }

  rel_dsc_file <- file.path(pkg_name, "DESCRIPTION")
  dsc_file <- file.path(pkg_path, "DESCRIPTION")
  dsc <- tryCatch(
    desc(dsc_file),
    error = function(e) {
      abort(type = "invalid_input",
        "{filename} is not a valid binary, invalid {rel_dsc_file}.",
        package = pkg_name)
    }
  )

  if (!length(dsc$fields())) {
    abort(type = "invalid_input",
      "{filename} is not a valid binary, {rel_dsc_file} is empty.",
      package = pkg_name)
  }

  dsc_pkg <- dsc$get("Package")
  if (is.na(dsc_pkg)) {
    abort(type = "invalid_input",
      "{filename} has no `Package` entry in {rel_dsc_file}",
      package = pkg_name)
  }

  if (pkg_name != str_trim(dsc_pkg[[1]])) {
    abort(type = "invalid_input",
      "{filename} is not a valid binary, package name mismatch in
      archive and in {rel_dsc_file}",
      package = pkg_name)
  }

  if (is.na(dsc$get("Built"))) {
    abort(type = "invalid_input",
      "{filename} is not a valid binary, no 'Built' entry in {rel_dsc_file}.",
      package = pkg_name)
  }

  list(name = pkg_name, path = pkg_path, desc = dsc)
}
