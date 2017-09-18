#' Verify a R binary archive is valid
#'
#' @inheritParams install_binary
#' @return A [desc::desc()] object of the DESCRIPTION file from the binary
#'   archive.
#' @importFrom archive archive archive_read archive_write_dir
#' @importFrom desc desc
#' @importFrom withr local_connection defer local_libpaths
#' @importFrom utils head
#' @export
verify_binary <- function(filename) {

  tarball <- archive(filename)

  pkg <- get_archive_pkg_name(tarball)

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
