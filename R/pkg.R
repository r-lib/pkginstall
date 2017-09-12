# unload dll windows
# code available on all platforms
# better error messaging
# parallel installs

#' @importFrom archive archive
install_mac_binary <- function(filename) {

}

#' @importFrom archive archive archive_read archive_write_dir
#' @importFrom desc desc
#' @importFrom withr local_connection defer
verify_mac_binary <- function(filename) {

  tarball <- archive(filename)

  description_path <- grep("DESCRIPTION$", tarball$path, value = TRUE)

  # If there is more than one DESCRIPTION in the tarball use the shortest one,
  # which should always be the top level DESCRIPTION file.
  # This may happen if there are test packages in the package tests for instance.
  description_path <- head(description_path[order(nchar(description_path))], n = 1)

  if (length(description_path) == 0) {
    abort(type = "invalid_input", "
      {filename} is not a valid mac binary, it does not contain a `DESCRIPTION` file.
      ")
  }

  pkg <- dirname(description_path)

  nested_directory <- dirname(pkg) != "."
  if (nested_directory) {
    abort(type = "invalid_input", "
      {filename} is not a valid mac binary, the `DESCRIPTION` file is nested more than 1 level deep {description_path}.
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
      {filename} is not a valid mac binary, it does not contain {missing_files*}.
      ")
  }
  desc_path <- file.path(pkg, "DESCRIPTION")
  desc_lines <- readLines(local_connection(archive_read(tarball, desc_path)))
  if (length(desc_lines) == 0) {
    abort(type = "invalid_input", "
      {filename} is not a valid mac binary, {desc_path} is empty.
      ")
  }
  desc <- desc(text = desc_lines)

  if (is.na(desc$get("Built"))) {
    abort(type = "invalid_input", "
      {filename} is not a valid mac binary, no 'Built' entry in {desc_path}.
      ")
  }

  TRUE
}
