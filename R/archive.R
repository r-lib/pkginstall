#' @importFrom utils unzip untar
archive_files <- function(path) {
  stopifnot(file.exists(path))

  path <- normalizePath(path)

  if (is_windows_archive(path)) {
    return(unzip(path, list = TRUE, unzip = "internal")$Name)
  }

  untar(path, list = TRUE, tar = "internal")
}

archive_extract <- function(path, dir) {
  if (is_windows_archive(path)) {
    return(unzip(path, exdir = dir, unzip = "internal"))
  }
  untar(path, exdir = dir, tar = "internal")
}

is_windows_archive <- function(path) {
  grepl("[.]zip$", basename(path))
}

archive_read <- function(path, file) {
  dir <- create_temp_dir()
  on.exit(unlink(dir, recursive = TRUE))
  out_path <- file.path(dir, file)
  if (is_windows_archive(path)) {
    unzip(path, file, exdir = dir, unzip = "internal")
  } else {
    untar(path, file, exdir = dir, tar = "internal")
  }

  readLines(out_path)
}

#' @importFrom zip zip
#' @importFrom utils tar
archive_write_dir <- function(archive, dir, ..., recursive = TRUE, full.names = FALSE) {
  archive <- file.path(normalizePath(dirname(archive)), basename(archive))
  old <- setwd(dir)
  on.exit(setwd(old))
  files <- dir(dir, ..., recursive = recursive, full.names = full.names)
  if (is_windows_archive(archive)) {
    zip(archive, files)
  } else {
    (fix_tar(tar))(archive, files, tar = "internal")
  }
  invisible()
}

fix_tar <- function(tar) {
  # tar in R 3.3.0-3.4.2 does not behave as documented and only accepts
  # directories rather than filenames. It also does not allow relative paths
  # for files in the archive.
  #
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16716
  #
  # Simply removing the conditional in the first argument fixes both of these
  # issues.
  first_expr <- body(tar)[[2]]
  if (identical(first_expr,
      quote(
        files <- if (is.null(files)) list.files(recursive = TRUE, all.files = TRUE,
          full.names = TRUE, include.dirs = TRUE) else list.files(files,
          recursive = TRUE, all.files = TRUE, full.names = TRUE, include.dirs = TRUE)
      ))) {
    body(tar)[[2]] <- NULL
  }
  tar
}
