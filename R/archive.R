#' @importFrom utils unzip
#' @importFrom tar untar
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
#' @importFrom tar tar
archive_write_dir <- function(archive, dir, ..., recursive = TRUE, full.names = FALSE) {
  archive <- file.path(normalizePath(dirname(archive)), basename(archive))
  old <- setwd(dir)
  on.exit(setwd(old))
  files <- dir(dir, ..., recursive = recursive, full.names = full.names)
  if (is_windows_archive(archive)) {
    zip(archive, files)
  } else {
    tar(archive, files, tar = "internal")
  }
  invisible()
}
