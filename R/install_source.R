#' Install a source package
#'
#' @inheritParams install_binary
#' @inheritParams pkgbuild::build
#' @param ... Additional arguments passed to [pkgbuild::build].
install_source <- function(path, lib = .libPaths()[[1L]],
                               lock = getOption("install.lock", TRUE), quiet = TRUE, ...) {

  is_tarball <- !file.info(path)$isdir
  if (identical(is_tarball, TRUE)) {
    pkg_name <- get_archive_pkg_name(archive(path))
    tmp_path <- tempfile()
    archive_extract(path, tmp_path)
    return(Recall(file.path(tmp_path, pkg_name), lib, lock, quiet, ...))
  }
  pkg_name <- desc::desc_get("Package", path)

  lib_cache <- library_cache(lib, pkg_name, lock)

  tmp_dir <- create_temp_dir(tmpdir = lib_cache)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  pkgbuild::build(path, tmp_dir, binary = TRUE, quiet = quiet, ...)
  built_files <- list.files(tmp_dir, full.names = TRUE)
  if (length(built_files) != 1L) {
    abort("Source package at `path`: {path} failed to build")
  }
  install_binary(built_files, lib, lock = lock)
}
