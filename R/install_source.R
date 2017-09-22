#' Install a source package
#'
#' @inheritParams install_binary
#' @inheritParams pkgbuild::build
#' @param ... Additional arguments passed to [pkgbuild::build].
install_source <- function(path, lib = .libPaths()[[1L]],
                               lock = getOption("install.lock", TRUE), quiet = TRUE, ...) {

  now <- Sys.time()

  is_tarball <- !file.info(path)$isdir
  if (is.na(is_tarball)) {
    abort(type = "invalid_input",
      "File {path} does not exist")
  }
  if (identical(is_tarball, TRUE)) {
    pkg_name <- get_archive_pkg_name(archive(path))
  } else {
    pkg_name <- desc::desc_get("Package", path)
  }
  cnd_signal(
    cnd("pkginstall_begin",
      package = pkg_name,
      path = path))

  if (is_tarball) {
    tmp_path <- tempfile()
    archive_extract(path, tmp_path)

    return(
      with_handlers(
        pkginstall_begin = inplace(identity, muffle = TRUE),
        pkginstall_built = inplace(function(cond) {
          cond$time = Sys.time() - now
          cnd_signal(cond)
        }, muffle = TRUE),
        error = inplace(function(cond) {
          cond$package <- pkg_name
          cnd_signal(cond)
        }),
        install_source(file.path(tmp_path, pkg_name), lib, lock, quiet, ...)
      )
    )
  }
  pkg_name <- desc::desc_get("Package", path)

  lib_cache <- library_cache(lib)
  lock <- lock_cache(lib_cache, pkg_name, lock)
  on.exit(unlock(lock))

  tmp_dir <- create_temp_dir(tmpdir = lib_cache)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  with_handlers(pkgbuild::build(path, tmp_dir, binary = TRUE, quiet = quiet, ...),
    system_command_error = exiting(handle_pkgbuild_errors))

  built_files <- list.files(tmp_dir, full.names = TRUE)
  if (length(built_files) != 1L) {
    abort("Source package at `path`: {path} failed to build")
  }
  cnd_signal(
    cnd("pkginstall_built",
      package = pkg_name, path = tmp_dir, time = Sys.time() - now))

  install_binary(built_files, lib, lock = lock)
}

# pkgbuild puts the stderr output in stderr, and R CMD INSTALL / R CMD build
# outputs additional information about the installation directory we don't
# want.
#' @importFrom rematch2 re_match_all
handle_pkgbuild_errors <- function(e) {
  errors <- sub("ERROR: ", "", grep("ERROR: [^\n]+", strsplit(e$stderr, "\n")[[1L]], value = TRUE))
  e$message <- collapse(errors, "\n")
  e$call <- NULL
  stop(e)
}
