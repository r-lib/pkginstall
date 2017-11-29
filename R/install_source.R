#' Install a source package
#'
#' @inheritParams install_binary
#' @inheritParams pkgbuild::build
#' @param ... Additional arguments passed to [pkgbuild::build].
#' @export
install_source <- function(path, lib = .libPaths()[[1L]],
                           lock = getOption("install.lock", TRUE), quiet = TRUE,
                           metadata = NULL, ...) {

  now <- Sys.time()

  is_tarball <- !file.info(path)$isdir
  if (is.na(is_tarball)) {
    abort(type = "invalid_input",
      "File {path} does not exist")
  }
  if (identical(is_tarball, TRUE)) {
    pkg_name <- get_pkg_name(path)
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
        install_source(file.path(tmp_path, pkg_name), lib, lock, quiet,
                       metadata = metadata, ...)
      )
    )
  }
  pkg_name <- desc::desc_get("Package", path)

  lib_cache <- library_cache(lib)
  lock <- lock_cache(lib_cache, pkg_name, lock)
  on.exit(unlock(lock))

  tmp_dir <- create_temp_dir(tmpdir = lib_cache)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  with_handlers(pkgbuild::build(path, tmp_dir, binary = TRUE, quiet = quiet, args = glue("--library={lib}"), ...),
    system_command_error = exiting(handle_pkgbuild_errors))

  built_files <- list.files(tmp_dir, full.names = TRUE)
  if (length(built_files) != 1L) {
    abort("Source package at `path`: {path} failed to build")
  }
  cnd_signal(
    cnd("pkginstall_built",
      package = pkg_name, path = tmp_dir, time = Sys.time() - now))

  install_binary(built_files, lib, lock = lock, metadata = metadata)
}

# pkgbuild puts the stderr output in `e$stderr`, and R CMD INSTALL / R CMD build
# outputs additional information about the installation directory we don't
# want.
#' @importFrom rematch2 re_match_all
handle_pkgbuild_errors <- function(e) {
  stderr <- grep("^[^*]+", strsplit(e$stderr, "\n")[[1L]], value = TRUE)
  errors <- grepl("ERROR: [^\n]+", stderr)
  e$message <- collapse(c(sub("^ERROR: ", "", stderr[errors]), parse_compiler_errors(stderr[!errors])), "\n")
  e$call <- NULL
  stop(e)
}

#' @importFrom glue glue_data
#' @importFrom crayon red magenta bold
parse_compiler_errors <- function(lines) {
  m <- rematch2::re_match(lines, "^(?<file>[^:]+):(?<line>[0-9]+):(?:(?<column>[0-9]+?):)? (?<type>error|warning): (?<msg>.+)")
  error_lines <- !is.na(m[[1]])
  m_err <- m[error_lines, ]
  m_err$type <- ifelse(m_err$type == "error", red(m_err$type), magenta(m_err$type))
  lines[error_lines] <- glue_data(m_err, "{bold}{file}:{line}:{column}: {type}: {msg}{reset}")
  lines
}
