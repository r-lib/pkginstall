#' Install a R binary package
#'
#' @param filename filename of built binary package to install
#' @param lib library to install packages into
#' @param metadata Named character vector of metadata entries to be added
#'   to the \code{DESCRIPTION} after installation.
#' @param quiet Whether to suppress console output.
#' @importFrom filelock lock unlock
#' @importFrom rlang cnd cnd_signal
#' @export
install_binary <- function(filename, lib = .libPaths()[[1L]],
                           metadata = NULL, quiet = !interactive()) {

  stopifnot(
    is_string(filename), file.exists(filename),
    is_string(lib),
    all_named(metadata),
    is_flag(quiet))

  px <- make_install_process(filename, lib = lib, metadata = metadata)
  stdout <- ""
  stderr <- ""

  if (!quiet) {
    bar <- cliapp$new()$progress_bar(
      format = paste0(":spin Installing ", filename))
  }

  repeat {
    px$poll_io(100)
    if (!quiet) bar$tick(0)
    stdout <- paste0(stdout, px$read_output())
    stderr <- paste0(stderr, px$read_error())
    if (!px$is_alive() &&
        !px$is_incomplete_output() && !px$is_incomplete_error()) {
      break
    }
  }

  if (!quiet) bar$terminate()
  if (px$get_exit_status() != 0) {
    stop("Package installation failed\n", stderr)
  }

  if (!quiet) cliapp$new()$alert_success(paste0("Installed ", filename))

  invisible(px$get_result())
}

install_extracted_binary <- function(filename, lib_cache, pkg_cache, lib,
                                     metadata, now) {

  pkg <- verify_extracted_package(filename, pkg_cache)
  add_metadata(pkg$path, metadata)
  pkg_name <- pkg$name

  lockfile <- lock_cache(lib_cache, pkg_name, getOption("install.lock"))
  on.exit(unlock(lockfile), add = TRUE)

  installed_path <- file.path(lib, pkg_name)
  if (file.exists(installed_path)) {
    # First move the existing library (which still works even if a process has
    # the DLL open), then try to delete it, which may fail if another process
    # has the file open.
    move_to <- file.path(create_temp_dir(), pkg_name)
    ret <- file.rename(installed_path, move_to)
    if (!ret) {
      abort(type = "filesystem",
        "Failed to move installed package at {installed_path}",
        package = pkg_name)
    }
    ret <- unlink(move_to, recursive = TRUE, force = TRUE)
    if (ret != 0) {
      warn(type = "filesystem",
        "Failed to remove installed package at {move_to}",
        package = pkg_name)
    }
  }
  ret <- file.rename(pkg$path, installed_path)
  if (!ret) {
    abort(type = "filesystem",
      "Unable to move package from {pkg$path} to {installed_path}",
      package = pkg_name)
  }

  cnd_signal(
    cnd("pkginstall_installed",
      package = pkg_name, path = installed_path, time = Sys.time() - now, type = "binary"))

  installed_path
}

#' @importFrom utils modifyList
add_metadata <- function(pkg_path, metadata) {
  if (!length(metadata)) return()

  ## During installation, the DESCRIPTION file is read and an package.rds
  ## file created with most of the information from the DESCRIPTION file.
  ## Functions that read package metadata may use either the DESCRIPTION
  ## file or the package.rds file, therefore we attempt to modify both of
  ## them, and return an error if neither one exists.

  source_desc <- file.path(pkg_path, "DESCRIPTION")
  binary_desc <- file.path(pkg_path, "Meta", "package.rds")
  if (file.exists(source_desc)) {
    do.call(desc::desc_set, c(as.list(metadata), list(file = source_desc)))
  }

  if (file.exists(binary_desc)) {
    pkg_desc <- base::readRDS(binary_desc)
    desc <- as.list(pkg_desc$DESCRIPTION)
    desc <- modifyList(desc, as.list(metadata))
    pkg_desc$DESCRIPTION <- stats::setNames(as.character(desc), names(desc))
    base::saveRDS(pkg_desc, binary_desc)
  }

  if (!file.exists(source_desc) && !file.exists(binary_desc)) {
    stop("No DESCRIPTION found!", call. = FALSE)
  }
}
