#' Install multiple binaries
#'
#' @param filenames filenames of tarballs to install
#' @inheritParams install_binary
#' @param num_workers Number of parallel workers to use
#' @importFrom rlang with_handlers exiting
#' @export
install_binaries <- function(filenames, lib = .libPaths()[[1L]],
  lock = getOption("install.lock", TRUE), num_workers = 1) {

  if (num_workers == 1) {
    for (file in filenames) {
      with_handlers(install_binary(file, lib = lib, lock = lock), pkginstall_installation = exiting(print))
    }
    return(invisible())
  }

  # Assumes the order of installation is not important
  processes <- lapply(split(filenames, seq_along(filenames) %% num_workers),
    function(files) {
      callr::r_bg(
        args = list(
          filenames = files, lib = lib, lock = lock, num_workers = 1,
          crayon.enabled = getOption("crayon.enabled"),
          crayon.colors = getOption("crayon.colors")),

        function(filenames, lib, lock, num_workers, crayon.enabled, crayon.colors) {
          options("crayon.enabled" = crayon.enabled, "crayon.colors" = crayon.colors)
          pkginstall::install_binaries(filenames, lib = lib, lock = lock, num_workers = num_workers)
        })
  })

  repeat {
    res <- processx::poll(processes, ms = 10 * 1000)
    output_ready <- vapply(res, function(x) x[[1]] == "ready", logical(1))

    done <- all(!vapply(processes, function(x) x$is_incomplete_output(), logical(1)))
    if (done) {
      break
    }
    for (i in seq_along(processes)) {
      lines <- processes[[i]]$read_output_lines()
      err_lines <- processes[[i]]$read_error_lines()
      if (length(lines) > 0 && nzchar(lines)) {
        cat(glue("{i}: {lines}"), sep = "\n")
      }
      if (length(err_lines) > 0 && nzchar(err_lines)) {
        cat(glue("{crayon::red}{i}:\n {lines}\n{crayon::reset}"))
      }
    }
  }
}

#' @importFrom crayon cyan reset make_style
#' @importFrom prettyunits pretty_dt
#' @export
print.pkginstall_installation <- function(x, ...) {
  cat(glue("{make_style('darkgrey')}Installed {x$package} {cyan}({pretty_dt(x$time)}){reset}\n\n"))
}
