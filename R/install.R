#' Install multiple binaries
#'
#' @param filenames filenames of tarballs to install
#' @inheritParams install_binary
#' @param num_workers Number of parallel workers to use
#' @export
install_binaries <- function(filenames, lib = .libPaths()[[1L]],
  lock = getOption("install.lock", TRUE), num_workers = 1) {

  if (num_workers == 1) {
    return(invisible(vapply(filenames, install_binary, character(1), lib = lib, lock = lock)))
  }

  # Assumes the order of installation is not important
  processes <- lapply(split(filenames, seq_along(filenames) %% num_workers),
    function(files) {
      callr::r_bg(args = list(filenames = files, lib = lib, lock = lock, num_workers = 1), function(filenames, lib, lock, num_workers) pkginstall::install_binaries(filenames))
    })


  repeat {
    res <- processx::poll(processes, ms = 10 * 1000)
    output_ready <- vapply(res, function(x) x[[1]] == "ready", logical(1))

    done <- all(!vapply(processes, function(x) x$is_incomplete_output(), logical(1)))
    if (done) {
      break
    }
    for (i in seq_along(processes)) {#which(output_ready)) {
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
