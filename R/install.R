#' Install multiple binaries
#'
#' @param filenames filenames of tarballs to install
#' @inheritParams install_binary
#' @param num_workers Number of parallel workers to use
#' @export
install_binaries <- function(filenames, lib = .libPaths()[[1L]],
  lock = getOption("install.lock", TRUE), num_workers = 1) {

  if (num_workers == 1) {
    return(vapply(filenames, install_binary, character(1), lib = lib, lock = lock))
  }

  # Assumes the order of installation is not important
  processes <- lapply(split(filenames, seq_along(filenames) %% num_workers),
    function(files) {
      callr::r_bg(args = list(filenames = files, lib = lib, lock = lock, num_workers = 1),
        function(...) pkginstall::install_binaries(...))
    })


  repeat {
    Sys.sleep(.5)
    done <- all(!vapply(processes, function(x) x$is_alive(), logical(1)))
    if (done) break
  }

  for (i in seq_along(processes)) {#which(output_ready)) {
    lines <- processes[[i]]$get_result()
    if (length(lines) > 0 && nzchar(lines)) {
      cat(collapse(glue("{i}: {lines}"), "\n"), "\n")
    }
  }
}
