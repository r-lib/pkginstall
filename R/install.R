#' Install multiple binaries
#'
#' @param filenames filenames of tarballs to install
#' @inheritParams install_binary
#' @param num_workers Number of parallel workers to use
#' @importFrom rlang with_handlers exiting
#' @export
install_binaries <- function(filenames, lib = .libPaths()[[1L]],
  lock = getOption("install.lock", TRUE), num_workers = 1) {

  start <- Sys.time()

  if (num_workers == 1) {
    res <- lapply(filenames, function(file) {
      with_handlers(install_binary(file, lib = lib, lock = lock),
        pkginstall_installation = exiting(print),
        error = exiting(function(e) {
          cat(glue("{red_cross()} {file}\n\n"))
          e
        }))
  })
    names(res) <- filenames
    return(structure(res, class = "installation_results", elapsed = Sys.time() - start))
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

    done <- all(!map_lgl(processes, function(x) x$is_incomplete_output()))
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
  structure(
    Reduce(append, lapply(processes, function(x) x$get_result())),
    class = "installation_results", elapsed = Sys.time() - start)
}

#' @importFrom crayon make_style
greyish <- make_style("darkgrey")

green_tick <- function() green(symbol$tick)
red_cross <- function() red(symbol$cross)

#' @importFrom crayon cyan reset green
#' @importFrom prettyunits pretty_dt
#' @importFrom clisymbols symbol
#' @export
print.pkginstall_installation <- function(x, ...) {
  cat(glue("{green_tick()}{make_style('darkgrey')} {x$package} {cyan}({pretty_dt(x$time)}){reset}\n\n"))
  invisible(x)
}

#' @importFrom crayon red reset
#' @importFrom clisymbols symbol
#' @export
print.installation_results <- function(x, ...) {
  failures <- map_lgl(x, inherits, "error")
  if (any(failures)) {
    for (failure in which(failures)) {
      cat(glue("
          \n{red_cross()} {names(x)[[failure]]}
          {red}{conditionMessage(x[[failure]])}{reset}\n
          "))
    }
  }
  successes <- length(x) - sum(failures)
  cat(glue("
      \n{successes} packages installed ({successes}{green_tick()} {sum(failures)}{red_cross()}) in {pretty_dt(attr(x, 'elapsed'))}.\n
      "))
}
