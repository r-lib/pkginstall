#' Install a local R package
#'
#' @param filename filename of package to install. Can be a source
#' directory, source tarball or binary package.
#' @inheritParams install_binary
#' @inheritParams install_source
install_package <- function(filename, lib = .libPaths()[[1L]],
  lock = getOption("install.lock", TRUE), ...) {
  if (is_binary_package(filename)) {
    return(install_binary(filename, lib, lock))
  }
  install_source(filename, lib, lock)
}

#' Install multiple local packages
#'
#' @param filenames filenames of packages to install. Can be source
#' directories, source tarballs or binary packages.
#' @inheritParams install_binary
#' @param num_workers Number of parallel workers to use
#' @importFrom rlang with_handlers exiting inplace
#' @importFrom processx poll
#' @export
install_packages <- function(filenames, lib = .libPaths()[[1L]],
  lock = getOption("install.lock", TRUE), num_workers = 1) {

  start <- Sys.time()

  if (num_workers == 1) {
    bar <- StatusBar$new()
    res <- lapply(filenames, function(file) {
      with_handlers(install_package(file, lib = lib, lock = lock),
        pkginstall_installed = inplace(function(x) bar$add_message(format(x))),
        pkginstall_built = inplace(function(x) bar$add_message(format(x))),
        pkginstall_begin = inplace(function(x) bar$change_status(format(x))),
        error = exiting(function(e) {
          bar$add_message(glue("{red_cross()} {basename(file)}"))
          e
        }))
    })
    names(res) <- filenames
    bar$remove()
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
          pkginstall::install_packages(filenames, lib = lib, lock = lock, num_workers = num_workers)
        })
  })

  repeat {
    res <- poll(processes, ms = 10 * 1000)
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
format.pkginstall_installed <- function(x, ...) {
  glue("{green_tick()} Installed {make_style('darkgrey')}{x$package} {cyan}({pretty_dt(x$time)}){reset}")
}

#' @export
format.pkginstall_built <- function(x, ...) {
  glue("{green_tick()} Built {make_style('darkgrey')}{x$package} {cyan}({pretty_dt(x$time)}){reset}")
}

#' @export
format.pkginstall_begin <- function(x, ..., width = getOption("width")) {
  glue("Building {make_style('darkgrey')}{x$package}{reset}")
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

is_binary_package <- function(filename) {
  tryCatch({
    verify_binary(filename)
    TRUE
  }, error = function(e) FALSE)
}
