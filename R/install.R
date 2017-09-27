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
    res <- lapply(filenames, function(file) {
      with_handlers(install_package(file, lib = lib, lock = lock),
        pkginstall_installed = inplace(function(x) {
          message(format(x))
        }),
        pkginstall_built = inplace(function(x) message(format(x))),
        pkginstall_begin = inplace(function(x) message(format(x))),
        error = exiting(function(e) {
          name <- basename(e$package %||% e$path %||% file)
          message(glue("{red_cross()} Failed {name}"))
          stop(e)
        })
        )
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
          pkginstall::install_packages(filenames, lib = lib, lock = lock, num_workers = num_workers)
        })
  })

  running <- character()
  bar <- progress::progress_bar$new(
    total = length(filenames),
    format = "[:current/:total] :elapsedfull | ETA: :eta | :packages",
    stream = stdout(),
    show_after = 0
  )

  done <- FALSE
  repeat {
    res <- poll(processes, -1)
    output_ready <- vapply(res, function(x) any(x == "ready"), logical(1))
    for (i in which(!done & output_ready)) {
      lines <- processes[[i]]$read_error_lines()
      output_lines <- processes[[i]]$read_output_lines()
      lines <- remove_spaces(lines)
      if (length(lines) > 0) {
        is_running <- grepl("^Building", strip_style(lines))
        running <- union(running, sub("^Building ", "", strip_style(lines[is_running])))
        finished <- lines[!is_running]

        if (length(finished) > 0 && nzchar(finished)) {

          # remove installed and failed from running
          installed <- strip_style(finished)[grepl("^. Installed ", strip_style(finished))]
          installed <- sub(". Installed ([^[:space:]]+).*", "\\1", installed)

          failed <- strip_style(finished)[grepl("^. Failed ", strip_style(finished))]
          failed <- sub(". Failed ([^[:space:]]+).*", "\\1", failed)
          if (length(failed)) {
            bar$terminate()
            lines <- c(lines, processes[[i]]$read_all_error_lines())
            output_lines <- c(output_lines, processes[[i]]$read_all_output_lines())
            lapply(processes, function(x) x$kill(tools::SIGINT))
            stop(collapse(c(lines, output_lines), sep = "\n"), call. = FALSE)
          }
          running <- setdiff(running, c(installed, failed))
          bar$message(glue("{i}: {finished}"))
          bar$tick(length(installed) + length(failed), tokens = list(packages = collapse(running, ", ")))
        }
        if (length(running)) {
          bar$tick(0, tokens = list(packages = collapse(running, ", ")))
        }
      }
    }
    done <- !map_lgl(processes, function(x) x$is_alive())
    if (all(done)) {
      break
    }
  }

  structure(
    Reduce(append, lapply(processes, function(x) x$get_result())),
    class = "installation_results", elapsed = Sys.time() - start)
}

remove_spaces <- function(x) {
  x <- x[!grepl("^[[:space:]]*$", x)]
}

#' @importFrom crayon make_style strip_style
greyish <- function() make_style("darkgrey")

green_tick <- function() green(symbol$tick)
red_cross <- function() red(symbol$cross)

#' @importFrom crayon cyan reset green
#' @importFrom prettyunits pretty_dt
#' @importFrom clisymbols symbol
#' @export
format.pkginstall_installed <- function(x, ...) {
  glue("{green_tick()} Installed {greyish()}{x$package} {cyan}({pretty_dt(x$time)}){reset}")
}

#' @export
format.pkginstall_built <- function(x, ...) {
  glue("{green_tick()} Built {greyish()}{x$package} {cyan}({pretty_dt(x$time)}){reset}")
}

#' @export
format.pkginstall_begin <- function(x, ...) {
  glue("Building {greyish()}{x$package}{reset}")
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
