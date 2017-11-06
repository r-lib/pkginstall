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
#' @param progress show a progress bar of installation progress.
#' @param p5an The installation plan from `pkgdepends::remote`
#' @importFrom rlang with_handlers exiting inplace
#' @importFrom processx poll
#' @importFrom zeallot %<-%
#' @importFrom tibble data_frame
#' @export
install_packages <- function(filenames, lib = .libPaths()[[1L]], plan = get_install_plan(filenames, lib),
  lock = getOption("install.lock", TRUE), num_workers = 1, progress = interactive()) {

  start <- Sys.time()

  bar_fmt <- if (isTRUE(progress)) {
    collapse(sep = " | ", c(
      "[:current/:total] :elapsedfull",
      "ETA: :eta",
      if (num_workers > 1) ":processes/:num_workers",
      ":packages"))
  } else {
    ""
  }

  bar <- progress::progress_bar$new(
    total = if (!is.null(plan)) NROW(plan) else length(filenames),
    format = bar_fmt,
    stream = stdout(),
    show_after = 0
  )

  if (is.null(plan)) {
    res <- lapply(filenames, function(file) {
      bar$tick(0, tokens = list(packages = get_pkg_name(file)))
      format_message <- inplace(function(x) bar$message(format(x)))
      installed_path <- with_handlers(
        pkginstall_installed = format_message,
        pkginstall_built = format_message,
        pkginstall_begin = format_message,
        error = exiting(function(e) {
          name <- basename(e$package %||% e$path %||% file)
          bar$message(glue("{red_cross()} Failed {name}"))
          stop(e)
        }),

        install_package(file, lib = lib, lock = lock)
      )

      bar$tick(1, tokens = list(packages = get_pkg_name(file)))
      installed_path
    })

    names(res) <- filenames
    return(structure(res, class = "installation_results", elapsed = Sys.time() - start))
  }

  running <- character()
  results <- list()
  processes <- list()
  update_progress <- function(count) {
    bar$tick(count, tokens = list(packages = collapse(running, ", "), processes = length(processes), num_workers = num_workers))
  }
  update_progress(0)

  # Currently assumes the order of installation is not important
  c(plan, processes, results) %<-% get_processes(plan, processes = list(), results = list(), num_workers, lib, lock)
  while(length(processes) > 0) {
    res <- poll(processes, -1)
    output_ready <- vapply(res, function(x) any(x == "ready"), logical(1))
    for (i in which(output_ready)) {
      error_lines <- processes[[i]]$read_error_lines()
      output_lines <- processes[[i]]$read_output_lines()
      output_lines <- remove_spaces(output_lines)
      if (length(lines) > 0) {
        is_running <- grepl("^Building", strip_style(output_lines))
        running <- union(running, sub("^Building ", "", strip_style(output_lines[is_running])))
        finished <- output_lines[!is_running]

        if (length(finished) > 0 && nzchar(finished)) {

          # remove installed and failed from running
          installed <- strip_style(finished)[grepl("^. Installed ", strip_style(finished))]
          installed <- sub(". Installed ([^[:space:]]+).*", "\\1", installed)

          failed <- strip_style(finished)[grepl("^. Failed ", strip_style(finished))]
          failed <- sub(". Failed ([^[:space:]]+).*", "\\1", failed)
          if (length(failed)) {
            bar$terminate()
            error_lines <- c(error_lines, processes[[i]]$read_all_error_lines())
            output_lines <- c(output_lines, processes[[i]]$read_all_output_lines())
            lapply(processes, function(x) x$kill(tools::SIGINT))
            stop(collapse(c(error_lines, output_lines), sep = "\n"), call. = FALSE)
          }
          running <- setdiff(running, installed)
          bar$message(finished)
          update_progress(length(installed))
        }
        if (!bar$finished && length(running)) {
          update_progress(0)
        }
      }
    }
    c(plan, processes, results) %<-% get_processes(plan, processes, results, num_workers, lib, lock)
  }
  bar$terminate()

  structure(results, class = "installation_results", elapsed = Sys.time() - start)
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

#' @importFrom crayon blue reset
#' @importFrom clisymbols symbol
#' @export
print.installation_results <- function(x, ...) {
  time <- cyan(pretty_dt(attr(x, 'elapsed')))
  if (length(x) > 1) {
    cat(glue("
        {sum(map_int(x, length))} packages installed in {time}.
        "))
  } else {
    cat(glue("
        {blue}{names(x)}{reset} installed in {time} at {single_quote(x)}.
        "))
  }
  invisible(x)
}

is_binary_package <- function(filename) {
  tryCatch({
    verify_binary(filename)
    TRUE
  }, error = function(e) FALSE)
}

new_install_packages_process <-  function(file, lib, lock) {
  callr::r_bg(
    args = list(
      filenames = file, lib = lib, lock = lock, num_workers = 1,
      crayon.enabled = getOption("crayon.enabled"),
      crayon.colors = getOption("crayon.colors")),

    function(filenames, lib, lock, num_workers, crayon.enabled, crayon.colors) {
      options("crayon.enabled" = crayon.enabled, "crayon.colors" = crayon.colors)
      pkginstall::install_packages(filenames, lib = lib, lock = lock, num_workers = num_workers, plan = NULL)
    })
}

get_processes <- function(plan, processes, results, num_workers, lib, lock) {
  done <- map_lgl(processes, function(x) !x$is_alive() && !x$is_incomplete_output() && !x$is_incomplete_error())
  results <- append(results, lapply(processes[done], function(x) x$get_result()))
  processes <- processes[!done]

  # Removed installed packages from dependencies
  installed <- unlist(lapply(results, function(x) basename(unlist(x))))
  plan$dependencies <- lapply(plan$dependencies, setdiff, installed)
  ready <- which(plan$binary | lengths(plan$dependencies) == 0)

  while (length(ready) && length(processes) < num_workers) {
    # Distribute all binary packages to a single worker
    # TODO: maybe distribute them to all workers?
    is_binary <- plan$binary
    if (any(is_binary)) {
      processes[[length(processes) + 1]] <- new_install_packages_process(plan$file[is_binary], lib, lock)
      binary_pkgs <- plan$package[is_binary]
      plan <- plan[!is_binary, ]
      ready <- which(lengths(plan$dependencies) == 0)
    } else {
      i <- ready[[1]]
      processes[[length(processes) + 1]] <- new_install_packages_process(plan$file[[i]], lib, lock)
      plan <- plan[-1 * i, ]
      ready <- ready[-1]
    }
  }

  list(plan, processes, results)
}

#' @importFrom pkgdepends remotes
get_install_plan <- function(filenames, library) {
  r <- remotes$new(glue("local::{filenames}"), library = library)
  r$solve()
  r$download_solution()
  plan <- r$get_install_plan()
  plan[plan$type != "installed", ]
}
