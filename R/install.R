
#' Install multiple local packages
#'
#' @param filenames filenames of packages to install. Can be source
#' directories, source tarballs or binary packages.
#' @inheritParams install_binary
#' @param num_workers Number of parallel workers to use
#' @export
install_packages <- function(filenames, lib = .libPaths()[[1L]],
                             num_workers = 1) {

  plan <- get_install_plan(filenames, lib)
  install_packages_internal(filenames, lib, num_workers, plan)
}

#' @importFrom rlang with_handlers exiting inplace
#' @importFrom processx poll
install_packages_internal <- function(filenames, lib, num_workers, plan) {

  start <- Sys.time()

  progress <- is_verbose()
  bar <- setup_progress_bar(progress, plan, num_workers)

  running <- character()
  results <- list()
  processes <- list()
  update_progress <- function(count) {
    if (progress) {
      bar$tick(count, tokens = list(packages = collapse(running, ", "), processes = length(processes), num_workers = num_workers))
    }
  }
  update_progress(0)

  # Currently assumes the order of installation is not important
  events <- get_events(list(plan = plan, processes = list(), results = list()), num_workers, lib, bar)
  while(running_processes(events)) {
    for (output in get_output(events)) {
      is_running <- grepl("^Building", strip_style(output$output))
      running <- union(running, sub("^Building ", "", strip_style(output$output[is_running])))
      finished <- output$output[!is_running]

      if (length(finished) > 0 && nzchar(finished)) {

        # remove installed from running
        installed <- strip_style(finished)[grepl("^. Installed ", strip_style(finished))]
        installed <- sub(". Installed ([^[:space:]]+).*", "\\1", installed)

        running <- setdiff(running, installed)
        if (progress) {
          bar$message(finished)
          update_progress(length(installed))
        }
      }
      if (progress && !bar$finished && length(running)) {
        update_progress(0)
      }
    }
    events <- get_events(events, num_workers, lib, bar)
  }
  if (progress) bar$terminate()

  structure(unlist(events$results, recursive = FALSE), class = "installation_results", elapsed = Sys.time() - start)
}

install_packages_simple <- function(filenames, lib, metadata, vignettes) {

  start <- Sys.time()

  res <- lapply(seq_along(filenames), function(idx) {
    file <- filenames[idx]
    meta <- metadata[[idx]]
    vig <- vignettes[idx]
    format_message <- inplace(function(x) cat(format(x)))
    installed_path <- with_handlers(
      pkginstall_installed = format_message,
      pkginstall_built = format_message,
      pkginstall_begin = format_message,
      error = exiting(function(e) {
        name <- basename(e$package %||% e$path %||% file)
        cat(glue("{red_cross()} Failed {name}"))
        stop(e)
      }), {
        ## bar$tick(0, tokens = list(packages = get_pkg_name(file)))
        install_package(file, lib = lib, metadata = meta, vignettes = vig)
      })
    ## bar$tick(1, tokens = list(packages = get_pkg_name(file)))
    installed_path
  })

  names(res) <- filenames
  return(structure(res, class = "installation_results", elapsed = Sys.time() - start))
}

setup_progress_bar <- function(progress, plan, num_workers) {
  if (isTRUE(progress)) {
    bar_fmt <- collapse(sep = " | ", c(
      "[:current/:total] :elapsedfull",
      "ETA: :eta",
      if (num_workers > 1) ":processes/:num_workers",
      ":packages"
    ))
    progress::progress_bar$new(
      total = nrow(plan),
      format = bar_fmt,
      stream = stdout(),
      show_after = 0
    )
  }
}

#' Install a local R package
#'
#' @param filename filename of package to install. Can be a source
#' directory, source tarball or binary package.
#' @param vignettes whether to (re)build the vignettes of the packages.
#' It is ignored for binary packages.
#' @inheritParams install_binary
#' @inheritParams install_source
#' @keywords internal
install_package <- function(filename, lib, metadata, vignettes) {
  if (is_binary_package(filename)) {
    install_binary(filename, lib, metadata = metadata)
  } else {
    install_source(filename, lib, metadata = metadata, vignettes = vignettes)
  }
}

get_events <- function(events, num_workers, lib, bar) {
  done <- map_lgl(events$processes, function(x) !x$is_alive() && !x$is_incomplete_output() && !x$is_incomplete_error())
  failed <- map_lgl(events$processes, function(x) !x$is_alive() && x$get_exit_status() != 0)
  if (any(failed)) {
    kill_all_processes(events$processes)
    bar$terminate()
    events$processes$get_result[which(failed)[[1]]]
  }
  results <- append(events$results, lapply(events$processes[done], function(x) x$get_result()))
  processes <- events$processes[!done]
  plan <- events$plan

  # Removed installed packages from dependencies
  installed <- unlist(lapply(results, function(x) basename(unlist(x))))
  plan$dependencies <- lapply(plan$dependencies, setdiff, installed)
  ready <- which(plan$binary | lengths(plan$dependencies) == 0)

  i <- 1
  while (i <= length(ready) && length(processes) < num_workers) {
    # Distribute all binary packages to a single worker
    # TODO: maybe distribute them to all workers?
    is_binary <- plan$binary
    if (any(is_binary)) {
      processes[[length(processes) + 1]] <- new_install_packages_process(
        plan$file[is_binary], plan$metadata[is_binary],
        plan$vignettes[is_binary], lib)
      binary_pkgs <- plan$package[is_binary]
      plan <- plan[!is_binary, ]
      ready <- which(lengths(plan$dependencies) == 0)
    } else {
      processes[[length(processes) + 1]] <- new_install_packages_process(
        plan$file[[ready[[i]]]], plan$metadata[ready[i]],
        plan$vignettes[ready[i]], lib)
      i <- i + 1
    }
  }
  plan <- remove_rows(plan, ready[seq_len(i - 1)])

  list(plan = plan, processes = processes, results = results)
}


#' @importFrom pkgdepends remotes
get_install_plan <- function(filenames, library) {
  r <- remotes$new(glue("local::{filenames}"), library = library)
  r$solve()
  r$download_solution()
  plan <- r$get_install_plan()

  installed <- plan$type == "installed"

  # Remove installed dependencies from the dependency lists.
  plan$dependencies <- lapply(plan$dependencies, setdiff, plan$package[installed])

  # Return uninstalled packages
  plan[!installed, ]
}

get_output <- function(x) {
  res <- poll(x$processes, -1)
  idx <- which(vapply(res, function(x) any(x == "ready"), logical(1)))
  lapply(idx,
    function(i) {
      list(
        output = remove_spaces(x$processes[[i]]$read_output_lines()),
        error = x$processes[[i]]$read_error_lines(),
        index = i)
    })
}

running_processes <- function(x) {
  length(x$processes) > 0
}

kill_all_processes <- function(events) {
  for (proc in events$processes) {
    proc$kill(tools::SIGINT)
  }
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
  time <- cyan("(", pretty_dt(attr(x, 'elapsed')), ")", sep = "")
  if (length(x) > 1) {
    cat(glue("
        Installed {blue}{sum(map_int(x, length))}{reset} packages {time}.
        "))
  } else {
    cat(glue("
        Installed {blue}{basename(names(x))}{reset} {time}.
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

new_install_packages_process <-  function(file, metadata, vignettes, lib) {
  callr::r_bg(
    args = list(
      filenames = file, lib = lib, metadata = metadata,
      vignettes = vignettes, crayon.enabled = crayon::has_color(),
      crayon.colors = crayon::num_colors()),

    function(filenames, metadata, vignettes, lib,
             crayon.enabled, crayon.colors) {
      options("crayon.enabled" = crayon.enabled, "crayon.colors" = crayon.colors)
      get("install_packages_simple", asNamespace("pkginstall"))(
        filenames, lib = lib, metadata = metadata, vignettes = vignettes)
    })
}

remove_rows <- function(x, i) {
  if (length(i) == 0) {
    return(x)
  }
  x[-1 * i, ]
}
