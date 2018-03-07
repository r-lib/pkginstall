
#' Perform a package installation plan, as creted by pkgdepends
#'
#' @param plan Package plan object, returned by pkgdepends
#' @param lib Library directory to install to.
#' @param num_workers Number of worker processes to use.
#' @return Information about the installation process.
#' 
#' @importFrom rlang with_handlers exiting inplace
#' @importFrom callr poll
#' @export

install_package_plan <- function(plan, lib = .libPaths()[[1]],
                                 num_workers = 1) {

  start <- Sys.time()

  required_columns <- c(
    "type", "binary", "dependencies", "file", "vignettes",
    "needs_compilation", "metadata", "package")
  stopifnot(
    inherits(plan, "data.frame"),
    all(required_columns %in% colnames(plan)),
    is_string(lib),
    is_count(num_workers, min = 1L)
  )

  config <- list(lib = lib, num_workers = num_workers)
  state <- make_start_state(plan, config)

  withCallingHandlers({

    ## Initialise one task for each worker
    for (i in seq_len(state$config$num_workers)) {
      ## TODO: update progress bar here
      task <- select_next_task(state)
      state <- start_task(state, task)
    }

    while (1) {
      if (are_we_done(state)) break;
      ## TODO: update progress bar here

      events <- poll_workers(state)
      state <- handle_events(state, events)
      task  <- select_next_task(state)
      state <- start_task(state, task)
    }
  }, error = function(e) kill_all_processes(state))

  create_install_result(state)
}

make_start_state <- function(plan, config) {

  ## We store the data about build and installation here
  install_cols <- data.frame(
    stringsAsFactors = FALSE,
    build_done = plan$type == "installed" | plan$binary,
    build_time = ifelse(plan$binary, 0, NA_real_),
    build_error = I(rep_list(nrow(plan), list())),
    build_stdout = I(rep_list(nrow(plan), character())),
    build_stderr = I(rep_list(nrow(plan), character())),
    install_done = plan$type == "installed",
    install_time = ifelse(plan$type == "installed", 0, NA_real_),
    install_error = I(rep_list(nrow(plan), list())),
    install_stdout = I(rep_list(nrow(plan), character())),
    install_stderr = I(rep_list(nrow(plan), character())),
    worker_id = NA_character_
  )
  plan <- cbind(plan, install_cols)

  list(
    plan = plan,
    workers = list(),
    config = config)
}

are_we_done <- function(state) {
  all(state$plan$install_done)
}

#' @importFrom callr poll

poll_workers <- function(state) {
  if (length(state$workers)) {
    timeout <- get_timeout(state)
    procs <- lapply(state$workers, "[[", "process")
    res <- poll(procs, ms = timeout)
    vapply(res, function(x) "ready" %in% x, logical(1))

  } else {
    logical()
  }
}

## TODO: No timeout currently

get_timeout <- function(state) -1L

handle_events <- function(state, events) {
  for (i in which(events)) state <- handle_event(state, i)
  state$workers <- drop_nulls(state$workers)
  state
}

handle_event <- function(state, evidx) {
  proc <- state$workers[[evidx]]$process

  ## Read out stdout and stderr. If process is done, then read out all
  if (proc$is_alive()) {
    state$workers[[evidx]]$stdout <-
      c(state$workers[[evidx]]$stdout, out <- proc$read_output(n = 10000))
    state$workers[[evidx]]$stderr <-
      c(state$workers[[evidx]]$stderr, err <- proc$read_error(n = 10000))
  } else {
    state$workers[[evidx]]$stdout <-
      c(state$workers[[evidx]]$stdout, out <- proc$read_all_output())
    state$workers[[evidx]]$stderr <-
      c(state$workers[[evidx]]$stderr, err <- proc$read_all_error())
  }

  ## If there is still output, then wait a bit more
  if (proc$is_incomplete_output() || proc$is_incomplete_error()) {
    return(state)
  }

  ## Otherwise we are done. Remove worker
  worker <- state$workers[[evidx]]
  state$workers[evidx] <- list(NULL)

  ## Post-process, this will throw on error
  if (is.function(proc$get_result)) proc$get_result()

  ## Cut stdout and stderr to lines
  worker$stdout <- cut_into_lines(worker$stdout)
  worker$stderr <- cut_into_lines(worker$stderr)

  ## Record what was done
  stop_task(state, worker)
}

select_next_task <- function(state) {

  ## Cannot run more workers?
  if (length(state$workers) >= state$config$num_workers) {
    return(task("idle"))
  }

  ## Can we select a source package build? Do that.
  can_build <- which(
    ! state$plan$build_done &
    vapply(state$plan$dependencies, length, integer(1)) == 0 &
    is.na(state$plan$worker_id))

  if (any(can_build)) {
    pkgidx <- can_build[1]
    return(task("build", pkgidx = pkgidx))
  }

  ## TODO: can we select a binary that is depended on by a source package?

  ## Otherwise select a binary if there is one
  can_install <- which(
    state$plan$build_done &
    ! state$plan$install_done &
    is.na(state$plan$worker_id))

  if (any(can_install)) {
    pkgidx <- can_install[1]
    return(task("install", pkgidx = pkgidx))
  }

  ## Looks like nothing else to do
  task("idle")
}

task <- function(name, ...) {
  list(name = name, args = list(...))
}

start_task <- function(state, task) {
  if (task$name == "idle") {
    state

  } else if (task$name == "build") {
    start_task_build(state, task)

  } else if (task$name == "install") {
    start_task_install(state, task)

  } else {
    stop("Unknown task, internal error")
  }
}

get_worker_id <- (function() {
  id <- 0
  function() {
    id <<- id + 1
    as.character(id)
  }
})()

#' @importFrom pkgbuild pkgbuild_process

start_task_build <- function(state, task,
                             dummy = FALSE, dummy_args = list()) {
  pkgidx <- task$args$pkgidx
  path <- state$plan$file[pkgidx]
  vignettes <- state$plan$vignettes[pkgidx]
  needs_compilation <- !identical(state$plan$needs_compilation[pkgidx], "no")
  tmp_dir <- create_temp_dir()
  lib <- state$config$lib

  px <- if (dummy) {
    do.call(make_dummy_worker_process, dummy_args)

  } else {
    pkgbuild_process$new(
      path, tmp_dir, binary = TRUE, vignettes = vignettes,
      needs_compilation = needs_compilation, compile_attributes = FALSE,
      args = glue("--library={lib}"))
  }

  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character(), stderr = character(), dummy = dummy)
  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state
}

start_task_install <- function(state, task,
                               dummy = FALSE, dummy_args = list()) {
  pkgidx <- task$args$pkgidx
  filename <- state$plan$file[pkgidx]
  lib <- state$config$lib
  metadata <- state$plan$metadata[[pkgidx]]

  px <- if (dummy) {
    do.call(make_dummy_worker_process, dummy_args)

  } else {
    make_install_process(filename, lib = lib, metadata = metadata)
  }

  worker <- list(
    id = get_worker_id(), task = task, process = px,
    stdout = character(), stderr = character(), dummy = dummy)

  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state
}

stop_task <- function(state, worker) {
  if (worker$task$name == "build") {
    stop_task_build(state, worker)

  } else if (worker$task$name == "install") {
    stop_task_install(state, worker)

  } else {
    stop("Unknown task, internal error")
  }
}

stop_task_build <- function(state, worker) {

  ## TODO: make sure exit status is non-zero on build error!
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx

  ## Need to save the name of the built package
  if (success && !worker$dummy) {
    state$plan$file[pkgidx] <- worker$process$get_built_file()
  }

  pkg <- state$plan$package[pkgidx]
  state$plan$build_done[[pkgidx]] <- TRUE
  ## TODO: build time
  state$plan$build_error[[pkgidx]] <- ! success
  state$plan$build_stdout[[pkgidx]] <- worker$stdout
  state$plan$build_stderr[[pkgidx]] <- worker$stderr
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    abort("Failed to build source package {pkg}.")
  }

  state
}

stop_task_install <- function(state, worker) {

  ## TODO: make sure the install status is non-zero on exit
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx
  pkg <- state$plan$package[pkgidx]
  state$plan$install_done[[pkgidx]] <- TRUE
  ## TODO: install time
  state$plan$install_error[[pkgidx]] <- ! success
  state$plan$install_stdout[[pkgidx]] <- worker$stdout
  state$plan$install_stderr[[pkgidx]] <- worker$stderr
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    abort("Failed to install binary package {pkg}.")
  }

  ## Need to remove from the dependency list
  state$plan$dependencies <- lapply(state$plan$dependencies, setdiff, pkg)

  state
}

create_install_result <-  function(state) {
  result <- state$plan
  class(result) <- c("pkginstall_result", class(result))
  result
}

kill_all_processes <- function(state) {
  alive <- FALSE
  for (i in seq_along(state$workers)) {
    proc <- state$workers[[i]]$process
    proc$signal(tools::SIGINT)
    alive <- alive || proc$is_alive()
  }

  if (alive) {
    for (i in seq_along(state$workers)) {
      proc <- state$workers[[i]]$process
      proc$wait(200)
      proc$kill()
    }
  }
}
