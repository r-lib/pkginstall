
pkg_data <- new.env()

#' @importFrom cli cli

alert <- function(type, msg, .envir = parent.frame()) {
  if (!is_verbose()) return()
  switch (
    type,
    success = cli$alert_success(msg, .envir = .envir),
    info = cli$alert_info(msg, .envir = .envir),
    warning = cli$alert_warning(msg, .envir = .envir),
    danger = cli$alert_danger(msg, .envir = .envir)
  )
}

#' @importFrom cli get_spinner

create_progress_bar <- function(state) {
  if (!is_verbose()) return()
  pkg_data$spinner <- get_spinner()
  pkg_data$spinner_state <- 1L
  pkg_data$last_msg <- NULL

  cli$progress_bar(
    format = ":xbar ETA :eta | :xbuilt | :xinst | :xmsg",
    total = sum(!state$plan$build_done) + sum(!state$plan$install_done)
  )
}

## TODO: get rid of UTF-8 characters, have fallback

update_progress_bar <- function(state, tick = 0, msg = NULL) {

  if (!is_verbose()) return()
  if (! is.null(msg)) pkg_data$last_msg <- msg

  plan <- state$plan
  total <- nrow(plan)
  installed <- sum(plan$install_done)
  built <- sum(plan$build_done)

  building <- sum(buildingl <- !plan$build_done & !is.na(plan$worker_id))
  installing <- sum(!buildingl & !is.na(plan$worker_id))

  tokens <- list(
    xbar = make_bar(installed / total, built/total, width =  15),
    xbuilt = make_progress_block("📦", built, total, building),
    xinst = make_progress_block("✅", installed, total, installing),
    xmsg = pkg_data$last_msg %||% ""
  )

  state$progress$tick(tick, tokens = tokens)
}

## p1 <= p2 must hold

make_bar <- function(p1, p2, width) {
  width <- width - 2L

  w1 <- if (isTRUE(all.equal(p1, 1))) width else trunc(width * p1)
  w2 <- if (isTRUE(all.equal(p2, 1))) width - w1 else trunc(width * (p2 - p1))

  p1chars <- rep("█", w1)
  p2chars <- rep("░", w2)
  xchars <- rep(" ", max(width - w1 - w2, 0))
  bar <- paste(c("⸨", p1chars, p2chars, xchars, "⸩"), collapse = "")

  crayon::green(bar)
}

make_progress_block <- function(sym, done, total, prog) {
  spin <- pkg_data$spinner$frames[[pkg_data$spinner_state]]
  pkg_data$spinner_state <-
    pkg_data$spinner_state %% length(pkg_data$spinner$frames) + 1L
  paste0(
    sym, "  ",
    done, "/", total,
    if (prog) paste0(" ", spin, " ", prog) else "    "
  )
}

done_progress_bar <- function(state) {
  if (!is_verbose()) return()
  state$progress$terminate()
}
