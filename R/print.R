
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
