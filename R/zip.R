
make_unzip_process <- function(zipfile, files = NULL, exdir = ".",
                               restore_times = TRUE, post_process = NULL) {
  r_unzip_process$new(zipfile, files, exdir, restore_times, post_process)
}

r_unzip_process <- R6Class(
  "r_unzip_proces",
  inherit = callr::r_process,

  public = list(
    initialize = function(zipfile, files = NULL, exdir = ".",
                          restore_times = TRUE, post_process = NULL)
      runzip_init(self, private, super, zipfile, files, exdir,
                  restore_times, post_process)
  ),

  private = list(
    options = NULL
  )
)

#' @importFrom callr r_process_options

runzip_init <- function(self, private, super, zipfile, files, exdir,
                        restore_times, post_process) {

  options <- list(
    zipfile = normalizePath(zipfile),
    files = files,
    exdir = exdir,
    restore_times = restore_times,
    post_process = post_process)

  process_options <- r_process_options()
  process_options$func <- function(options) {
    # nocov start
    ret <- utils::unzip(
      zipfile = options$zipfile,
      files = options$files,
      list = FALSE,
      exdir = options$exdir,
      setTimes = options$restore_times,
      unzip = "internal"
    )

    if (!is.null(options$post_process)) options$post_process() else ret
    # nocov end
  }
  process_options$args <- list(options = options)
  super$initialize(process_options)
}
