clear_line <- function(width = getOption("width")) {
  cat(sep = "", "\r", strrep(" ", width), "\r")
}

#' @importFrom R6 R6Class
StatusBar <- R6Class("StatusBar",
  private = list(
    current_status = "\n",
    width = integer(),
    clear = logical(),
    removed = FALSE
  ),
  public = list(
    initialize = function(width = getOption("width"), clear = TRUE) {
      private$width <- width
      private$clear <- clear
    },
    add_message = function(msg) {
      clear_line(private$width)
      cat(msg, "\n", sep = "")
      cat(private$current_status)
    },
    change_status = function(msg) {
      clear_line(private$width)
      private$current_status <- msg
      cat(private$current_status)
    },
    remove = function() {
      private$removed <- TRUE
      clear_line(private$width)
    }
  )
)

test <- function(x = .4) {
  b <- status_bar$new()
  b$change_status("Running foo")
  Sys.sleep(x)
  b$change_status("Running bar")
  Sys.sleep(x)
  b$add_message("done baz")
  Sys.sleep(x)
  b$change_status("Running 12345")
  Sys.sleep(x)
  b$terminate()
}
