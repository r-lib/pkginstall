#' @importFrom R6 R6Class
StatusBar <- R6Class("StatusBar",
  private = list(
    con = stdout(),
    current_status = "\n",
    width = integer(),
    removed = FALSE,
    clear_line = function() {
      cat(file = private$con, sep = "", "\r", strrep(" ", private$width), "\r")
    }
  ),
  public = list(
    initialize = function(width = getOption("width"), con = stdout()) {
      private$width <- width
      private$con <- con
    },
    add_message = function(msg) {
      private$clear_line()
      msg <- resize(collapse(msg, "\n"), private$width)
      cat(file = private$con, msg, "\n", sep = "")
      cat(file = private$con, private$current_status)
    },
    change_status = function(msg) {
      private$clear_line()
      private$current_status <- resize(collapse(msg, "\n"), private$width)
      cat(file = private$con, private$current_status)
    },
    remove = function() {
      private$removed <- TRUE
      private$clear_line()
    }
  )
)


#' @importFrom crayon col_substr
resize <- function(x, width) {
  x_width <- nchar(strip_style(x), "width")
  too_wide <- x_width > width
  if (too_wide) {

    # We need to reset colors in case we chop them off
    x <- paste0(col_substr(x, 1, width - 3), "...")
  }
  x
}

test <- function(x = .4) {
  b <- StatusBar$new(con = stderr())
  b$change_status("Running foo")
  Sys.sleep(x)
  b$change_status("Running bar")
  Sys.sleep(x)
  b$add_message("done baz")
  Sys.sleep(x)
  b$change_status("Running 12345")
  Sys.sleep(x)
  b$remove()
}
