#' @useDynLib foo foo_
#' @export
foo <- function() {
  .Call(foo_)
}
