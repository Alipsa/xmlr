#' Common utility functions

#' @describeIn utils Simple combo of print and paste.
#' @param ... all the stuff that should be printed
#' @export
printp <- function(...) {
  print(paste(...))
}

#' @describeIn utils Simple combo of print and paste0.
#' @param ... all the stuff that should be printed
#' @export
printp0 <- function(...) {
  print(paste0(...))
}

#' @describeIn utils Check if the object is a reference class, similar to isS4().
#' @param x the object to check
#' @export
isRc <- function(x) {
  is(x, "refClass")
}
