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
#' @param clazz the name of the class e.g. "Element" for the Element class.
#' Optional, if omitted it checks that the object is a reference class
#' @export
isRc <- function(x, clazz = "refClass") {
  is(x, clazz)
}

#' @describeIn utils Trim whitespace
#' @param x the object to trim
#' @export
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

notImplemented <- function(className, methodName, ...) {
  print(paste(paste0(className, "$", methodName), "Not implemented,", ...))
}