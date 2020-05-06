#' Common utility functions

#' @describeIn utils Check if the object is a reference class, similar to isS4().
#' @param x the object to check
#' @param clazz the name of the class e.g. "Element" for the Element class.
#' Optional, if omitted it checks that the object is a reference class
#' @return A boolean indicating whether the object \code{x} belongs to the class or not
#' @export
isRc <- function(x, clazz = "refClass") {
  is(x, clazz)
}

# internal
notImplemented <- function(className, methodName, ...) {
  warning(paste(paste0(className, "$", methodName), "Not implemented,", ...))
}

# internal
whitespace <- c(" ", "\t", "\n", "\r" , "\v", "\f")

# internal
hasWhiteSpace <- function(string) {
  if (is.null(string)) return(FALSE)
  str <- strsplit(string, "")[[1]]
  any(str %in% whitespace)
}

# internal
isWhiteSpaceChar <- function(char) {
  if (is.null(char)) return(FALSE)
  if (nchar(char, keepNA=TRUE) > 1)  return(hasWhiteSpace(char))
  char %in% whitespace
}