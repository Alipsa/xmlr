printp <- function(...) {
  print(paste(...))
}

printp0 <- function(...) {
  print(paste0(...))
}

isRc <- function(x) {
  is(x, "refClass")
}
