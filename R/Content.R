#' A reference class representing content that can belong to an Element

#' @export
Content <- setRefClass(
  Class = "Content",
  contains = "Parent",
  methods = list(
    show = function() {
      cat(toString())
    }
  )
)