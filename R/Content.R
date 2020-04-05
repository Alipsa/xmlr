#' A reference class representing content that can belong to an Element

#' @export
Content <- setRefClass(
  Class = "Content",
  contains = "Parent",
  fields = c("m_parent"),
  methods = list(

    show = function() {
      cat(toString())
    },

    getParent = function() {
      m_parent
    },

    setParent = function(parent) {
      m_parent <<- parent
    }
  )
)