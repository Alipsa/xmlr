#' An abstract reference class representing content that can belong to an Element
#'
#' @export
Content <- setRefClass(
  Class = "Content",
  contains = "AbstractClass",
  fields = c(
    #' @field m_parent the parent (if any)
    "m_parent"
  ),
  methods = list(

    getParent = function() {
      m_parent
    },

    setParent = function(parent) {
      m_parent <<- parent
    }

  )
)
