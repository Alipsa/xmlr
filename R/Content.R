#' An abstract reference class representing content that can belong to an Element
#'
#' #' @field m_parent the parent (if any)
#' @export
Content <- setRefClass(
  Class = "Content",
  contains = "AbstractClass",
  fields =  "m_parent",
  methods = list(

    getParent = function() {
      m_parent
    },

    setParent = function(parent) {
      m_parent <<- parent
    }

  )
)
