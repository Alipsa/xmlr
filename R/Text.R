#' Reference class representing text content
#' @details
#' An XML character sequence. Provides a modular, parentable method of representing text.

#' @export
Text <- setRefClass(
  Class = "Text",
  contains = "Content",
  fields = list(
    value = "character"
  ),
  methods = list(
    initialize = function(val) {
      value <<- val
    },

    toString = function() {
      return(value)
    },

    show = function() {
      cat(toString())
    }
  )
)

#' as.vector for Text classes
#' @describeIn Text as.vector(Text)
#' @param x the object to convert
setMethod('as.vector', "Text",
  function(x) {
    x$toString()
  }
)

#' as.character for Text classes
#' @describeIn Text as.character(Text)
#' @param x the object to convert
setMethod('as.character', "Text",
  function(x) {
    x$toString()
  }
)