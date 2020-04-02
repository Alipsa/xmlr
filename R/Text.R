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

setMethod('as.vector', "Text",
  function(x) {
    x$toString()
  }
)

setMethod('as.character', "Text",
  function(x) {
    x$toString()
  }
)