Namespace <- setRefClass(
  Class ="Namespace",
  fields = list(
    # The prefix mapped to this namespace
    prefix = "character",
    # The URI for this namespac
    uri = "character"
  ),
  methods = list(
    initialize = function(...) {
      args <- list(...)
      argsNames <- names(args)      
      if ("prefix" %in% argsNames) {
        prefix <<- args$prefix
      } else {
        prefix <<- ""
      }
      if ("uri" %in% argsNames) {
        uri <<- args$uri
      } else {
        uri <<- ""
      }
    },
    getPrefix = function() {
      return(prefix)
    },
    getUri = function() {
      return(uri)
    },
    toString = function() {
      return(paste0(prefix, ":", uri))
    }
  )
)

setMethod('as.vector', "Namespace",
  function(x) {
    x$toString()
  }
)

setMethod('as.character', "Namespace",
  function(x) {
    x$toString()
  }
)