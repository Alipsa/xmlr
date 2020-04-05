#' A reference class representing the context (Namespace)

#' @export
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
      if (identical(uri,character(0)) | uri == "") {
        return("")
      }
      if (identical(prefix,character(0)) | prefix == "") {
        return(paste0("xmlns='",uri, "'"))
      }
      return(paste0("xmlns:'", prefix, "=", uri, "'"))
    },

    equals = function(other) {
      if (isRc(other, "Namespace")) {
        return(prefix == other$getPrefix() & uri == other$getUri())
      } else {
        return (FALSE)
      }
    }
  )
)

#' as.vector for Namespace classes
#' @describeIn Namespace as.vector(Namespace)
#' @param x the object to convert
setMethod('as.vector', "Namespace",
  function(x) {
    x$toString()
  }
)

#' as.character for Namespace classes
#' @describeIn Namespace as.character(Namespace)
#' @param x the object to convert
setMethod('as.character', "Namespace",
  function(x) {
    x$toString()
  }
)