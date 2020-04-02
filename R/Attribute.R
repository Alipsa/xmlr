Attribute <- setRefClass(
  Class = "Attribute",
  contains = "NamespaceAware",
  fields = list(
    # The local name of the element
    name = "character",
    # The namespace of the element
    namespace = "Namespace",
    value = "character",
    parent = "Element"
  ),
  methods = list(
    initialize = function(...) {
      args <- list(...)
      argsNames <- names(args)   
      if ("name" %in% argsNames) {
        name <<- args$name
      }
      if ("namespace" %in% argsNames) {     
        ns <- args$namespace
        # TODO should we alow NULL or NA here?
        if ("Namespace" == class(ns)) {
         namespace <<- ns 
        } else {
         stop(paste("Attribute constructor, namespace is not an instance of the Namespace class:", class(ns))) 
        }      
      }
      if ("value" %in% argsNames) {
        value <<- args$value
      }
      #print(paste("Element created, name is", private$name))
    },
    toString = function() {
      paste0(name, "='", value, "'")
    },
    show = function() {
      cat(toString())
    },
    getName = function() {
      return(name)
    },
    getValue = function() {
      return(value)
    }
  )
)

setMethod('as.vector', "Attribute",
  function(x) {
    x$toString()
  }
)

setMethod('as.character', "Attribute",
  function(x) {
    x$toString()
  }
)