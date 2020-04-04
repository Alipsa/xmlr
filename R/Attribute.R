#' A reference class representing an tag (element) attribute

#' @export
Attribute <- setRefClass(
  Class = "Attribute",
  contains = "NamespaceAware",
  fields = list(
    # The local name of the element
    attributeName = "character",
    # The namespace of the element
    attributeNamespace = "Namespace",
    attributeValue = "character",
    attributeParent = "Element"
  ),
  methods = list(
    #' @param name The name (character) of the new Attribute
    #' @param value The value (character) of the new Attribute
    #' @param namespace The namespace (Namespace class) for the new Attribute, must be named
    initialize = function(name=character(0), value=character(0), namespace=NULL) {
      attributeName <<- name
      attributeValue <<- value
      if (!is.null(namespace)) {
        if (isRc(namespace, "Namespace")) {
          attributeNamespace <<- namespace
        } else {
         stop(paste("Attribute constructor, namespace is not an instance of the Namespace class:", class(namespace)))
        }      
      }
    },

    toString = function() {
      paste0(attributeName, "='", attributeValue, "'")
    },

    show = function() {
      cat(toString())
    },

    getName = function() {
      return(attributeName)
    },

    getValue = function() {
      return(attributeValue)
    }
  )
)

#' as.vector for Attribute classes
#' @describeIn Attribute as.vector(Attribute)
#' @param x the object to convert
setMethod('as.vector', "Attribute",
  function(x) {
    x$toString()
  }
)

#' as.character for Attribute classes
#' @describeIn Attribute as.character(Attribute)
#' @param x the object to convert
setMethod('as.character', "Attribute",
  function(x) {
    x$toString()
  }
)