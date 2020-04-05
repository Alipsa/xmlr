#' A reference class representing an tag (element) attribute

#' @export
Attribute <- setRefClass(
  Class = "Attribute",
  fields = list(
    attributeName = "character",
    attributeValue = "character",
    attributeParent = "Element"
  ),
  methods = list(
    #' @param name The name (character) of the new Attribute
    #' @param value The value (character) of the new Attribute
    initialize = function(name=character(0), value=character(0)) {
      attributeName <<- name
      attributeValue <<- value
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