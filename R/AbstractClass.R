#' Reference Class representing a non instantiable class
#' @description An abstract base class with some utility methods
#' @export
AbstractClass <- setRefClass(
  Class = "AbstractClass",
  methods = list (
      initialize = function() {         
         stop("AbstractClass is an abstract class that can't be initialized.")
      },
      preventInstatiation = function(self) {
        stop(paste(class(self), "is an abstract class that can't be initialized."))
      }
   )
)