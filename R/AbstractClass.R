# An abstract base class with some utility methods
AbstractClass <- setRefClass(
  Class = "AbstractClass",
  methods = list (
      initialize = function() {         
         stop("AbstractClass is an abstract class that can't be initialized.")
      },
      notImplemented = function(...) {
        print(paste("Not implemented,", ...))
      },
      preventInstatiation = function(self) {
        stop(paste(class(self), "is an abstract class that can't be initialized."))
      }
   )
)