#' An abstract class

#' @export
NamespaceAware <- setRefClass(
  Class = "NamespaceAware",
  contains = "AbstractClass",
  methods = list (
      initialize = function() {         
         preventInstatiation(.self)
      }, 
      getNamespacesInScope = function() {
        notImplemented("Not implemented, should return a list of all namespaces that are in scope for the current content.")
      }, 
      getNamespacesIntroduced = function() {
        notImplemented("Not implemented, should return a list of all namespaces that are introduced to the XML tree by this node.")
      },
      getNamespacesInherited = function() {
        notImplemented("Not implemented, should return a list of all namespaces that are in scope for this content, but were not introduced by this content.")
      }
   )
)