#' An abstract class for rDOM objects which are allowed to contain Content, Element Document.
#' Not sure what to do with
#' getDescendants()
#' canContainContent()
#'...omitting them for now

#' @export
Parent <- setRefClass(
  Class = "Parent",
  contains = c("AbstractClass","NamespaceAware"),
  methods = list(
    initialize = function() {      
      preventInstatiation(.self)
    }, 
    getContentSize = function() {
      notImplemented("should return the number of children in this parent's content list.")
    },
    indexOf = function(child) {
      notImplemented("should return the number of children in this parent's content list.")
    },
    # clone is reserved
    deepClone = function() {
      notImplemented("should return a deep, unattached copy of this parent and its children")
    },
    cloneContent = function() {
      notImplemented("should return a list containing detached clones of this parent's content list")
    }, 
    getContent = function(...) {
      notImplemented("no args should return the full content of this parent as a list", 
      ", with a Filter it should return a list of the content of this parent that matches the supplied filter",
      ", with and index it should return the child at the given index")
    },
    removeContent = function(...) {
      notImplemented("no args should remove all content from this parent and return the detached children", 
      ", with a Filter it should remove all content from this parent that matches the supplied filter and return a list of the detached children",
      ", with an index it should removes and return the child at the given index, or return NA if there's no such child")
    },
    getParent = function() {
      notImplemented("should return return this parent's parent, or NA if this parent is currently not attached to another parent.")
    },
    getDocument = function() {
      notImplemented("should return this parent's owning document or null if the branch containing this parent is currently not attached to a document.")
    },
    addContent = function(...) {
      notImplemented("Should append the content and return the parent. 4 types of arguments:",
      "1. addContent(Content): Appends the content child to the end of the content list.",
      "2. addContent(list):  Appends all children in the given liat to the end of the content list.",
      "3. addContent(index, Content): Inserts the content into the content list at the given index.",
      "4. addContent(index, list): Inserts the content of the list into the content list at the given index.")
    },
    toString = function() {
      notImplemented("Should be overridden in all subclasses")
    }
  )
)