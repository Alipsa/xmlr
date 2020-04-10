#' A general purpose linked stack
#'

#' @export
Stack <- setRefClass(
  Class="Stack",
  fields = list(
    #' @field size the size of the stack (number of elements in the stack)
    m_size = "integer",
    #' @field stackNode an envronment containing the current element and the one under
    m_stackNode = "ANY"
  ),
  methods = list(

    initialize = function (...) {
      m_size <<- 0L
    },

    isEmpty = function() {
      # printp("Stack size is", size)
      if (m_size == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },

    createEmptyEnvironment = function() {
      emptyenv()
    },

    createNode = function(val, nextNode = NULL) {
      node <- new.env(parent=createEmptyEnvironment())
      node$element <- val
      node$nextNode <- nextNode
      node
    },

    push = function(val) {
      "Add an element to the top of the stack"
      if(isEmpty()) {
        m_stackNode <<- createNode(val)
      } else {
        m_stackNode <<- createNode(val, m_stackNode)
      }
      m_size <<- (m_size + 1L)
    },

    pop = function() {
      "Pull the top element from the stack removing it from the stack"
      if(!isEmpty()) {
        currentNode <- m_stackNode$element
        m_stackNode <<- m_stackNode$nextNode
        m_size <<- (m_size - 1L)
        return(currentNode)
      }
    },

    peek = function() {
      "Get the top element from the stack without changing it"
      if(!isEmpty()) {
        return(m_stackNode$element)
      }
    },

    size = function() {
      "Get the current size of the stack"
      m_size
    }
  )
)