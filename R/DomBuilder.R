#' Create a xmlr object tree based on parsing events
DomBuilder <- setRefClass(
  Class = "DomBuilder",
  fields = c(
    "stack",
    "doc",
    "root"
  ),
  methods = list(

    initialize = function(document) {
      doc <<- document
    },

    startDocument = function() {
      "Event signalling parsing has begun"
      #print("Start document")
      stack <<- Stack$new()
    },

    endDocument = function() {
      "Event signalling parsing has completed"
      #print("End document")
      doc$setRootElement(root)
      stack <<- NULL
      #printp("Parsing complete!")
    },

    startElement = function(name, attributes) {
      "start element event; @param name the element name, @param attributes a named list of attributes"
      #printp("Start element", name)
      e <- Element$new(name)
      #print(str(attributes))
      e$setAttributes(attributes)
      stack$push(e)
    },

    endElement = function(name) {
      "end element event; @param name the element name"
      #printp("End element", name)
      current <- stack$peek()
      if (!stack$isEmpty()) {
        e <- stack$pop()
        parent <- stack$peek()
        if (!is.null(parent)) {
          parent$addContent(e)
        } else {
          root <<- e
        }
      }
    },

    text = function(text) {
      "text event; @param text the character content of the Text node"
      if (nchar(trimws(text)) > 0) {
        #printp0("Add text '", text, "'")
        stack$peek()$setText(text)
      }
    }
  )
)