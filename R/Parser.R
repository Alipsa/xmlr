#' Parse an xml string and create sax like events
#'
#' @description an XML parser based on an article on creating a quick and dirty xml parser by Steven Brandt
Parser <- setRefClass(
  Class = "Parser",
  fields = c(
    "TEXT",
    "ENTITY",
    "OPEN_TAG",
    "CLOSE_TAG",
    "START_TAG",
    "ATTRIBUTE_LVALUE",
    "ATTRIBUTE_EQUAL",
    "ATTRIBUTE_RVALUE",
    "QUOTE",
    "IN_TAG",
    "SINGLE_TAG",
    "COMMENT",
    "DONE",
    "DOCTYPE",
    "PRE",
    "CDATA"
  ),
  methods = list(

    initialize = function() {
      TEXT <<- 1
      ENTITY <<- 2
      OPEN_TAG <<- 3
      CLOSE_TAG <<- 4
      START_TAG <<- 5
      ATTRIBUTE_LVALUE <<- 6
      ATTRIBUTE_EQUAL <<- 9
      ATTRIBUTE_RVALUE <<- 10
      QUOTE <<- 7
      IN_TAG <<- 8
      SINGLE_TAG <<- 12
      COMMENT <<- 13
      DONE <<- 11
      DOCTYPE <<- 14
      PRE <<- 15
      CDATA <<- 16
    },

    parse = function(db, charVector) {
      mode <- PRE
      st <- Stack$new()
      sb <- ""
      etag <- ""

      depth <- 0
      tagName <- NULL
      lvalue <- NULL
      rvalue <- NULL

      line <- 1
      col <- 0
      eol <- FALSE
      attrs <- list()
      quotec <- '"'

      popMode <- function() {
        if (st$isEmpty()) {
          return(PRE)
        }
        st$pop()
      }

      exc <- function(msg, line, col) {
        stop(paste(msg, "near line", line, ", column ", col))
      }

      db$startDocument()

      for (c in charVector) {
        #printp0("  looking at: '", c, "'")

        # We need to map \r, \r\n, and \n to \n See XML spec section 2.11
        if (c == '\n' & eol) {
          eol <- FALSE
          next
        } else if (eol) {
          eol <- FALSE
        } else if (c == '\n') {
          line <- line + 1
          col <- 0
        } else if (c == '\r') {
          eol <- TRUE
          c <- '\n'
          line <- line + 1
          col <- 0
        } else {
          col <- col + 1
        }

        if (mode == DONE) {
          db$endDocument()
          return()
        } else if (mode == TEXT) {
          # We are between tags collecting text.
          if (c == '<') {
            st$push(mode)
            mode <- START_TAG
            if (length(db) > 0) {
              db$text(sb)
              sb <- ""
            }
          } else if (c == '&') {
            st$push(mode)
            mode <- ENTITY
            etag <- ""
          } else {
            sb <- paste0(sb, c)
          }
        } else if (mode == CLOSE_TAG) {
          # we are processing a closing tag: e.g. </foo>
          if (c == '>') {
            mode <- popMode()
            tagName <- sb
            sb <- ""
            depth <- depth -1
            if (depth == 0) mode <- DONE
            db$endElement(tagName)
          } else {
            sb <- paste0(sb, c)
          }
        } else if (mode == CDATA) {
          # we are processing CDATA
          if (c == '>' & endsWith(sb, "]]")) {
            db$text(substr(sb, 1, nchar(sb)-2))
            sb <- ""
            mode <- popMode()
          } else
          sb <- paste0(sb, c)
        } else if (mode == COMMENT) {
          # we are processing a comment.  We are inside the <!-- .... --> looking for the -->.
          if (c == '>' & endsWith(sb, "--")) {
            sb <- ""
            mode <- popMode()
          } else
          sb <- paste0(sb,c)
        } else if (mode == PRE) {
          # We are outside the root tag element
          if (c == '<') {
            mode <- TEXT
            st$push(mode)
            mode <- START_TAG
          }
        } else if (mode == DOCTYPE) {
          # We are inside one of these <? ... ?>
          # or one of these <!DOCTYPE ... >
          if (c == '>') {
            mode <- popMode()
            if (mode == TEXT) mode <- PRE
          }
        } else if (mode == START_TAG) {
          # we have just seen a < and are wondering what we are looking at
          # <foo>, </foo>, <!-- ... --->, etc.
          mode <- popMode()
          if (c == '/') {
            st$push(mode)
            mode <- CLOSE_TAG
          } else if (c == '?') {
            mode <- DOCTYPE
          } else {
            st$push(mode)
            mode <- OPEN_TAG
            tagName <- NULL
            attrs <- list()
            sb <- paste0(sb, c)
          }
        } else if (mode == ENTITY) {
          # we are processing an entity, e.g. &lt;, &#187;, etc.
          if (c == ';') {
            mode <- popMode()
            cent <- etag
            etag <- ""
            if (cent == "lt")
              sb <- paste0(sb,'<')
            else if (cent == "gt")
              sb <- paste0(sb,'>')
            else if (cent == "amp")
              sb <- paste0(sb, '&')
            else if (cent == "quot")
              sb <- paste0(sb,'"')
            else if (cent == "apos")
              sb <- paste0(sb, "'")
            # Could parse hex entities if we wanted to
            # else if(cent.startsWith("#x"))
            # sb.append((char)Integer.parseInt(cent.substring(2),16))
            else if (startsWith(cent,"#"))
              sb <- paste(sb, substr(cent, 2, nchar(cent)))
            # Insert custom entity definitions here
            else
              exc(paste0("Unknown entity: &", cent, ";"), line, col)
          } else {
            etag <- paste0(etag, c)
          }
        } else if (mode == SINGLE_TAG) {
          # we have just seen something like this:
          # <foo a="b"/
          # and are looking for the final >.
          if (is.null(tagName))
            tagName <- sb
          if (c != '>')
            exc(paste0("Expected > for tag: <" + tagName + "/>"), line, col)

          db$startElement(tagName, attrs)
          db$endElement(tagName)
          if (depth == 0) {
            db$endDocument()
            return()
          }
          sb <- ""
          attrs <- list()
          tagName <- NULL
          mode <- popMode()
        } else if (mode == OPEN_TAG) {
          # we are processing something like this <foo ... >.
          # It could still be a <!-- ... --> or something.
          if (c == '>') {
            if (is.null(tagName)) {
              tagName <- sb
            }
            sb <- ""
            depth <- depth +1
            db$startElement(tagName, attrs)
            tagName <- NULL
            attrs <- list()
            mode <- popMode()
          } else if (c == '/') {
            mode <- SINGLE_TAG
          } else if (c == '-' & sb == "!-") {
            mode <- COMMENT
          } else if (c == '[' & sb == "![CDATA") {
            mode <- CDATA
            sb <- ""
          } else if (c == 'E' & sb == "!DOCTYP") {
            sb <- ""
            mode <- DOCTYPE
          } else if (isWhiteSpaceChar(c)) {
            tagName <- sb
            sb <- ""
            mode <- IN_TAG
          } else {
            sb <- paste0(sb, c)
          }
        } else if (mode == QUOTE) {
          # We are processing the quoted right-hand side of an element's attribute.
          if (c == quotec) {
            rvalue <- sb
            sb <- ""
            attrs[[lvalue]] <- rvalue
            mode <- IN_TAG
            # See section the XML spec, section 3.3.3 on normalization processing.
          } else if (c %in% " \r\n\u0009") {
            sb <- paste0(sb,' ')
          } else if (c == '&') {
            st$push(mode)
            mode <- ENTITY
            etag <- ""
          } else {
            sb <- paste0(sb,c)
          }
        } else if (mode == ATTRIBUTE_RVALUE) {
          if (c == '"' | c == "'") {
            quotec <- c
            mode <- QUOTE
          } else if (isWhiteSpaceChar(c)) {
            # do nothing
          } else {
            exc("Error in attribute processing", line, col)
          }
        } else if (mode == ATTRIBUTE_LVALUE) {
          if (isWhiteSpaceChar(c)) {
            lvalue <- sb
            sb <- ""
            mode <- ATTRIBUTE_EQUAL
          } else if (c == '=') {
            lvalue <- sb
            sb <- ""
            mode <- ATTRIBUTE_RVALUE
          } else {
            sb <- paste0(sb, c)
          }
        } else if (mode == ATTRIBUTE_EQUAL) {
          if (c == '=') {
            mode <- ATTRIBUTE_RVALUE
          } else if (isWhiteSpaceChar(c)) {
          # do nothing
          } else {
            exc("Error in attribute processing.", line, col)
          }
        } else if (mode == IN_TAG) {
          if (c == '>') {
            mode <- popMode()
            db$startElement(tagName, attrs)
            depth <- depth +1
            tagName <- NULL
            attrs <- list()
          } else if (c == '/') {
            mode <- SINGLE_TAG
          } else if (isWhiteSpaceChar(c)) {
            #do nothing
          } else {
            mode <- ATTRIBUTE_LVALUE
            sb <- paste0(sb, c)
          }
        }
      } # for loop
      if (mode == DONE)
        db$endDocument()
      else
        exc("missing end tag", line, col)
    }
  )
)