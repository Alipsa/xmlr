#' @title XML import functions
#' @name xmlConverter
NULL

#' @describeIn xmlConverter create a Document from a character string
#' @param xml an xml character string to parse
#' @return a Document object
#' @export
parse.xmlstring <- function(xml) {
  "parse an xml string into a document"
  doc <- Document$new()
  domBuilder <- DomBuilder$new(doc)
  parser <- Parser$new()
  parser$parse(domBuilder, strsplit(xml, "")[[1]])
  return(doc)
}

#' @describeIn xmlConverter create a Document from a xml file
#' @param fileName the name of the xml file to parse
#' @return a Document object
#' @export
parse.xmlfile <- function(fileName) {
  # TODO: consider parsing directly from the file stream instead of loading up the content to a string first
  #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/readChar
  # or line by line;
  # conn <- file(fileName,open="r")
  # linn <-readLines(conn)
  # for (i in 1:length(linn)){
  #   print(linn[i])
  # }
  # close(conn)
  if (!file.exists(fileName)) {
    stop(paste("File", fileName, "does not exist"))
  }
  parse.xmlstring(readChar(fileName, file.info(fileName)$size))
}

#' @describeIn xmlConverter create a data frame from a xmlr Element
#' @description this is a convenience method to take all the children of the given Element
#' and create a data frame based on the content of each child where each child constitutes a row
#' and the attributes or elements (including text) will constiture the columns.
#' It assumes a homeogenous structure and the column names are takes from the first child
#' @param element the element to convert
#' @return a data frame
#' @export
xmlrToDataFrame <- function(element) {
  if (!isRc(element, "Element")) {
    stop(paste("element argument is not an Element Reference Class:", class(element)))
  }
  xmldf <- NULL
  for (child in element$getChildren()) {
    row <- list()
    for (att in child$getAttributes()) {
      row[[att$getName()]] <- att$getValue()
    }
    if (child$hasText()) {
      row[[child$getName()]] <- child$getText()
    }
    for (subchild in child$getChildren()) {
      for (att in subchild$getAttributes()) {
        row[[att$getName()]] <- att$getValue()
      }
      if (subchild$hasText()) {
        row[[subchild$getName()]] <- subchild$getText()
      }
    }
    if (is.null(xmldf)) {
      xmldf <- data.frame(row, stringsAsFactors = FALSE)
    } else {
      xmldf <- rbind(xmldf, row)
    }
  }
  xmldf
}



