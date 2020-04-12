#' @title XML import functions
#' @name xmlImporter
NULL

#' @describeIn xmlImporter create a Document from a character string
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

#' @describeIn xmlImporter create a Document from a xml file
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