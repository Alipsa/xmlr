#' @title Create a data frame from a xmlr Element
#' @description This is a convenience method to take all the children of the given Element
#' and create a data frame based on the content of each child where each child constitutes a row
#' and the attributes or elements (including text) will constiture the columns.
#' It assumes a homeogenous structure and the column names are takes from the first child
#' @return a data frame
#' @param element the element to convert

#' @export
xmlrToDataFrame <- function(element) {
  if (!isRc(element, "Element")) {
    stop(paste("element argument is not an Element Reference Class:", class(element)))
  }
  xmldf <- NULL
  for (child in element$getChildren()) {
    row <- list()
    if (child$hasAttributes()) {
      row <- append(row, child$getAttributes())
    }
    if (child$hasText()) {
      row[[child$getName()]] <- child$getText()
    }
    for (subchild in child$getChildren()) {
      if (subchild$hasAttributes()) {
        row <- append(row, subchild$getAttributes())
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



