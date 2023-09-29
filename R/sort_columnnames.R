#' Sort column names of a data frame
#'
#' This function sorts the column names of a data frame in alphabetical order.
#'
#' @param data A data frame.
#' @return A data frame with column names sorted in alphabetical order.
#' @export
sort_columnnames <- function(data){
  data <- data[,order(colnames(data))]
  return(data)
}
