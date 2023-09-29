#' Replace NA values in a data frame column with values from another column
#'
#' This function replaces NA values in a specified column of a data frame
#' with corresponding values from another specified column. The function works
#' like the 'coalesce' function in SQL, but it is designed to handle data frames.
#'
#' @param data A data frame.
#' @param var_with_NA A variable name within the data frame that contains missing values.
#' @param var_to_fill_NA A variable name within the data frame that contains values
#' to replace the missing values in var_with_NA.
#' @return A data frame with the NA values in the var_with_NA variable replaced by the
#' corresponding values in the var_to_fill_NA variable. The var_to_fill_NA variable is removed from the output.
#' @export
replace_NA_with_y <- function(data, var_with_NA, var_to_fill_NA) {
  ## Create a list of the NA's
  nas <- is.na(data[[var_with_NA]])

  ## Replace the NA's
  data[nas, var_with_NA] <- data[nas, var_to_fill_NA]
  ## Remove the var_to_fill_NA variable
  data[, var_to_fill_NA] <- NULL

  return(data)
}
