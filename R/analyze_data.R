#' Analyze Data
#'
#' This function analyzes the given data and returns various statistics.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param print_results Logical value indicating whether to print the results. Default is TRUE.
#'
#' @return A data frame with the analyzed statistics.
#'
#'
analyze_data <- function(data, print_results = TRUE) {

  # Quantity of zeros
  quantity_of_zeros <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(. == 0, na.rm = TRUE))) %>%
    unlist()

  # Percentage of zeros
  percentage_of_zeros <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~round(100 * sum(. == 0, na.rm = TRUE) / nrow(data), 2))) %>%
    unlist()

  # Quantity of NA's
  quantity_of_nas <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(is.na(.)))) %>%
    unlist()

  # Percentage of NA's
  percentage_of_nas <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~round(100 * sum(is.na(.)) / nrow(data), 2))) %>%
    unlist()

  # Quantity of infinite values
  quantity_of_inf <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(is.infinite(.)))) %>%
    unlist()

  # Percentage of infinite values
  percentage_of_nas <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~round(100 * sum(is.infinite(.)) / nrow(data), 2))) %>%
    unlist()


  # Apply the custom function to each column
  column_types <- sapply(data, get_type_v)

  # Number of unique values
  quantity_unique <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(!is.na(unique(.))))) %>%
    unlist()

  sd <-  sapply(data, function(x) ifelse(is.numeric(x),round(stats::sd(x,na.rm = TRUE),2),NA)) # SD
  var <-  sapply(data, function(x) ifelse(is.numeric(x),round(stats::var(x,na.rm = TRUE),2),NA)) # SD
  q1 <-  sapply(data, function(x) ifelse(is.numeric(x),round(stats::quantile(x, 0.25,na.rm = TRUE),2),NA)) # Q1
  median <-  sapply(data, function(x) ifelse(is.numeric(x),round(stats::median(x,na.rm = TRUE),2),NA)) # Median
  q3 <-  sapply(data, function(x) ifelse(is.numeric(x),round(stats::quantile(x,0.75,na.rm = TRUE),2),NA)) # Q3
  iqr <-  sapply(data, function(x) ifelse(is.numeric(x), round(stats::IQR(x,0.75,na.rm = TRUE),2),NA))# IQR
  min <-  sapply(data, function(x) ifelse(is.numeric(x),round(min(x,na.rm = TRUE),2),NA)) # Min
  max <-  sapply(data, function(x) ifelse(is.numeric(x),round(max(x,na.rm = TRUE),2),NA)) # Max
  mean <-  sapply(data, function(x) ifelse(is.numeric(x),round(mean(x,na.rm = TRUE),2),NA)) # Mean



  # Create a data frame with the analyzed statistics
  df_inspect_res <- data.frame(
    variable = names(data),
    quantity_of_zeros,
    percentage_of_zeros,
    quantity_of_nas,
    percentage_of_nas,
    quantity_of_inf,
    percentage_of_nas,
    column_types,
    quantity_unique,
    sd,
    var,
    q1,
    median,
    q3,
    iqr,
    min,
    max,
    mean
  )


  # Print or return results
  if (print_results) {
    print(df_inspect_res)
  } else {
    return(df_inspect_res)
  }
}



# Define the custom function
get_type_v <- function(x) {
  posix <- ifelse(lubridate::is.POSIXct(x), "POSIXct", "")
  posix <- ifelse(lubridate::is.POSIXlt(x), paste(posix, "POSIXlt", sep = "/"), posix)
  posix <- ifelse(lubridate::is.POSIXt(x), paste(posix, "POSIXt", sep = "/"), posix)

  if (posix == "") {
    cl <- class(x)
    return(ifelse(length(cl) > 1, paste(cl, collapse = "-"), cl))
  } else {
    return(posix)
  }
}
