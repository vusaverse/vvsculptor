#' strict_left_join
#'
#' A wrapper around dplyr's left_join, with an error message if
#' duplicate values are present in the matching fields in y.
#' This will prevent duplicating rows. See dplyr::left_join .
#'
#' @param x data frame x (left)
#' @param y data frame y (right)
#' @param by unquoted variable names to join.
#' @param ... Pass further arguments to dplyr::left_join
#' @return merged data frame
#' @seealso \code{\link[dplyr:join]{left_join}}
#' @examples
#'   left_df <- data.frame(id = c(1, 2, 3), name = c("Alice", "Bob", "Charlie"))
#'   right_df <- data.frame(id = c(1, 2, 4), age = c(20, 25, 30))
#'   strict_left_join(left_df, right_df, by = "id")
#'
#' @export
strict_left_join <- function(x, y, by = NULL, ...) {
  by <- dplyr::common_by(by, x, y)
  if (any(duplicated(y[by$y]))) {
    stop("Duplicate values in foreign key")
  } else
    return(dplyr::left_join(x, y, by = by, ...))
}
