#' strict_left_join
#'
#' A wrapper around dplyr's left_join, with an error message if there
#' duplicate values are in the matching fields in y. This will double
#' of rows. See dplyr::left_join .
#'
#' @param x data frame x (left)
#' @param y data frame y (right)
#' @param by unquoted variable names to join.
#' @param ... Pass further arguments to dplyr::left_join
#' @return merged data frame
#' @seealso \code{\link[dplyr:join]{left_join}}
#' @export
strict_left_join <- function(x, y, by = NULL, ...) {
  by <- dplyr::common_by(by, x, y)
  if (any(duplicated(y[by$y]))) {
    stop("Duplicate values in foreign key")
  } else
    return(dplyr::left_join(x, y, by = by, ...))
}
