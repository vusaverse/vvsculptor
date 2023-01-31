#' Correct model levels
#'
#' Correct level names for modelling and in the use of ROC curve/AUC.
#'
#' @param data String with level names.
#'
#' @return Corrected level names.
#' @examples
#'   data <- data.frame(id = c(1,2,3),
#'                  name = c("Alice","Bob","Charlie"),
#'                  gender = factor(c("Female","Male","Female"), levels = c("Female","Male")))
#'
#'   correct_model_levels(data)
#'   # returns a data frame with factor levels of the variable gender corrected to "Female" and "Male"
#'
#'   data <- data.frame(id = c(1,2,3),
#'                  name = c("Alice","Bob","Charlie"),
#'                  gender = factor(c("Female","Male","Female")))
#'   correct_model_levels(data)
#'   # returns a data frame with factor levels of the variable gender corrected to "F" and "M"

#' @export
correct_model_levels <- function(data) {
    feature.names <- names(data)

    for (f in feature.names) {
        if (inherits(data[[f]], "factor")) {
            levels <- unique(c(data[[f]]))
            data[[f]] <- factor(data[[f]],
                                labels=make.names(levels))
        }
    }

    return(data)
}
