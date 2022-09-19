#' Correct model levels
#'
#' Correct level names for modelling and in the use of ROC curve/AUC.
#'
#' @param data String with level names.
#'
#' @return Corrected level names.
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
