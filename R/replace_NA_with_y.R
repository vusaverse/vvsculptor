#' Replace NA with y
#'
#' Een functie om missende waarden van x op te vullen met de waarde van y voor
#' diezelfde rij. Deze functie werkt zoals coalesce, maar hier kan een data frame
#' als imput gegeven worden.
#'
#' @param data a data frame
#' @param x a (quoted) variable name with missing values.
#' @param y a (quoted) variable name with values that can be imputed in x.
#' @family vector calculations
#' @family missing data functies
#' @return the dataframe with the imputed x variable. The y variable is removed from the output.
#' @export
replace_NA_with_y <- function(data, x, y) {
    ## Maak een lijst van de NA's
    nas <- is.na(data[[x]])

    ## Vervang de NA's
    data[nas, x] <- data[nas, y]
    ## Verwijder de y-variabele

    data[, y] <- NULL

    return(data)
}
