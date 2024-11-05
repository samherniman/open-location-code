#' Compute the latitude precision value for a given code length
#'
#' @param codeLength Numeric representing the length of the given open location code
#' @param grid_rows_ Number of rows in the grid refinement method
#'
#' @description
#' Compute the latitude precision value for a given code length. Lengths <= 10 have the same precision for latitude and longitude, but lengths > 10 have different precisions due to the grid method having fewer columns than rows.
#'
#'
#' @return Numeric latitude precision
#' @export
#'
#' @examples
#' computeLatitudePrecision(8)
#' computeLatitudePrecision(10)
#' computeLatitudePrecision(11)
#' computeLatitudePrecision(13)
#' computeLatitudePrecision(14)
computeLatitudePrecision <- function(codeLength, grid_rows_ = 5) {
  if (codeLength <= 10) {
    return(20^floor((codeLength / -2) + 2))
  }
  return((20^-3) / (grid_rows_^(codeLength - 10)))
}
