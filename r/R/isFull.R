#' Determine if a code is a valid full Open Location Code
#'
#' @param code Open Location Code. Character
#' @param code_alphabet_ The character set used to encode the values
#' @param encoding_base_ The base to use to convert numbers to/from
#' @param latitude_max_ The maximum value for latitude in degrees
#' @param longitude_max_ The maximum value for longitude in degrees
#'
#' @details
#' Not all possible combinations of Open Location Code characters decode to valid latitude and longitude values. This checks that a code is valid and also that the latitude and longitude values are legal. If the prefix character is present, it must be the first character. If the separator character is present, it must be after four characters.
#'
#'
#' @return Logical. TRUE if a code is a valid full Open Location Code. FALSE if not.
#' @export
#'
#' @examples
#' isFull("8FVC9G8F+6X")
#' isFull("8F+6X")
#' isFull("9G8F+6X")
#' isFull("G8F+6X")
isFull <- function(code, code_alphabet_ = "23456789CFGHJMPQRVWX", encoding_base_ = nchar(code_alphabet_), latitude_max_ = 90, longitude_max_ = 180) {
  if (!isValid(code)) {
    return(FALSE)
  }

  # If it's short, it's not full
  if (isShort(code)) {
    return(FALSE)
  }

  # Work out what the first latitude character indicates for latitude.
  firstLatValue <- code |>
    stringr::str_sub(1, 1) |>
    stringr::str_to_upper()

  firstLatValue <- stringr::str_locate(code_alphabet_, firstLatValue)[1] * encoding_base_

  if (firstLatValue >= latitude_max_ * 2) {
    # The code would decode to a latitude of >= 90 degrees.
    return(FALSE)
  }
  if (nchar(code) > 1) {
    # Work out what the first longitude character indicates for longitude.
    firstLngValue <- code |>
      stringr::str_sub(2, 2) |>
      stringr::str_to_upper()

    firstLngValue <- stringr::str_locate(code_alphabet_, firstLngValue)[1] * encoding_base_
  }
  if (firstLngValue >= longitude_max_ * 2) {
    # The code would decode to a longitude of >= 180 degrees.
    return(FALSE)
  }
  return(TRUE)
}
