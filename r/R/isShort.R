#' Determine if a code is a valid short Open Location Code
#'
#' @param code Open Location Code. Character
#' @param seperator_ Regular expression denoting the separator character. Usually '\\+'
#' @param seperator_position_ Expected location of the separator character
#'
#' @details
#' A short Open Location Code is a sequence created by removing four or more digits from an Open Location Code. It must include a separator character.
#'
#'
#' @return Logical. TRUE if code is a valid short Open Location Code. FALSE if not
#' @export
#'
#' @examples
#' isShort("8FVC9GF6+6X7") # FALSE
#' isShort("9GF6+6X7") # TRUE
#' isShort("GF6+6X7") # FALSE
isShort <- function(code, seperator_ = "\\+", seperator_position_ = 9) {
  # Determines if a code is a valid short code.
  # A short Open Location Code is a sequence created by removing four or more
  # digits from an Open Location Code. It must include a separator
  # character.

  # Check it's valid.
  if (!isValid(code)) {
    return(FALSE)
  }

  # If there are fewer characters than expected before the SEPERATOR.
  sep <- stringr::str_locate(code, seperator_)[1]
  if (sep >= 1 && sep < seperator_position_) {
    return(TRUE)
  }
  return(FALSE)
}
