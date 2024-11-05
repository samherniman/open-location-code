#' Determine if a code is valid
#'
#' @param code Open Location Code. Character
#' @param seperator_ Regular expression denoting the separator character. Usually '\\+'
#' @param padding_character_ Regular expression denoting the padding character. Usually '0'
#' @param seperator_position_ Expected location of the separator character
#' @param code_alphabet_ The character set used to encode the values
#'
#' @details
#' To be valid, all characters must be from the Open Location Code character set with at most one separator. The separator can be in any even-numbered position up to the eighth digit.
#'
#'
#' @return Logical. TRUE if the code is a valid Open Location Code. FALSE if not
#' @export
#'
#' @examples
#' isValid("8FVC9GF6+6X7")
#' isValid("I am not an OLC")
isValid <- function(code, seperator_ = "\\+", padding_character_ = "0", seperator_position_ = 9, code_alphabet_ = "23456789CFGHJMPQRVWX") {
  # Determines if a code is valid.


  # The separator is required.
  if (!stringr::str_detect(code, seperator_)) {
    return(FALSE)
  }
  # Is it the only character?
  if (nchar(code) == 1) {
    return(FALSE)
  }
  # Is it in an illegal position?
  sep <- stringr::str_locate(code, seperator_)[1]
  if (sep > seperator_position_ || (sep - 1) %% 2 == 1) {
    return(FALSE)
  }
  # We can have an even number of padding characters before the separator,
  # but then it must be the final character.
  pad <- stringr::str_locate(code, padding_character_)[1]

  if (!is.na(pad)) {
    # Short codes cannot have padding
    if (sep < seperator_position_) {
      return(FALSE)
    }
    # Not allowed to start with them!
    if (pad == 1) {
      return(FALSE)
    }

    # There can only be one group and it must have even length.
    pads <- stringr::str_extract_all(code, "0+", simplify = TRUE)

    if (length(pads) > 1 || nchar(pads) %% 2 == 1) {
      return(FALSE)
    }
    # If the code is long enough to end with a separator, make sure it does.
    if (!stringr::str_ends(code, pattern = seperator_)) {
      return(FALSE)
    }
  }
  # If there are characters after the separator, make sure there isn't just
  # one of them (not legal).
  if (nchar(code) - sep == 1) {
    return(FALSE)
  }
  sepPad <- paste0("[^", code_alphabet_, padding_character_, seperator_, "]")
  # Check the code contains only valid characters.
  return(stringr::str_detect(toupper(code), sepPad, negate = TRUE))
}
