#' Remove characters from the start of an OLC code
#'
#' @param code Open Location Code. Character
#' @param latitude A latitude in signed decimal degrees. Will be clipped to the range -90 to 90
#' @param longitude A longitude in signed decimal degrees. Will be normalised to the range -180 to 180
#' @param padding_character_ Regular expression denoting the padding character. Usually '0'
#' @param min_trimmable_code_length_ Minimum length of a code that can be shortened
#' @param pair_resolutions_ The resolution values in degrees for each position in the lat/lng pair encoding. These give the place value of each position, and therefore the dimensions of the resulting area
#'
#' @description
#' This uses a reference location to determine how many initial characters can be removed from the OLC code. The number of characters that can be removed depends on the distance between the code center and the reference location. The minimum number of characters that will be removed is four. If more than four characters can be removed, the additional characters will be replaced with the padding character. At most eight characters will be removed. The reference location must be within 50% of the maximum range. This ensures that the shortened code will be able to be recovered using slightly different locations.
#'
#'
#' @return Either the original code, if the reference location was not close enough, or the trimmed code.
#' @export
#'
#' @examples
#' shorten('8FVC9G8F+6X', 47.5, 8.5)
shorten <- function(code, latitude, longitude, padding_character_ = "0", min_trimmable_code_length_ = 6, pair_resolutions_ = c(20.0, 1.0, .05, .0025, .000125)) {
  if (!isFull(code)) {
    cli::cli_abort(c(
      "Error while shortening Open Location Code:",
      "Passed Open Location Code is not a valid full code - {code}"
    ))
  }
  if (stringr::str_detect(code, padding_character_, negate = FALSE)) {
    cli::cli_abort(c(
      "Error while shortening Open Location Code:",
      "Cannot shorten padded codes: {code}",
      "Supplied code contains the padding character: {padding_character_}"
    ))
  }
  code <- stringr::str_to_upper(code)
  codeArea <- decode(code)

  if (codeArea$codeLength < min_trimmable_code_length_) {
    cli::cli_abort(c(
      "Error while shortening Open Location Code:",
      "Code length much be at least: {min_trimmable_code_length_}"
    ))
  }

  # Ensure that latitude and longitude are valid.
  latitude <- clipLatitude(latitude)
  longitude <- normalizeLongitude(longitude)

  # How close are the latitude and longitude to the code center.
  coderange <- max(
    abs(codeArea$latitudeCenter - latitude),
    abs(codeArea$longitudeCenter - longitude)
  )

  for (i in seq(length(pair_resolutions_) - 2, 1, -1)) {
    # Check if we're close enough to shorten. The range must be less than 1/2
    # the resolution to shorten at all, and we want to allow some safety, so
    # use 0.3 instead of 0.5 as a multiplier.
    if (coderange < (pair_resolutions_[i + 1] * 0.3)) {
      # Trim it.
      return(stringr::str_sub(code, ((i + 1) * 2) + 1, -1L))
    }
  }

  return(code)
}
