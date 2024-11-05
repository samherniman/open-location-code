#' Recover the nearest matching code to a specified location
#'
#' @param code Open Location Code. Character
#' @param referenceLatitude The latitude (in signed decimal degrees) to use to find the nearest matching full code
#' @param referenceLongitude The longitude (in signed decimal degrees) to use to find the nearest matching full code
#' @param seperator_ Regular expression denoting the separator character. Usually '\\+'
#' @param seperator_position_ Expected location of the separator character
#' @param latitude_max_ The maximum value for latitude in degrees
#'
#' @return The nearest full Open Location Code to the reference location that matches the short code. If the passed code was not a valid short code, but was a valid full code, it is returned with proper capitalization but otherwise unchanged.
#' @export
#'
#' @examples
#' recoverNearest("9G8F+6X", 47.4, 8.6)
#' recoverNearest("8F+6X", 47.4, 8.6)
recoverNearest <- function(code, referenceLatitude, referenceLongitude, seperator_ = "\\+", seperator_position_ = 9, latitude_max_ = 90) {
  # if code is a valid full code, return it properly capitalized
  if (isFull(code)) {
    return(stringr::str_to_upper(code))
  }
  if (!isShort(code)) {
    cli::cli_abort(c(
      "Error while recovering the nearest matching code:",
      "Passed short code is not valid - {code}"
    ))
  }
  # Ensure that latitude and longitude are valid.
  referenceLatitude <- clipLatitude(referenceLatitude)
  referenceLongitude <- normalizeLongitude(referenceLongitude)
  # Clean up the passed code.
  code <- stringr::str_to_upper(code)
  # Compute the number of digits we need to recover.
  paddingLength <- seperator_position_ - stringr::str_locate(code, seperator_)[1]
  # The resolution (height and width) of the padded area in degrees.
  resolution <- 20^(2 - (paddingLength / 2))
  # Distance from the center to an edge (in degrees).
  halfResolution <- resolution / 2
  # Use the reference location to pad the supplied short code and decode it.
  codeArea <- decode(
    encode(
      latitude = referenceLatitude,
      longitude = referenceLongitude
    ) |>
      stringr::str_sub(1, paddingLength) |>
      paste0(code)
  )

  # How many degrees latitude is the code from the reference? If it is more
  # than half the resolution, we need to move it north or south but keep it
  # within -90 to 90 degrees.
  if (referenceLatitude + halfResolution < codeArea$latitudeCenter &&
    codeArea$latitudeCenter - resolution >= -latitude_max_) {
    # If the proposed code is more than half a cell north of the reference location,
    # it's too far, and the best match will be one cell south.
    codeArea$latitudeCenter <- codeArea$latitudeCenter - resolution
  } else if (referenceLatitude - halfResolution > codeArea$latitudeCenter &&
    codeArea$latitudeCenter + resolution <= latitude_max_) {
    # If the proposed code is more than half a cell south of the reference location,
    # it's too far, and the best match will be one cell north.
    codeArea$latitudeCenter <- codeArea$latitudeCenter + resolution
  }
  # Adjust longitude if necessary.
  if (referenceLongitude + halfResolution < codeArea$longitudeCenter) {
    codeArea$longitudeCenter <- codeArea$longitudeCenter - resolution
  } else if (referenceLongitude - halfResolution > codeArea$longitudeCenter) {
    codeArea$longitudeCenter <- codeArea$longitudeCenter + resolution
  }

  return(
    encode(
      latitude = codeArea$latitudeCenter,
      longitude = codeArea$longitudeCenter,
      codeLength = codeArea$codeLength
    )
  )
}
