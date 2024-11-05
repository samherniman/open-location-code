#' Normalize a longitude into the range -180 to 180, not including 180.
#'
#' @param longitude Numeric longitude
#'
#' @return Numeric longitude that is between -180 and 180
#' @export
#'
#' @examples
#' normalizeLongitude(-200)
#' normalizeLongitude(-181)
#' normalizeLongitude(-180)
#' normalizeLongitude(-179)
#' normalizeLongitude(179)
#' normalizeLongitude(180)
#' normalizeLongitude(181)
normalizeLongitude <- function(longitude) {
  while (longitude < -180) {
    longitude <- longitude + 360
  }
  while (longitude >= 180) {
    longitude <- longitude - 360
  }
  return(longitude)
}
