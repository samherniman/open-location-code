#' Clip a latitude into the range -90 to 90
#'
#' @param latitude A latitude in signed decimal degrees.
#'
#' @return A latitude in signed decimal degrees between -90 and 90
#' @export
#'
#' @examples
#' clipLatitude(-220.3)
#' clipLatitude(-90.3)
#' clipLatitude(-90)
#' clipLatitude(-89.9)
#' clipLatitude(-22.370475875)
#' clipLatitude(0)
#' clipLatitude(24.3)
#' clipLatitude(89.9)
#' clipLatitude(90)
#' clipLatitude(221.3)
clipLatitude <- function(latitude) {
  return(min(90, max(-90, latitude)))
}
