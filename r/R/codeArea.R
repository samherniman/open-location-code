#' codeArea object constructor
#'
#' @param code Open Location Code. Character
#' @param codeLength The number of significant digits in the output code, not including any separator characters.
#' @param polygon_sf sf polygon representing the bounding box of the Open Location Code
#'
#' @return codeArea object
#' @export
new_code_area <- function(code = character(), codeLength, polygon_sf) {
  structure(
    list(
      code = code,
      codeLength = codeLength,
      latitudeCenter = latlng(polygon_sf)[1],
      longitudeCenter = latlng(polygon_sf)[2],
      polygon_sf = polygon_sf
    ),
    class = "code_area"
  )
}

#' codeArea print method
#'
#' @param x codeArea object
#' @param ... Not used
#'
#' @return Nothing
#' @export
print.code_area <- function(x, ...) {
  print(x$code)
  print(x$polygon_sf)
  invisible(x)
}

#' Find the centroid of a codeArea
#'
#' @param x codeArea object
#' @param ... Not used
#'
#' @return codeArea centroid in latitude longitude
#' @export
latlng <- function(x, ...) {
  UseMethod("latlng")
}

#' Find the centroid of a codeArea
#'
#' @param x codeArea object
#' @param ... Not used
#'
#' @return codeArea centroid in latitude longitude
#' @export
latlng.default <- function(x, ...) {
  centroid(x) |>
    rev()
}

#' Find the centroid of a codeArea
#'
#' @param x codeArea object
#' @param ... Not used
#'
#' @return codeArea centroid in latitude longitude
#' @export
latlng.code_area <- function(x, ...) {
  latlng.default(x$polygon_sf)
}

#' Centroid function
#'
#' @param x sf polygon
#' @param ... Not used
#'
#' @return Coordinates of the centroid of a polygon
#' @export
centroid <- function(x, ...) {
  sf::st_centroid(x) |>
    sf::st_coordinates()
}
