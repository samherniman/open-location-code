#' Construct a codeArea object
#'
#' @param code Open Location Code. Character
#' @param lat A latitude in signed decimal degrees. Will be normalised to the range -180 to 180.
#' @param lng A longitude in signed decimal degrees. Will be normalised to the range -180 to 180.
#' @param latPrecision Multiply latitude by this much to make it a multiple of the finest precision
#' @param lngPrecision Multiply longitude by this much to make it a multiple of the finest precision
#' @param crs_str sf crs object denoting the output coordinate reference system for the sf polygon. Usually 4326
#'
#' @return codeArea object
#' @export
olc_to_sf <- function(code, lat, lng, latPrecision, lngPrecision, crs_str = sf::st_crs(4326)) {
  olc_poly <-
    sf::st_polygon(
      x = list(
        matrix(c(
          round(lng, 14), round(lat, 14),
          round(lng, 14), round(lat + latPrecision, 14),
          round(lng + lngPrecision, 14), round(lat + latPrecision, 14),
          round(lng + lngPrecision, 14), round(lat, 14),
          round(lng, 14), round(lat, 14)
        ) |> as.numeric(), ncol = 2, byrow = TRUE)
      ),
      dim = "XY"
    ) |> sf::st_sfc(crs = crs_str)

  new_code_area(
    code = code,
    codeLength = nchar(code),
    polygon_sf = olc_poly
  )
}
