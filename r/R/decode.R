#' Decode an Open Location Code into location coordinates
#'
#' @param code Open Location Code. Character
#' @param latitude_max_ The maximum value for latitude in degrees
#' @param longitude_max_ The maximum value for longitude in degrees
#' @param max_digit_count_ The max number of digits to process in a plus code
#' @param code_alphabet_ The character set used to encode the values
#' @param encoding_base_ The base to use to convert numbers to/from
#' @param pair_precision_ Inverse of the precision of the pair section of the code
#' @param pair_code_length_ Maximum code length using lat/lng pair encoding. Excludes prefix and separator characters
#' @param pair_first_place_value_ First place value of the pairs (if the last pair value is 1)
#' @param grid_columns_ Number of columns in the grid refinement method
#' @param grid_rows_ Number of rows in the grid refinement method
#' @param grid_lat_first_place_value_ First place value of the latitude grid (if the last place is 1)
#' @param grid_lng_first_place_value_ First place value of the longitude grid (if the last place is 1)
#' @param final_lat_precision_ Multiply latitude by this much to make it a multiple of the finest precision
#' @param final_lng_precision_ Multiply longitude by this much to make it a multiple of the finest precision
#'
#' @description
#' Given an Open Location Code, this function will return a codeArea object which is essentially a polygon that represents the area on the ground that the code refers to
#'
#'
#' @return A code_area object which contains the original code, code length, center coordinates and an sf polygon
#' @export
#'
#' @examples
#' decode("8FVC9G8F+6X")
decode <- function(code, latitude_max_ = 90, longitude_max_ = 180, max_digit_count_ = 15, code_alphabet_ = "23456789CFGHJMPQRVWX", encoding_base_ = nchar(code_alphabet_), pair_precision_ = encoding_base_^3, pair_code_length_ = 10, pair_first_place_value_ = encoding_base_^(pair_code_length_ / 2 - 1), grid_columns_ = 4, grid_rows_ = 5, grid_lat_first_place_value_ = grid_rows_^((max_digit_count_ - pair_code_length_) - 1), grid_lng_first_place_value_ = grid_columns_^((max_digit_count_ - pair_code_length_) - 1), final_lat_precision_ = pair_precision_ * grid_rows_^(max_digit_count_ - pair_code_length_), final_lng_precision_ = pair_precision_ * grid_columns_^(max_digit_count_ - pair_code_length_)) {
  if (!isFull(code)) {
    cli::cli_abort(c(
      "Error while decoding Open Location Code:",
      "Passed Open Location Code is not a valid full code - {code}"
    ))
  }
  # Strip out separator character (we've already established the code is
  # valid so the maximum is one), and padding characters. Convert to upper
  # case and constrain to the maximum number of digits.
  code <-
    code |>
    stringr::str_remove_all("[\\+0]") |>
    stringr::str_to_upper() |>
    stringr::str_sub(1L, max_digit_count_)
  # Initialise the values for each section. We work them out as integers and
  # convert them to floats at the end.
  normalLat <- -latitude_max_ * pair_precision_
  normalLng <- -longitude_max_ * pair_precision_
  gridLng <- gridLat <- 0
  # How many digits do we have to process?
  digits <- min(nchar(code), pair_code_length_)
  # Define the place value for the most significant pair.
  pv <- pair_first_place_value_
  # Decode the paired digits.
  for (i in seq(1, digits, by = 2)) {
    normalLat <- normalLat + (stringr::str_locate(code_alphabet_, stringr::str_sub(code, i, i))[1] - 1) * pv
    normalLng <- normalLng + (stringr::str_locate(code_alphabet_, stringr::str_sub(code, i + 1, i + 1))[1] - 1) * pv
    if (i < digits - 1) {
      pv <- pv %/% encoding_base_
    }
  }

  # Convert the place value to degrees.
  latPrecision <- pv / pair_precision_
  lngPrecision <- pv / pair_precision_

  if (nchar(code) > pair_code_length_) {
    # Initialise the place values for the grid.
    rowpv <- grid_lat_first_place_value_
    colpv <- grid_lng_first_place_value_
    # How many digits do we have to process?
    digits <- min(nchar(code), max_digit_count_)
    for (i in seq(pair_code_length_ + 1, digits)) {
      digitVal <- stringr::str_locate(code_alphabet_, stringr::str_sub(code, i, i))[1] - 1
      row <- digitVal %/% grid_columns_
      col <- digitVal %% grid_columns_
      gridLat <- gridLat + row * rowpv
      gridLng <- gridLng + col * colpv
      if (i < digits) {
        rowpv <- rowpv %/% grid_rows_
        colpv <- colpv %/% grid_columns_
      }
    }

    # Adjust the precisions from the integer values to degrees.
    latPrecision <- rowpv / final_lat_precision_
    lngPrecision <- colpv / final_lng_precision_
  }

  # Merge the values from the normal and extra precision parts of the code.
  lat <- normalLat / pair_precision_ + gridLat / final_lat_precision_
  lng <- normalLng / pair_precision_ + gridLng / final_lng_precision_

  return(
    olc_to_sf(code, lat, lng, latPrecision, lngPrecision)
  )
}
