#' Encode a location into an Open Location Code
#'
#' @param latitude A latitude in signed decimal degrees. Will be clipped to the range -90 to 90.
#' @param longitude A longitude in signed decimal degrees. Will be normalised to the range -180 to 180.
#' @param codeLength The number of significant digits in the output code, not including any separator characters.
#' @param code_alphabet_ The character set used to encode the values
#' @param encoding_base_ The base to use to convert numbers to/from
#' @param pair_code_length_ Maximum code length using lat/lng pair encoding. Excludes prefix and separator characters
#' @param final_lat_precision_ Multiply latitude by this much to make it a multiple of the finest precision
#' @param final_lng_precision_ Multiply longitude by this much to make it a multiple of the finest precision
#' @param max_digit_count_ The max number of digits to process in a plus code
#' @param latitude_max_ The maximum value for latitude in degrees
#' @param longitude_max_ The maximum value for longitude in degrees
#' @param grid_columns_ Number of columns in the grid refinement method
#' @param grid_rows_ Number of rows in the grid refinement method
#' @param grid_code_length_ Number of digits in the grid precision part of the code
#' @param seperator_position_ Expected location of the separator character
#'
#' @description
#' Produces a code of the specified length, or the default length if no length is provided. The length determines the accuracy of the code. The default length is 10 characters, returning a code of approximately 13.5x13.5 meters. Longer codes represent smaller areas, but lengths > 14 are sub-centimetre and so 11 or 12 are probably the limit of useful codes.
#'
#'
#' @return An Open Location Code
#' @export
#'
#' @examples
#'
#' encode(47.365590, 8.524997)
encode <- function(latitude, longitude, codeLength = 10, code_alphabet_ = "23456789CFGHJMPQRVWX", encoding_base_ = nchar(code_alphabet_), pair_code_length_ = 10, final_lat_precision_ = 25000000, final_lng_precision_ = 8192000, max_digit_count_ = 15, latitude_max_ = 90, longitude_max_ = 180, grid_columns_ = 4, grid_rows_ = 5, grid_code_length_ = max_digit_count_ - pair_code_length_, seperator_position_ = 9) {
  if (codeLength < 2 || (codeLength < pair_code_length_ && codeLength %% 2 == 1)) {
    cli::cli_abort(c(
      "Error while encoding Open Location Code:",
      "Invalid Open Location Code length - {codeLength}"
    ))
  }
  codeLength <- min(codeLength, max_digit_count_)
  # Ensure that latitude and longitude are valid.
  latitude <- clipLatitude(latitude)
  longitude <- normalizeLongitude(longitude)
  # Latitude 90 needs to be adjusted to be just less, so the returned code
  # can also be decoded.
  if (latitude == 90) {
    latitude <- latitude - computeLatitudePrecision(codeLength)
  }
  code <- "" ## we might not need this

  # Compute the code.
  # This approach converts each value to an integer after multiplying it by
  # the final precision. This allows us to use only integer operations, so
  # avoiding any accumulation of floating point representation errors.

  # Multiply values by their precision and convert to positive.
  # Force to integers so the division operations will have integer results.
  ## Note: Python requires rounding before truncating to ensure precision!
  # does R?
  latVal <- round((latitude + latitude_max_) * final_lat_precision_, 6)
  lngVal <- round((longitude + longitude_max_) * final_lng_precision_, 6)

  # Compute the grid part of the code if necessary.
  # I'm pretty sure there is a more effective way of doing this
  # in R, but I'm sticking with this for the moment to keep it
  # similar to the python
  if (codeLength > pair_code_length_) {
    for (i in seq(1, max_digit_count_ - pair_code_length_)) {
      latDigit <- latVal %% grid_rows_ |> floor()
      lngDigit <- lngVal %% grid_columns_ |> floor()
      ndx <- latDigit * grid_columns_ + lngDigit + 1
      code <- stringr::str_sub(code_alphabet_, ndx, ndx) |> paste0(code)
      latVal <- latVal %/% grid_rows_
      lngVal <- lngVal %/% grid_columns_
    }
  } else {
    latVal <- latVal %/% (grid_rows_^grid_code_length_)
    lngVal <- lngVal %/% (grid_columns_^grid_code_length_)
  }
  # Compute the pair section of the code.
  for (i in seq(1, pair_code_length_ %/% 2)) {
    lng_index <- lngVal %% encoding_base_ + 1
    lat_index <- latVal %% encoding_base_ + 1
    code <- stringr::str_sub(code_alphabet_, lng_index, lng_index) |> paste0(code)
    code <- stringr::str_sub(code_alphabet_, lat_index, lat_index) |> paste0(code)
    latVal <- latVal %/% encoding_base_
    lngVal <- lngVal %/% encoding_base_
  }
  # Add the separator character.
  code <- stringr::str_glue("{stringr::str_sub(code, 1L, seperator_position_ - 1)}+{stringr::str_sub(code, seperator_position_, -1L)}")

  # If we don't need to pad the code, return the requested section.
  if (codeLength >= seperator_position_) {
    return(stringr::str_sub(code, 1L, codeLength + 1))
  }
  # Pad and return the code.
  stringr::str_sub(code, 1L, codeLength) |>
    stringr::str_pad(seperator_position_ - codeLength, side = "right", pad = "0") |>
    paste0("+")
}
