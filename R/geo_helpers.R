#' @title get longitude center
#' @description get the longitude center of a given vector or column of longitude values
#' @usage get_long_center(x)
#' @param x is a vector or column of longitude values
#' @return A single numeric value that is the center (average) of x
#' @examples
#' \dontrun{
#' my_longitudes <- sample(-180:180, 100, replace = TRUE)
#' get_long_center(my_longitudes)
#' }
#' @export
get_long_center <- function(x) {
  max_long <- max(as.numeric(x))

  min_long <- min(as.numeric(x))

  if (max_long >= 0 & min_long >= 0) {
    long_center <- (max_long - ((max_long - min_long) / 2))
  } else {
    long_center <- max_long - abs((max_long - min_long) / 2)
  }
  long_center
}


#' @title get latitude center
#' @description get the latitide center of a given vector or column of latitude values
#' @usage get_lat_center(x)
#' @param x is a vector or column of latitude values
#' @return A single numeric value that is the center (average) of @param x
#' @examples
#' \dontrun{
#' my_latitudes <- sample(-90:90, 100, replace = TRUE)
#' get_lat_center(my_latitudes)
#' }
#' @export
get_lat_center <- function(x) {
  max_lat <- max(as.numeric(x))

  min_lat <- min(as.numeric(x))

  if (max_lat >= 0 & min_lat >= 0) {
    lat_center <- (max_lat - ((max_lat - min_lat) / 2))
  } else {
    lat_center <- max_lat - abs((max_lat - min_lat) / 2)
  }
  lat_center
}
