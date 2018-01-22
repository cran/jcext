#' @title calculate_constant
#' @description Calculates constants required to apply the rules
#' @param lon Numeric longitude of the central point
#' @param lat Numeric latitude of the central point
#' @export
#' @keywords internal
calculate_constant <- function(lon, lat) {
  # get paremeters required to obtain the indices
  # Input arguments: longitude and latitude of the central point

  a  <- c(lon, lat)
  a1 <- c(lon, lat - 5)
  a2 <- c(lon, lat + 5)

  A <- round(1 / cos(a[2] * pi / 180), 2)
  B <- round(sin(a[2] * pi / 180) / sin(a1[2] * pi / 180), 2)
  C <- round(sin(a[2] * pi / 180) / sin(a2[2] * pi / 180), 2)
  D <- round(1 / ( ( 2 * ( (cos(a[2] * pi / 180)) ^ 2))), 2)

  return(c(A, B, C, D))
}
