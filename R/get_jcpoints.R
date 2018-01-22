#' @title get_jcpoints
#'
#' @description Computes the 16-grid points that determine the JC-scheme, based on the
#'              coordinates of the central point.
#'
#' @param lon Longitude of the central point.
#' @param lat Latitude of the central point.
#' @details The function excludes the poles and the equatorial areas between 25S-25N.
#' @return A data frame with the coordinates of the 16-grid points and the central point.
#'
#' @seealso
#'\code{\link{plot_jcscheme}}
#' @export
get_jcpoints <- function(lon, lat) {

  # Get the grid points configuration (16) 10 lon x 5 lat
  # i is the longitudinal distance between points (10 by default)
  # j is the latitudinal distance between points (5 by default)
  i  <- 10
  j  <- 5
  cp <- c(lon, lat)

  if ( (cp[2] == 90 | cp[2] == (- 90)) | (cp[2] < 25 & cp[2] > ( - 25))) {
    points12   <- data.frame(matrix(nrow = 2, ncol = 16))
    cp         <- rbind(lon, lat)
    points_tot <- data.frame(points12, cp)

  } else {
    # Grid configuration

    p1 <- c(lon - (i / 2), lat + 2 * j)
    p2 <- c(p1[1] + i, p1[2])

    points12 <- array(NA, dim = c(2, 16))
    points12[, 1] <- p1
    points12[, 2] <- p2
    points12[, 3] <- c(p1[1] - i, p1[2] - j)

    for (l in 4:16) {

      if (l == 4 | l == 5 | l == 6) {
        points12 [, l] <- c(points12[1, l - 1] + i, points12[2, l - 1])

      } else {
        if (l == 7) {
          points12[, l] <- c(points12[1, l - 4], points12[2, l - 1] - j)
        } else {
          if (l == 8 | l == 9 | l == 10) {
            points12 [, l] <- c(points12[1, l - 1] + i, points12[2, l - 1])
          } else {
            if (l == 11) {
              points12[, l] <- c(points12[1, l - 4], points12[2, l - 1] - j)
            } else {
              if (l == 12 | l == 13 | l == 14) {
                points12[, l] <- c(points12[1, l - 1] + i, points12[2, l - 1])
              } else {
                if (l == 15) {
                  points12[, l] <- c(points12[1, l - 3], points12[2, l - 1] - j)
                } else {
                  points12[, l] <- c(points12[1, l - 3], points12[2, l - 1])
                }
              }
            }
          }
        }
      }
    }

    points12 <- data.frame(points12)

    for (k in 1:16) {
      if (points12[2, k] > 90){

        points12[2, k] <- 180 - points12[2, k]
        points12[1, k] <- points12[1, k] - 180

        }else{
        if (points12[2, k] < ( - 90)){
          points12[2, k] <- - 180 - points12[2, k]
          points12[1, k] <- points12[1, k] - 180
         }
        }

      if (points12[1, k] > 180){
        points12[1, k] <- points12[1, k] - 360
      } else {
        if (points12[1, k] <= (-180)){
          points12[1, k] <- points12[1, k] + 360
        }
      }
    }
    points_tot <- data.frame(points12, cp)
  }

  return(points_tot)
}
