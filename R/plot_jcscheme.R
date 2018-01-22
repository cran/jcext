#' @title Plot the classification scheme
#'
#' @description Visualises the original Jenkinson & Collison scheme
#'              for one given central point surrounded by the 16-points.
#'
#' @param centralp  Array with the central point (lonigutde, latitude).
#' @param loni      Array with longitude values.
#' @param lati      Array with latitude values.
#' @param fullmap   Logical. If TRUE a fullmap is plotted, if FALSE, only the region selected is plotted.
#' @return A plot device
#' @details      The map shows the scheme over the whole map,
#'               or either it shows the scheme over the selected region. For that, the maximum and mininum
#'               coordinates are defined as:
#'               Maximum longitude, by default defined by the points: x6, x10 or 14.
#'               Minimum longitude, by default defined by the points: x3, x7 or 11.
#'               Maximum latitude, by default defined by the points: x1 or x2.
#'               Minimum latitude, by default defined by the points: x15 or 16.
#'
#' @examples
#' # Visualise the scheme for one point
#' library(jcext)
#' # Define a central point
#' mycentral <- c(10,50)
#' # load the data to get coordinates
#' data(press)
#' # Visualise the whole map
#' plot_jcscheme(mycentral,press$loni,press$lati,fullmap=TRUE)
#' # Visualise the region
#' plot_jcscheme(mycentral,press$loni,press$lati,fullmap=FALSE)
#' @seealso
#' \code{\link{get_jcpoints}}
#' @export
#' @importFrom graphics points text grid
#' @importFrom sp plot
plot_jcscheme <- function(centralp, loni, lati, fullmap = TRUE) {

  points16 <- get_jcpoints(centralp[1], centralp[2])[1:16]

  if (!all(is.na(points16))) {

    ymx <-  points16[2, 1]
    ymn <-  points16[2, 15]

    #Defining newmap
    newmap <- rworldmap::getMap()
    # If centralp point is located in the Pacific
    # area another projection is required

    if (centralp[1] > 150 || centralp[1] < ( - 150)) {
      #Change lon to 0-360 (just for plot)
      points16[1, ] <- ifelse ( (points16[1, ] < 0 & points16[1, ] > ( - 180)),
                                points16[1, ] + 360, points16[1, ])

      if (fullmap) {

        maps::map(database = "world",
                  projection = "rectangular",
                  parameter = 0,
                  orientation = c(90, 0, 180),
                  col = "lightyellow",
                  bg = "white",
                  wrap = TRUE,
                  fill = TRUE,
                  resolution  = 0,
                  ylim = c(-60, 90),
                  mar = c(0, 0, 0, 0))

        loc <- pi * (centralp[1] - 180) / 180
        lac <-  pi * centralp[2] / 180
        points(loc, lac, pch = 20, col = "red", cex = 1)
        p_names <- array(NA, dim = c(1, 16))
        for (i in 1:length(points16)) {
          p_names[i] <- paste("p", i, sep = "")

          lon <- pi * (points16[1, i] - 180) / 180
          lat <- pi * points16[2, i] / 180

          points(lon, lat, pch = 16, col = "grey20", cex = 1)

          text(lon, lat, labels = as.character(p_names[i]),
                         col = "grey20",
                         cex = 0.6, font = 2, offset = 0.5, adj = c(0, 2))
        }

         grid(col = "black")

      } else {
        xmx <-  points16[1, 6]
        xmn <-  points16[1, 3]


        m <- plot(newmap,
                  ylim = c(ymn, ymx),
                  xlim = c(xmn, xmx),
                  col = "light yellow",
                  axes = T,
                  asp = 1)

        points(centralp[1], centralp[2], pch = 20, col = "red", cex = 1)

        p_names <- array(NA, dim = c(1, 16))
        for (i in 1:length(points16)) {

          p_names[i] <- paste("p", i, sep = "")
          points(points16[1, i], points16[2, i],
                           pch = 16, col = "grey20", cex = 1)
          text(points16[1, i], points16[2, i],
                         labels = as.character(p_names[i]), col = "grey20",
                         cex = 0.6, font = 2, offset = 0.5, adj = c(0, 2))

        }
        grid(col = "black")
      } # End full map
    } else if (fullmap) {


      m <- plot(newmap, col = "light yellow", axes = T, asp = 1)
      points(centralp[1], centralp[2], pch = 20, col = "red", cex = 1)
      p_names <- array(NA, dim = c(1, 16))

      for (i in 1:length(points16)) {

        p_names[i] <- paste("p", i, sep = "")
        points(points16[1, i], points16[2, i],
                         pch = 16, col = "grey20", cex = 1)
        text(points16[1, i], points16[2, i],
                       labels = as.character(p_names[i]), col = "grey20",
                       cex = 0.6, font = 2, offset = 0.5, adj = c(0, 2))
      }

      grid(col = "black")

    } else {

      xmx <- points16[1, 6]
      xmn <- points16[1, 3]
      m   <- plot(newmap,
                  ylim = c(ymn, ymx),
                  xlim = c(xmn, xmx),
                  col = "light yellow",
                  axes = T,
                  asp = 1)
      points(centralp[1], centralp[2], pch = 20, col = "red", cex = 1)

      p_names <- array(NA, dim = c(1, 16))

      for (i in 1:length(points16)) {

        p_names[i] <- paste("p", i, sep = "")
        points(points16[1, i], points16[2, i],
                         pch = 16, col = "grey20", cex = 1)
        text(points16[1, i], points16[2, i],
                       labels = as.character(p_names[i]), col = "grey20",
                       cex = 0.6, font = 2, offset = 0.5, adj = c(0, 2))

      }
      grid(col = "black")
    }

  } else {
    print("try other point, this is not allowed")
  }
}
