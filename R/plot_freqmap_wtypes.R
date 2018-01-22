#' @title plot_freqmap_wtypes
#'
#' @description Visualises absolute frequencies of the 11 main types from
#'              the extended_jc over the period.
#'
#' @param mat        Matrix output from extended_jc [loni,lati,time]
#' @param loni       Vector with longitude values must be -180, 180
#' @param lati       Latitude values
#' @param all.types  Logical. If TRUE all weather types are plotted in the same plot
#' @param mytype     Character with the name of the weather type wanted (i.g. N,NE,E,SE,S,SW,W,A,C and U)
#' @param center     Logical. If TRUE a center map is plotted

#' @return A ggplot2 map
#' @seealso  \code{\link{extended_jc}}
#' @examples
#' \dontrun{
#' library(jcext)
#' # This is a long running example for plotting results for all types globally
#' cwtGlobal <- extended_jc(press$msl, press$loni,press$lati, press$dates, gale=FALSE, num_cores=2)
#' wtypesGlobal <- cwtGlobal$wtypes
#' plot_freqmap_wtypes(wtypesGlobal,press$loni, press$lati, all.types = TRUE,mytype = NULL ,center = T)
#' # Plot the global results only for one type
#' plot_freqmap_wtypes(wtypesGlobal,press$loni, press$lati, all.types = FALSE,mytype = "C" ,center = T)
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
plot_freqmap_wtypes <- function(mat, loni, lati, all.types = TRUE,
                                mytype = NULL, center = TRUE){

  # Compute absolute frequencies of each WT
  vals <- lapply(mat, function(x) apply(x, 1:2, sum))
  long <- lon <- lat <- group <- n <- WT <- aes <- NULL
  if (center){
    # Shift longitudes values to -180 180 (in case they are defined as 0-360)
    loni <- ifelse ( (loni > 180 & loni < 360), loni - 360, loni)
  }
  # Convert to an appropiate data.frame to plot
  a <- sapply (names(vals),
               function(m){
                 a <- sapply (1:length(vals),
                              function(m){
                                x <- data.frame(vals[[m]])
                                x <- stats::reshape (
                                  x,
                                  idvar     = c("lon"),
                                  ids       = loni,
                                  timevar   = "lat",
                                  times     = lati,
                                  v.names   = "n",
                                  varying   = list(names(x)),
                                  direction = "long"
                                )
                                data.frame("WT" = names(vals[m]), x[, c(3, 1, 2)])
                              },
                              simplify = FALSE
                 )
                 do.call("rbind", a)
               },
               simplify = FALSE
  )

  mydat <- do.call("rbind", a)

  # Define palette
  mypalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")),
                                space = "Lab")


  if (all.types){

    myplot <- ggplot2::ggplot(data = mydat, aes(x = lon, y = lat, fill = n))

  }else if (!is.null(mytype)){

    myplot <- ggplot2::ggplot(data = subset(mydat, WT %in% mytype),
                              aes(x = lon, y = lat, fill = n))

  }

    myplot <- myplot + ggplot2::geom_raster() +
      ggplot2::scale_fill_gradientn(colours = mypalette(10)) +
      ggplot2::facet_wrap( ~ WT) +
      ggplot2::labs(x = "long", y = "lat")


  # last step: overlay the map
  map_data <- NULL
  myplot <- myplot + ggplot2::geom_polygon(data = map_data(map = "world"),
            aes(x = long, y = lat, group = group),
            fill = NA, color = "black", size = 0.08)
  myplot <- myplot + ggplot2::coord_cartesian(xlim = range(mydat$lon), ylim = range(mydat$lat))

  print(myplot)
}
