#' @title isnot_validcp
#' @description Checks if the central point used is valid.
#' @param grid16 Data.frame with the 16-grid scheme
#' @param loni Array with longitudes
#' @param lati Array with latitudes
#' @export
#' @keywords internal
isnot_validcp <- function(grid16, loni, lati) {
  # Checks if the point within the area selected is correct
  if (!any(is.na(grid16))) {
    # Definig limits
    xlim1 <- min(grid16[1, ])
    xlim2 <- max(grid16[1, ])
    ylim1 <- min(grid16[2, ])
    ylim2 <- max(grid16[2, ])

    # shift loni
    lon.shif <- ifelse ( (loni > 180 & loni < 360), loni - 360, loni)

    if (xlim2 > max(lon.shif) | xlim1 < min(lon.shif)
        | ylim2 > max(lati) | ylim1 < min(lati)) {
       return(TRUE)
    } else {
       return(FALSE)
    }
  }
  return(TRUE)
}
