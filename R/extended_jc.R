#' @title extended_jc
#' @description Gets daily classification of weather types
#'              at every grid-point over the map or selected area of interest according
#'              to the Jenkison and Collison scheme.
#'
#' @param mslp  3-Dimensional array ([loni,lati,time]) with mean sea level pressure in Pa.
#' @param loni  Array with longitude values.
#' @param lati  Array with latitude values.
#' @param times Array with the dates used.
#' @param gale  Logial. If TRUE, the function returns also Gale days.
#' @param num_cores Number of cores (2 by default).
#' @return A list with two objects:
#' \itemize{
#' \item A list of eleven matrix of daily frequencies of weather types ("wtypes").
#'       Each matrix is a 3D array [loni,lati,times] and it refers to each weather type (N,NE,E,SE,S,SW,W,A,C and U).
#' \item A list of six matrix of daily frequency of airflow indices ("indices").
#'       Each matrix is a 3D array [loni,lati,times] and it refers to each airflow index (W,S,TF,ZW,ZS,Z and D).
#' \item A list with the central points for which the classification is applied.
#' }
#'
#' @references
#' Otero, N., Sillmann, J. & Butler, T.
#' \emph{Assessment of an extended version of the Jenkinson–Collison classification on CMIP5 models over Europe}
#' Climate Dynamics. https://doi.org/10.1007/s00382-017-3705-y
#'
#' @seealso
#' \code{\link{classification_jc}}
#' \code{\link{calculate_cwt}}
#'
#' @examples
#'
#' # Load data
#' data(press)
#' # Get coordinates
#' longitudes <- press$loni
#' latitudes  <- press$lati
#' times      <- press$dates
#'
#' # Example when the classification is restricted to an area
#' # Select longitudes and latitudes within the European domain: -10W,40E, 40N,70N
#' ilon  <- which(longitudes>(-10)&longitudes<40)
#' loni  <- longitudes[ilon]
#' ilat  <- which(latitudes>40&latitudes<70)
#' lati  <- latitudes[ilat]
#' cwtEU <- extended_jc(press$msl[ilon,ilat,], loni, lati, times, gale=FALSE, num_cores=2)
#'  \dontrun{
#' # Not run
#' # This is a long running example
#' # Get the classification for the whole map, all longitudes and latitudes
#' cwtGlobal <- extended_jc(press$msl, longitudes, latitudes, times, gale=FALSE, num_cores=2)
#' }
#'
#' @export
#' @import parallel
#' @import stringr
extended_jc <- function(mslp, loni, lati, times, gale = FALSE, num_cores=2) {
  jc_points <- apply(expand.grid(loni, lati), MARGIN = 1,
                    function(x) get_jcpoints(x[[1]], x[[2]]))

  grid16   <- lapply(jc_points, function(x) x[1:16])
  centralp <- lapply(jc_points, function(x) x$cp)

  cl <- parallel::makeCluster(num_cores)

  parallel::clusterExport(cl,
                          list("classification_jc", "mslp", "grid16",
                               "centralp", "loni", "lati", "times", "gale"),
                          envir = environment())

  cwt_out <- parallel::parSapply(cl, 1:length(grid16),
                                 function(i) {
                                   classification_jc(mslp, grid16[[i]],
                                                     centralp[[i]], loni, lati,
                                                    times, gale)
                                 },
                                 simplify = F)

  parallel::stopCluster(cl)

  wtypes  <- getmat_cwt(cwt_out, centralp, times, "CWT")
  indices <- getmat_cwt(cwt_out, centralp, times, "indices")

  return(list("wtypes" = wtypes,
              "indices" = indices,
              "centralp" = centralp))
}
