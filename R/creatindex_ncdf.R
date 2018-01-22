#' @title createindex_ncdf
#'
#' @description Converts 3D arrays with daily values of airflow indices to NETCDF files.
#'
#' @param cwt_out  List with a list for each grid point that contains two objects $CWT and $indices.
#' @param times    Numeric with the dates.
#' @param centralp Numeric with the centralp obtained from the main program.
#' @param path     Path name to create the output file.
#' @return A ncdf file with the airflow indices.
#'
#' @examples
#' \donttest{
#' # This is a long running example
#' cwtGlobal <- extended_jc(press$msl, press$loni, press$lati, press$dates, gale=FALSE, num_cores=2)
#' # Create ncdf file (one file with all types)
#' createindex_ncdf(cwtGlobal, press$dates, cwtGlobal$centralp, path = NULL)
#'}
#'
#'@seealso
#' \code{\link{create_ncdfcwt}\link{extended_jc}}
#'@export
createindex_ncdf <- function(cwt_out, times, centralp, path=NULL) {


  if (!is.null(path)) {
    prefix <- file.path(path, "airflow_ind_")
  } else {
    prefix <- file.path(tempdir(), "airflow_ind_")
  }
  # Extracting longitude and latitude values for the central point

  a <- lapply(centralp, function(x) x[[1]])
  longvector <- unique(unlist(a))
  # Change values 0-360 into -180-180
  loni <- ifelse(longvector <= 180, longvector, - (360 - longvector))
  b    <- lapply(centralp, function(x) x[[2]])
  lati <- unique(unlist(b))

  # Define starting and final dates to create the file name
  start_date <- format(times, "%Y%m%d")[1]
  end_date   <- format(times, "%Y%m%d")[length(times)]

  # select the CWT airflow indices
  # The indices are: W,S, TF, ZW,ZS,Z,D
  index <- cwt_out$indices

  # Extract the names
  namind   <- names(index)
  long_nam <- c("Westerly flow", "Southerly flow",
                "Total flow", "westerly shear vorticity",
                "Southerly shear vorticity",
                "Total shear vorticity",
                "Direction")

  # Defining dimension for netcdf files
  dimx <- ncdf4::ncdim_def("longitude",  "degrees_east", loni)
  dimy <- ncdf4::ncdim_def("latitude", "degrees_north", lati)
  dimt <- ncdf4::ncdim_def("time",
                           paste("days since", start_date),
                           1:length(times), unlim = TRUE)
  missval <- -999

  # Defining the variables
  vars <- sapply (
    1:length(namind),
    function(i) {
      ncdf4::ncvar_def(name = namind[i], "hPalat",
                       list(dimx, dimy, dimt),
                       missval = missval,
                       longname = long_nam[i])

    },
    simplify = FALSE
  )

  # Create ncdf files
  file <- paste(prefix, start_date,
                "-", end_date, ".nc", sep = "")
  filnc <- ncdf4::nc_create(file, vars)

  #Create empty data elements for CWT
  data_ind <- vector("list", length(1:length(namind)))

  for (i in 1:length(namind)) {
    data_ind[[i]] <- index[[i]]
    ncdf4::ncvar_put(filnc, namind[i], data_ind[[i]])
  }

  ncdf4::nc_close(filnc)
}
