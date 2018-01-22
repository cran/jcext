#' @title create_ncdfcwt
#'
#' @description Converts 3D arrays with daily frequencies of weather types to NETCDF files.
#'
#' @param cwt_out  List with a list for each grid point that contains two objects: CWT and indices.
#' @param times    Numeric with the dates.
#' @param centralp Numeric with the centralp obtained from the main program.
#' @param onefile  Logical. If TRUE one single output file with all WT is created, if FALSE
#'                 one file per type is created.
#' @param path     Path name to create the output file.
#' @return         A netcdf file with all weather types or one file per type.
#' @examples
#' \donttest{
#' # This is a long running example
#' cwtGlobal <- extended_jc(press$msl, press$loni, press$lati, press$dates, gale=FALSE, num_cores=2)
#' # Create ncdf file (one file with all types)
#' create_ncdfcwt(cwtGlobal, press$dates, cwtGlobal$centralp, onefile = TRUE, path = NULL)
#'}
#'
#' @seealso
#'\code{\link{createindex_ncdf}\link{extended_jc}}
#'
#' @export
create_ncdfcwt <- function(cwt_out, times, centralp, onefile = TRUE, path = NULL) {

  if (!is.null(path)) {
    prefix <- file.path(path, "nctypes_JC_")
  } else {
    prefix <- file.path(tempdir(), "nctypes_JC_")
  }

  # Extract coordinates from central points
  a <- lapply(centralp, function(x) x[[1]])
  longvector <- unique(unlist(a))
  # Change values 0-360 into -180-180
  loni <- ifelse(longvector <= 180, longvector, - (360 - longvector))
  b <- lapply(centralp, function(x) x[[2]])
  lati <- unique(unlist(b))
  # Define starting and final dates to create the file name
  start_date <- format(times, "%Y%m%d")[1]
  end_date   <- format(times, "%Y%m%d")[length(times)]
  # select CWT types for getting indices that would correspond to the 2 element
  types <- cwt_out$wtypes
  # Extract the names
  nam_cwt <- names(types)

  # Check if the output contain Gale days.
  # There are 11 basic types, if the lenght is greater than 11,
  # then gale days are also computed
  if (length(nam_cwt > 11)) {
    long_nam <- c("North", "Northeast", "East", "Southeast",
                  "South", "Sourthwest", "West", "Northwest",
                  "Anticyclonic", "Cyclonic", "Unclassified", "Gale")
  } else {
    long_nam <- c("North", "Northeast", "East", "Southeast",
                  "South", "Sourthwest", "West", "Northwest",
                  "Anticyclonic", "Cyclonic", "Unclassified")
  }

  # Defining dimension for netcdf files
  dimx <- ncdf4::ncdim_def("longitude", "degrees_east", loni)
  dimy <- ncdf4::ncdim_def("latitude", "degrees_north", lati)
  dimt <- ncdf4::ncdim_def("time", paste("days since", start_date),
                           1:length(times), unlim = TRUE)
  missval <- -999

  # Defining the variables
  vars <- sapply (
    1:length(nam_cwt),
    function(v) {
      ncdf4::ncvar_def(name = nam_cwt[v],
                       "days",
                       list(dimx, dimy, dimt),
                       missval = missval,
                       longname = long_nam[v])
    },
    simplify = FALSE
  )

  # Creating output files
  if (onefile) {
    fname  <- paste( prefix, start_date, "-",
                    end_date, ".nc", sep = "" )
    filnc  <- ncdf4::nc_create( fname, vars )

  } else {
    file <- sapply(
      1:length(nam_cwt),
      function(j) {
        paste( prefix, nam_cwt[j], "_",
              start_date, "-", end_date, ".nc", sep = "" )
      })
    cat(file, "\n")

    nc1 <- sapply(
      1:length(nam_cwt),
      function(m) {
        ncdf4::nc_create(file[m], vars[[m]])
      },
      simplify = FALSE
    )
  }

  # Create empty data elements for CWT
  data_cwt <- vector("list", length(1:length(nam_cwt)))

  # Loop over all variables starting from i=1
  for (k in 1:length(nam_cwt)) {

    data_cwt[[k]] <- types[[k]]

    if (onefile) {
      ncdf4::ncvar_put(filnc, nam_cwt[k], data_cwt[[k]])

    } else {
      ncdf4::ncvar_put(nc1[[k]], nam_cwt[k], data_cwt[[k]])
      ncdf4::nc_close(nc1[[k]])
    }
  }

  if (onefile) {
    ncdf4::nc_close(filnc)
  }
}
