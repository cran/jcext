#' @title read_ncdata
#'
#' @description Reads a ncdf input file to extract the input for the classification:
#'              pressure field, longitudes, latitudes and dates.
#'              Absolute time is required to read the dates properly.
#'
#'
#' @param ncinput A NETCDF file with pressure field.
#' @param nam_coor Names of space and time coordinates
#' @param units   Units required (Pa or hPa).
#'
#' @return A list with:
#' \itemize{
#' \item A 3D-array of mean sea level (or pressure field) as [lon,lat,times]. The units returned as hPa.
#' \item A numeric with longitudes values.
#' \item A numeric with latitudes values.
#' \item A numeric with dates values.
#' }
#' @export
#'
read_ncdata <- function(ncinput,nam_coor, units) {

  ncdata  <- ncdf4::nc_open(ncinput)
  # Extract Coordinates
  lon.name  <- nam_coor[1]
  lat.name  <- nam_coor[2]
  time.name <- nam_coor[3]


  lat.vals  <- ncdata$dim[[lat.name]]$vals
  lon.vals  <- ncdata$dim[[lon.name]]$vals
  time.vals <- ncdata$dim[[time.name]]$vals

  date <- strptime(time.vals, "%Y%m%d")
  date <- as.Date(date)

  mvar <- ncdata$var[[1]]$name
  # To get the data
  press.data <- ncdf4::ncvar_get(ncdata,
                                 mvar,
                                 start = c(1, 1, 1),
                                 count = c(-1, -1, -1))

  # replace missing values with NA
  press.data[press.data < (- 900)] <- NA

  if (units == "Pa") {
    #Change Pa into hPa
    press <- press.data * 10 ^ ( - 2)
  } else if (units == "hPa") {
    press <- press.data
  } else {
    print("Units not valid, please return the appropiate field")
  }

  ncdf4::nc_close(ncdata)
  return(list("msl" = press,
              "loni" = lon.vals,
              "lati" = lat.vals,
              "dates" = date))
}
