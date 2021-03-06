% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creatindex_ncdf.R
\name{createindex_ncdf}
\alias{createindex_ncdf}
\title{createindex_ncdf}
\usage{
createindex_ncdf(cwt_out, times, centralp, path = NULL)
}
\arguments{
\item{cwt_out}{List with a list for each grid point that contains two objects $CWT and $indices.}

\item{times}{Numeric with the dates.}

\item{centralp}{Numeric with the centralp obtained from the main program.}

\item{path}{Path name to create the output file.}
}
\value{
A ncdf file with the airflow indices.
}
\description{
Converts 3D arrays with daily values of airflow indices to NETCDF files.
}
\examples{
\donttest{
# This is a long running example
cwtGlobal <- extended_jc(press$msl, press$loni, press$lati, press$dates, gale=FALSE, num_cores=2)
# Create ncdf file (one file with all types)
createindex_ncdf(cwtGlobal, press$dates, cwtGlobal$centralp, path = NULL)
}

}
\seealso{
\code{\link{create_ncdfcwt}\link{extended_jc}}
}
