#' @title getmat_cwt
#' @description Puts in a matrix the resulting list
#' @param cwt_out List with the frequencies of weather types and indices
#' @param centralp List with multiples central points
#' @param times Array with the dates
#' @param out String to select the output
#' @export
#' @keywords internal
getmat_cwt <- function(cwt_out, centralp, times, out = c("CWT", "indices")) {
  # Extract coordinates
  a <- lapply(centralp, function(x) x[[1]])
  longvector <- unique(unlist(a))
  # Change values 0-360 into -180-180
  loni <- ifelse(longvector <= 180, longvector, - (360 - longvector))
  b <- lapply(centralp, function(x) x[[2]])
  lati <- unique(unlist(b))

  # convert to matrix CWT and airflow indices
  if (out == "CWT") {
    types    <- lapply(cwt_out, function(x) x$CWT)
    nam_cwt  <- names(cwt_out[[1]]$CWT)

    # Create the matrix for each WT
    data_cwt <- vector("list", length(1:length(nam_cwt)))
    new_data <- vector("list", length(1:length(nam_cwt)))
    mat_cwt  <- vector("list", length(1:length(nam_cwt)))
    tmat_cwt <- vector("list", length(1:length(nam_cwt)))

    for (k in 1:length(nam_cwt)) {

      data_cwt[[k]] <- lapply(types, function(x) x[[k]])
      new_data[[k]] <- unlist(lapply(data_cwt[[k]], function(x) x$n))
      mat_cwt[[k]]  <- array(new_data[[k]],
                             dim = c(length(times),
                                     length(loni),
                                     length(lati)))
      #change the order of the dimensions [loni,lati,times]
      tmat_cwt[[k]] <- aperm(mat_cwt[[k]], c(2, 3, 1))
    }

    names(tmat_cwt) <- nam_cwt
    return("cwt" = tmat_cwt)

  } else if (out == "indices") {

    # Ignore the first column that contains the date
    indices <- lapply(cwt_out, function(x) x$indices[ - 1])
    nam_ind <- names(indices[[1]])
    # Create empty data elements for CWT
    data_ind <- vector("list", length(1:length(nam_ind)))
    new_data <- vector("list", length(1:length(nam_ind)))
    mat_ind  <- vector("list", length(1:length(nam_ind)))
    tmat_ind <- vector("list", length(1:length(nam_ind)))

    for (i in 1:length(nam_ind)) {

      data_ind[[i]] <- lapply(indices, function(x) x[[i]])
      new_data[[i]] <- unlist(data_ind[[i]])
      mat_ind[[i]]  <- array(new_data[[i]],
                             dim = c(length(times),
                                     length(loni),
                                     length(lati)))
      #change the order of the dimensions
      tmat_ind[[i]] <- aperm(mat_ind[[i]], c(2, 3, 1))
    }

    names(tmat_ind) <- nam_ind
    return("indices" = tmat_ind)
  }
}
