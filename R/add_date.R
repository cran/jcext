#' @title add_date
#' @description Adds dates in wich the weather types are obtained
#' @param f Array with frequencies
#' @param d Array of dates
#' @export
#' @keywords internal
add_date <- function (f, d) {
  # function used to merge dates

  if (is.null(f) | length(f[[1]]) == 0) {
    f <- data.frame("date" = d, "n" = 0)
  }

  f <- merge(f, d, all = T)
  f$n[is.na(f$n)] <- 0

  return(f)
}
