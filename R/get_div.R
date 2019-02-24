#' @title get_div
#' @description Divides the frequency of the hybrid type according to the rules # TODO: which rules?
#' @param fr Numeric with the frequency values
#' @param nam String with the name of the weather type
#' @export
#' @import stringr
#' @keywords internal
get_div <- function(fr, nam) {
  # get the part of the frequency
  # depending on the each hybrid type of weather computed before
  # extract the names of each type in the data frame

  if (!length(fr[[1]]) == 0) {
    # set the pattern
    pattern <- paste(c("CNW","ANW"), collapse='|')
    # found the pattern of the exception
    basic <- (stringr::str_count(nam, pattern) == 1)
    nam[basic] #this is the name (CNW or ACW, the basic type name)
    # count the number of letter in each name
    num <- nchar(nam)

    # if there are 2 or its the "pattern": divided in 2 if not in 3
    fr[[1]]$n <- ifelse (num == 2 | basic,
                         fr[[1]]$n / 2,
                         fr[[1]]$n / 3)
    return(fr)
  }
}
