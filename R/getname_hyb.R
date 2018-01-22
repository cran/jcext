#' @title getname_hyb
#' @description Gets the name of the hybrid types to account for the rest of the types
#' @param hybrid_div List with the hybrid types
#' @export
#' @import stringr
#' @keywords internal
getname_hyb <- function(hybrid_div) {

  # function used to extract the letter of hybrid names
  # it will be used to add the values to pureList
  nam_hyb <- names(hybrid_div)
  pattern <- paste(c("CNW", "ANW"), collapse = "|")
  # found the pattern of the exception
  basic <- (stringr::str_count(nam_hyb, pattern) == 1)
  nobasic <- nam_hyb[ !basic]
  basic_c <- "CNW"
  basic_a <- "ANW"
  nam_c <- nam_hyb[nam_hyb == basic_c]
  nam_a <- nam_hyb[nam_hyb == basic_a]
  lett_tot <- character()

  # Check if there is nobasic types
  if (length(nobasic) != 0) {

    # If the name is not basic type(CNW or CNW), split the letters
    for (i in 1:length(nobasic)) {
      lett <- strsplit( (nobasic[i]), "")
      lett_tot <- c(lett_tot, lett)
    }

    let1    <- lapply(lett_tot, function(x) x[[1]])
    let1_a  <- (let1 == "C")
    let1_c  <- (let1 == "A")
    let_hyb_c <- lett_tot[let1_c]
    let_hyb_a <- lett_tot[let1_a]
    # Order the array
    let_t <- c(let_hyb_c, nam_c, let_hyb_a, nam_a)

    #If the name is CNW or ANW divide it into "C" "NW" and "A" "NW"
    for (i in 1:length(let_t)) {
      if (length(let_t[[i]]) == 1) {
        let_t[[i]] <- c(substring(let_t[[i]], 1, 1),
                       substring(let_t[[i]], 2, 3))
      }
    }

    # If there are not No basic types, only CNW AND/OR ANW will be added
  } else {

    let_t <- list(nam_c, nam_a)
    # Filter if there is empty element
    let_t <- let_t[lapply(let_t, length) > 0]

    for (i in 1:length(let_t)) {
      let_t[[i]] <- c(substring(let_t[[i]], 1, 1),
                     substring(let_t[[i]], 2, 3))
    }
  }

  return(let_t)
}
