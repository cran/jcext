#' @title calculate_frequency
#' @description Computes the frequency of weather types, after dividing the
#'              frequency of the hybrid types
#' @param datalist List with the hybrid types
#' @param purelist List with the rest of types
#' @param let String with the name of the type
#' @export
#' @keywords internal
calculate_frequency <- function(datalist, purelist, let) {

  for (i in 1:length(let)) {
    l <- let[i]

    purelist[[l]] <- rbind(purelist[[l]], datalist)
    purelist[[l]] <- purelist[[l]][with(purelist[[l]],
                                        order(purelist[[l]]$date,
                                              - as.numeric(purelist[[l]]$n))), ]
  }
  return(purelist)
}
