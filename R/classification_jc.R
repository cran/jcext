#' @title classification_jc
#'
#' @description Calculates the classification of the main weather types
#'              for one central point that is surrounded by 16-points (grid16). Wind-flow characteristics
#'              are computed for the daily pressure field according to the rules proposed by the original
#'              Jenkinson and Collison classification (see Jones et al. 1993, Jones et al. 2016).
#' @param mslp  3-Dimensional multi-array ([loni,lati,time]) with mean sea level pressure in Pa.
#' @param grid16   Data frame obtained in the main function (extended_jc) that contains the 16 grid-points defining the scheme.
#'                 First row is for longitudes, while the second row is for latitudes.
#' @param centralp Numeric that refers to the central point for which the JC classification is calculated.
#' @param loni   Array with longitude values.
#' @param lati   Array with latitude values.
#' @param times  Array with the dates used.
#' @param gale   A logical for deteriming Gale days.
#' @return Daily frequencies of Weather Types and airflow indices.
#'
#' @references {
#' Jones, P. D., Hulme M., Briffa K. R. (1993)
#' \emph{A comparison of Lamb circulation types with an objective classification scheme}
#' Int. J. Climatol. 13: 655–663.
#'
#' Jones, P. D., Harpham C, Briffa K. R. (2013)
#' \emph{Lamb weather types derived from Reanalysis products}
#' Int. J. Climatol. 33: 1129–1139.
#' }
#' @seealso  \code{\link{calculate_cwt}}
#' @examples
#' # Load data
#' data(press)
#' mslp  <- press$msl
#' loni  <- press$loni
#' lati  <- press$lati
#' times <- press$dates
#' # Define a central point
#' centralp <- c(10,50)
#' # Get the scheme for the  central point
#' grid16 <- get_jcpoints(10,50)[1:16]
#' classification_jc(mslp, grid16, centralp, loni, lati, times, gale=FALSE)
#'
#' @export
classification_jc <- function(mslp, grid16, centralp, loni, lati, times, gale) {

  # Function ClassificationWT: main function to get CWT
  # There are 27 different types that are reagrouped into 11 main types

  # Extracting coordinates from the central point:
  inputlon <- centralp[1]
  inputlat <- centralp[2]

  # IsvalidCP function checks the availabilty of Central-points
  if (isnot_validcp(grid16, loni, lati) | any(is.na(grid16))) {

    data <- data.frame("date" = times, "n" = NA)
    if (!gale) {
      f_total <- list("N" = data, "NE" = data, "E" = data, "SE" = data,
                      "S" = data, "SW" = data, "W" = data, "NW" = data,
                      "A" = data, "C" = data, "U" = data)
    } else {
      f_total <- list("N" = data, "NE" = data, "E" = data, "SE" = data,
                      "S" = data, "SW" = data, "W" = data, "NW" = data,
                      "A" = data, "C" = data, "U" = data, "G" = data)
    }
    indices <- data.frame("date" = times, "W" = NA, "S" = NA, "TF" = NA,
                          "ZW" = NA, "ZS" = NA, "Z" = NA, "D" = NA)

    return(list("CWT" = f_total, "indices" = indices))

  } else {

    totdays <- data.frame("date" = times)

    # Shift longitudes [0 360] into [-180 180]
    loni1 <- ifelse ( (loni > 180 & loni < 360), loni - 360, loni)

    # get the postions of these 16 points
    p_grid <- array(NA, dim <- c(2, 16))

    # controlling the valid points
    for (i in 1:length(grid16)) {
      lx <- loni1 == grid16[1, i]
      ly <- lati == grid16[2, i]
      if (all(!lx) | all(!ly)) {
        stop ("Any of corrdinate is not correct")
      } else {
        p_grid[1, i]  <-  which(loni1 == grid16[1, i])
        p_grid[2, i]  <-  which(lati == grid16[2, i])
      }
    }

    # Calculate circulation index
    # Get the constant (A,B,C,D) to compute the airflow indices
    parameter <- calculate_constant(inputlon, inputlat)

    # Westearly flow: W = 1/2*(12+13)-1/2*(4+5)
    W <- (1 / 2 * (mslp[p_grid[1, 12], p_grid[2, 12], ] +
                 mslp[p_grid[1, 13], p_grid[2, 13], ]) -
            1 / 2 * (mslp[p_grid[1, 4], p_grid[2, 4], ] +
                   mslp[p_grid[1, 5], p_grid[2, 5], ]))
    W <- data.frame("value" = W, "date" = times)

    # Southerly flow: S = A*[(1/4(5+2*9+13)-1/4*(4+2*8+12))
    S <- parameter[1] * (1 / 4 * (mslp[p_grid[1, 5], p_grid[2, 5], ] +
                              2 * mslp[p_grid[1, 9], p_grid[2, 9], ] +
                              mslp[p_grid[1, 13], p_grid[2, 13], ]) -
                         1 / 4 * (mslp[p_grid[1, 4], p_grid[2, 4], ] +
                                2 * mslp[p_grid[1, 8], p_grid[2, 8], ] +
                                mslp[p_grid[1, 12], p_grid[2, 12], ]))
    S <- data.frame("value" = S, "date" = times)

    # Resultant flow: F = (S^2+W^2)^1/2
    TF <- sqrt(S$value ^ 2 + W$value ^ 2)
    TF <- data.frame("value" = TF, "date" = times)

    # Westearly shear vorticity:
    # ZW = B*[1/2*(15+16)-1/2*(8+9)]-C*[1/2*(8+9)-1/2*(1+2)]
    ZW_1 <- parameter[2] * (1 / 2 * (mslp[p_grid[1, 15], p_grid[2, 15], ] +
                                 mslp[p_grid[1, 16], p_grid[2, 16], ]) -
                            1 / 2 * (mslp[p_grid[1, 8], p_grid[2, 8], ] +
                                   mslp[p_grid[1, 9], p_grid[2, 9], ]))

    ZW_2 <- parameter[3] * (1 / 2 * (mslp[p_grid[1, 8], p_grid[2, 8], ] +
                                 mslp[p_grid[1, 9], p_grid[2, 9], ])
                          - 1 / 2 * (mslp[p_grid[1, 1], p_grid[2, 1], ] +
                                  mslp[p_grid[1, 2], p_grid[2, 2], ]))

    ZW   <- ZW_1 - ZW_2

    # Southerly shear vorticity:
    # ZS = D*[1/4*(6+2*10+14)-1/4*(5+2*9+13)-1/4*(4+2*8+12)+1/4*(3+2*7+11)]

    ZS_1 <- 1 / 4 * (mslp[p_grid[1, 6], p_grid[2, 6], ] +
                   2 * mslp[p_grid[1, 10], p_grid[2, 10], ] +
                   mslp[p_grid[1, 14], p_grid[2, 14], ])

    ZS_2 <- 1 / 4 * (mslp[p_grid[1, 5], p_grid[2, 5], ] +
                   2 * mslp[p_grid[1, 9], p_grid[2, 9], ] +
                   mslp[p_grid[1, 13], p_grid[2, 13], ])

    ZS_3 <- 1 / 4 * (mslp[p_grid[1, 4], p_grid[2, 4], ] +
                   2 * mslp[p_grid[1, 8], p_grid[2, 8], ] +
                   mslp[p_grid[1, 12], p_grid[2, 12], ])

    ZS_4 <- 1 / 4 * (mslp[p_grid[1, 3], p_grid[2, 3], ] +
                   2 * mslp[p_grid[1, 7], p_grid[2, 7], ]
                 + mslp[p_grid[1, 11], p_grid[2, 11], ])

    ZS <- parameter[4] * (ZS_1 - ZS_2 - ZS_3 + ZS_4)

    # Total shear vorticity: Z = ZW+ZS
    Z <- ZW + ZS
    Z <- data.frame("value" = Z, "date" = times)

    # Gale Index
    G <- sqrt(TF$value ^ 2 + (0.5 * Z$value) ^ 2)
    G <- data.frame("value" = G, "date" = times)

    # Definition:
    # arctan(W/S) is the direction of flow. If (W>0) add 180

    # 1. Angles are in radians, not degrees. Need to convert radian into degree
    # In R, atan2(y,x)==atan(y,x)
    # The direction of low is tan-1(W/S)
    # this result would be in radian

    PI <- 4 * atan(1)

    # If W is 0 and S is 0 the atan would be NaN or -999
    theta2 <- ifelse (W$value == 0 & S$value == 0,
                     -999, atan2(W$value, S$value))

    # Convert into degree the direction of low in degrees
    theta3 <- theta2 * 180 / PI
    # check the values of W G > 0
    direction <- ifelse (W$value > 0, theta3 + 180, theta3)
    # If theta3<0 add 180
    directionflow <- ifelse (direction < 0, direction + 180, direction)
    directionflow <- data.frame("value" = directionflow, "date" = times)

    # End of indices calculation

    indices <- data.frame("date" = times, "W" = W$value, "S" = S$value,
                          "TF" = TF$value, "ZW" = ZW, "ZS" = ZS,
                          "Z" = Z$value, "D" = directionflow$value)

    # Classification  LWT (Lamb Weather Type)
    LWT_types <- calculate_cwt(Z, TF, directionflow, G, thr = 6, Gth = 30)

    # Main types:
    # directional, pure cyclo, hybrid types and Unclassified
    cwt      <- LWT_types$Total_CWT[1:4]
    cwt_gale <- LWT_types$Total_CWT[5]

    # Analysis of frequency CWTs
    # Hybrid weather types are considered each
    # with half as occurrence of directional
    # and half as (anti-)cyclonical flow

    # get main groups
    f_dir  <- cwt[[1]]
    f_vor  <- cwt[[2]]
    f_pure <- c(f_dir, f_vor)
    f_hy   <- cwt[[3]]
    f_u    <- cwt[[4]]

    # adding a counter in every day and remove if there is any empty type
    f_pure   <- lapply(f_pure, function(x) {
      if (length(x[1][[1]]) != 0) {
        x$n <- 1
        x
      }
    })

    f_hy <- Filter(function(x) dim(x)[1] > 0, f_hy)
    f_hy <- lapply(f_hy, function(x) {
      x$n <- 1
      x
    })

    f_u <- lapply(f_u, function(x) {
      if (length(x[1][[1]]) != 0) {
        x$n <- 1
        x
      }
    })

    nam_hyb <- names(f_hy)
    # call the function to get the division for each hybrid type,
    # which is counting to the basic CWTs
    hybrid_div <- list()
    temp       <- list()

    # if f_hy is NOT EMPTY reagrouping frequencies
    if (length(f_hy) == 0) {
      f_total <- c(f_pure, f_u)
    } else {
      for (i in 1:length(f_hy)) {
        temp       <- get_div(f_hy[i], nam_hyb[i])
        hybrid_div <- c(hybrid_div, temp)
      }
      # Reagrouping Hybrid types into basic types (Jones et al.1993)
      let_tot <- getname_hyb(hybrid_div)
      f_new <- f_pure

      for (i in 1:length(let_tot)) {
        f_new <- calculate_frequency(hybrid_div[[i]], f_new, let_tot[[i]])
      }

      f_total <- c(f_new, f_u)
    }

    # Merge with the times
    f_total <- lapply(f_total, add_date, totdays)
  }

  # End reagrouping types

  if (!gale) {

    return(list("CWT" = f_total, "indices" = indices))

  } else {

    f_gale <- cwt_gale$LWT_G$G
    if (length(f_gale[[1]]) != 0) {
      f_gale$n <- 1
    }

    f_total$G <- add_date(f_gale, totdays)

    return(list("CWT" = f_total, "indices" = indices))
  }
}
