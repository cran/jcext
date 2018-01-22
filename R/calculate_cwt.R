#' @title calculate_cwt
#'
#' @description Applies the rules to obtain the weather types.
#'              (more details, see Jones et al. 1997)
#'
#' @param Z               Data frame with the total shear vorticity and dates.
#' @param TF              Data frame with resultant flow and dates.
#' @param directionflow   Data frame with the direction of the flow and dates.
#' @param G               Data frame with Gale days and dates.
#' @param thr             Numeric threshold used for Unclassified days.
#' @param Gth             Numeric threshold used for Gale days.
#' @details As defined in the original scheme, the threshold to determine unclassified days is 6.
#'          Gale days are estimated by using a threshold of 30. If Gale days with a greater
#'          intensity (e.g. 40 or 50) is wanted, Gth must be changed.
#'          The airflow indices are used within the following rules
#'          to define the appropriate Lamb weather types.
#' \itemize{
#' \item  The appropriate direction is calculated on an eight-point compass allowing 45° per sector.
#' \item  If abs(Z) is less than TF, flow is essentially pure directional type.
#' \item  If |Z| is greater than 2TF, then the pattern is strongly cyclonic (Z > 0) or anticyclonic (Z < 0).
#' \item  If |Z| lies between TF and 2TF then the flow is partly (anti-) cyclonic and this corresponds to one
#'        of Lamb’s synoptic/direction hybrid types, e.g. AE.
#' \item  If TF is less than 6 and |Z| is less than 6, there is light indeterminate flow corresponding to Lamb’s
#'        unclassified type U.
#'        }
#' @return A list with two objects:
#' \itemize{
#' \item Total_CWT with five groups of weather types: LWT_D, LWT_C, LWT_CH, LWT_U and LWT_G
#'       The main four groups contain the total of 27 weather types and the five list LWT_G refers to the gale days.
#'       \itemize{
#'       \item LWT_D:  8 directional types.
#'       \item LWT_C:  1 anticyclonic and  1 cyclonic.
#'       \item LWT_CH: 16 hybrid types.
#'       \item LWT_U: 1 unclassified type.
#'       }
#'\item Logical values of daily occurrence of each type.
#'      The user might want to use these values to get composites with specific atmospheric fields (e.g. pressure, temperature).
#'}
#'
#'@seealso
#'\code{\link{classification_jc}}
#'
#' @export
calculate_cwt <- function(Z, TF, directionflow, G, thr = 6, Gth = 30){

  # Function calculate_cwt:
  # main function with the rules to do the CWT classification
  # (Jones et al.1993)
  # Input arguments: Indices
  # Output arguments: A list with 2 list:
  # 1.list: Weather types (data frame structures, with Z values and date)
  # 2.list: Logical values with every CWTs


  # appropiate direction is calculated on an eight-point compass.
  # There will be eigth directional types

  NE <- (directionflow$value > 22.5 & directionflow$value < 67.5)
  E  <- (directionflow$value >= 67.5 & directionflow$value <= 112.5)
  SE <- (directionflow$value > 112.5 & directionflow$value < 157.5)
  S  <- (directionflow$value >= 157.5 & directionflow$value <= 202.5)
  SW <- (directionflow$value > 202.5 & directionflow$value < 247.5)
  W  <- (directionflow$value >= 247.5 & directionflow$value <= 292.5)
  NW <- (directionflow$value > 292.5 & directionflow$value < 337.5)
  N  <- (directionflow$value >= 337.5 | directionflow$value <= 22.5)

  # get pure and non pure types
  # If TF is greater than thr=6 (original threshold)
  # or Z is greater than 6 then we classify the types
  # Basic condition
  clas_condition <- (TF$value > thr | abs(Z$value) > thr)

  # Gale days:
  # threshold values G>30, G>30 for low wind G<10,G<5
  # Gale condition Gth =30 This can be changed.
  gale_l <- (G$value > Gth)
  gale <- data.frame("date" = Z$date[gale_l])

  # Pure condition: corresponds to Lamb’s pure cyclonic and anticyclonic
  temp_purecyclo <- (clas_condition & (abs(Z$value) > 2 * TF$value))

  cyclo_l <- (temp_purecyclo & (Z$value > 0))
  cyclo   <- data.frame("date" = Z$date[cyclo_l])
  # Merging Gale days with cyclo character
  C_Ga <- merge(cyclo, gale, by = "date", all.x = TRUE)

  anticyclo_l <- (temp_purecyclo & (Z$value < 0))
  anticyclo   <- data.frame("date" = Z$date[anticyclo_l])
  # Merging Gale days with anticyclo character
  A_Ga <- merge(anticyclo, gale, by = "date", all.x = TRUE)

  # Condition: Directional types

  # To get the right directional, it is calculated the direction
  # If abs(Z) is less than TF, flow is essentially straight.
  # It would correspond to pure directional
  condition_puredirectional <- (clas_condition & (abs(Z$value) < TF$value))

  N_l <- (condition_puredirectional & N)
  D_N <- data.frame("date" = Z$date[N_l])
  # Merging Gale days with N directional character
  N_Ga <- merge(D_N, gale, by = "date", all.x = TRUE)

  NE_l <- (condition_puredirectional & NE)
  D_NE <- data.frame("date" = Z$date[NE_l])
  # Merging Gale days with NE directional character
  NE_Ga <- merge(D_NE, gale, by = "date", all.x = TRUE)

  E_l <- (condition_puredirectional & E)
  D_E <- data.frame("date" = Z$date[E_l])
  # Merging Gale days with NE directional character
  E_Ga <- merge(D_E, gale, by = "date", all.x = TRUE)

  SE_l <- (condition_puredirectional & SE)
  D_SE <- data.frame("date" = Z$date[SE_l])
  # Merging Gale days with NE directional character
  SE_Ga <- merge(D_SE, gale, by = "date", all.x = TRUE)

  S_l <- (condition_puredirectional & S)
  D_S <- data.frame("date" = Z$date[S_l])
  # Merging Gale days with South directional character
  S_Ga <- merge(D_S, gale, by = "date", all.x = TRUE)

  SW_l <- (condition_puredirectional & SW)
  D_SW <- data.frame("date" = Z$date[SW_l])
  # Merging Gale days with SW directional character
  SW_Ga <- merge(D_SW, gale, by = "date", all.x = TRUE)

  W_l <- (condition_puredirectional & W)
  D_W <- data.frame("date" = Z$date[W_l])
  # Merging Gale days with W directional character
  W_Ga <- merge(D_W, gale, by = "date", all.x = TRUE)

  NW_l <- (condition_puredirectional & NW)
  D_NW <- data.frame("date" = Z$date[NW_l])
  # Merging Gale days with NW directional character
  NW_Ga <- merge(D_NW, gale, by = "date", all.x = TRUE)

  # Condition: Hybrid type classification
  # If F < |Z| < 2*F , then it's hybrid type
  condition_hybrid <- (clas_condition &
                       (TF$value < abs(Z$value) &
                        abs(Z$value) < 2 * TF$value ))

  hibrid_cyclo     <- (condition_hybrid & (Z$value >= 0))
  hibrid_anticyclo <- (condition_hybrid & (Z$value < 0))

  # Classification into hybrid directional types
  # Hybrid Cyclonic types:
  hyb_CN_l <- (hibrid_cyclo & N)
  hyb_CN   <- data.frame("date" = Z$date[hyb_CN_l])
  # Merging Gale days with hybrid flow character, cyclonic N
  CN_Ga <- merge(hyb_CN, gale, by = "date", all.x = TRUE)

  hyb_CNE_l <- (hibrid_cyclo & NE)
  hyb_CNE   <- data.frame("date" = Z$date[hyb_CNE_l])
  # Merging Gale days with hybrid flow character, cyclonic NE
  CNE_Ga <- merge(hyb_CNE, gale, by = "date", all.x = TRUE)

  hyb_CE_l <- (hibrid_cyclo & E)
  hyb_CE   <- data.frame("date" = Z$date[hyb_CE_l])
  # Merging Gale days with hybrid flow character, cyclonic E
  CE_Ga <- merge(hyb_CE, gale, by = "date", all.x = TRUE)

  hyb_CSE_l <- (hibrid_cyclo & SE)
  hyb_CSE   <- data.frame("date" = Z$date[hyb_CSE_l])
  # Merging Gale days with hybrid flow character, cyclonic SE
  CSE_Ga <- merge(hyb_CSE, gale, by = "date", all.x = TRUE)

  hyb_CS_l <- (hibrid_cyclo & S)
  hyb_CS   <- data.frame("date" = Z$date[hyb_CS_l])
  # Merging Gale days with hybrid flow character, cyclonic S
  CS_Ga <- merge(hyb_CS, gale, by = "date", all.x = TRUE)

  hyb_CSW_l <- (hibrid_cyclo & SW)
  hyb_CSW   <- data.frame("date" = Z$date[hyb_CSW_l])
  # Merging Gale days with hybrid flow character, cyclonic SW
  CSW_Ga <- merge(hyb_CSW, gale, by = "date", all.x = TRUE)

  hyb_CW_l <- (hibrid_cyclo & W)
  hyb_CW   <- data.frame("date" = Z$date[hyb_CW_l])
  # Merging Gale days with hybrid flow character, cyclonic W
  CW_Ga <- merge(hyb_CW, gale, by = "date", all.x = TRUE)

  hyb_CNW_l <- (hibrid_cyclo & NW)
  hyb_CNW   <- data.frame("date" = Z$date[hyb_CNW_l])
  # Merging Gale days with hybrid flow character, cyclonic NW
  CNW_Ga <- merge(hyb_CNW, gale, by = "date", all.x = TRUE)

  # Hybrid Anticyclonic types
  hyb_AN_l <- (hibrid_anticyclo & N)
  hyb_AN   <- data.frame("date" = Z$date[hyb_AN_l])
  # Merging Gale days with hybrid flow character, anticyclonic N
  AN_Ga <- merge(hyb_AN, gale, by = "date", all.x = TRUE)

  hyb_ANE_l <- (hibrid_anticyclo & NE)
  hyb_ANE   <- data.frame("date" = Z$date[hyb_ANE_l])
  # Merging Gale days with hybrid flow character, anticyclonic NE
  ANE_Ga <- merge(hyb_ANE, gale, by = "date", all.x = TRUE)

  hyb_AE_l <- (hibrid_anticyclo & E)
  hyb_AE   <- data.frame("date" = Z$date[hyb_AE_l])
  # Merging Gale days with hybrid flow character, anticyclonic E
  AE_Ga <- merge(hyb_AE, gale, by = "date", all.x = TRUE)

  hyb_ASE_l <- (hibrid_anticyclo & SE)
  hyb_ASE   <- data.frame("date" = Z$date[hyb_ASE_l])
  # Merging Gale days with hybrid flow character, anticyclonic SE
  ASE_Ga <- merge(hyb_ASE, gale, by = "date", all.x = TRUE)

  hyb_AS_l <- (hibrid_anticyclo & S)
  hyb_AS   <- data.frame("date" = Z$date[hyb_AS_l])
  # Merging Gale days with hybrid flow character, anticyclonic S
  AS_Ga <- merge(hyb_AS, gale, by = "date", all.x = TRUE)

  hyb_ASW_l <- (hibrid_anticyclo & SW)
  hyb_ASW   <- data.frame("date" = Z$date[hyb_ASW_l])
  # Merging Gale days with hybrid flow character, anticyclonic SW
  ASW_Ga <- merge(hyb_ASW, gale, by = "date", all.x = TRUE)

  hyb_AW_l <- (hibrid_anticyclo & W)
  hyb_AW   <- data.frame("date" = Z$date[hyb_AW_l])
  # Merging Gale days with hybrid flow character, anticyclonic W
  AW_Ga <- merge(hyb_AW, gale, by = "date", all.x = TRUE)

  hyb_ANW_l <- (hibrid_anticyclo & NW)
  hyb_ANW   <- data.frame("date" = Z$date[hyb_ANW_l])
  # Merging Gale days with hybrid flow character, anticyclonic NW
  ANW_Ga <- merge(hyb_ANW, gale, by = "date", all.x = TRUE)

  # Condition: Unclassified types
  # condition_unclassified_l (flow and vorticity <6)
  # Using other threshold for undefined days
  condition_unclassified_l <- (TF$value < thr & abs(Z$value) < thr)
  unclassified <- data.frame("date" = Z$date[condition_unclassified_l])
  # Gale days with U
  U_Ga <- merge(unclassified, gale, by = "date", all.x = TRUE)

  # End Classification

  # Returns a list with all weather types
  LWT_D <- list("N" = N_Ga,
                "NE" = NE_Ga,
                "E" = E_Ga,
                "SE" = SE_Ga,
                "S" = S_Ga,
                "SW" = SW_Ga,
                "W" = W_Ga,
                "NW" = NW_Ga)

  LWT_C <- list("A" = A_Ga,
                "C" = C_Ga)

  LWT_CH <- list("CN" = CN_Ga,
                 "CNE" = CNE_Ga,
                 "CE" = CE_Ga,
                 "CSE" = CSE_Ga,
                 "CS" = CS_Ga,
                 "CSW" = CSW_Ga,
                 "CW" = CW_Ga,
                 "CNW" = CNW_Ga,
                 "AN" = AN_Ga,
                 "ANE" = ANE_Ga,
                 "AE" = AE_Ga,
                 "ASE" = ASE_Ga,
                 "AS" = AS_Ga,
                 "ASW" = ASW_Ga,
                 "AW" = AW_Ga,
                 "ANW" = ANW_Ga)

  LWT_U <- list("U" = U_Ga)

  LWT_G <- list("G" = gale)

  logical_vals <- list("N_l" = N_l,
                       "NE_l" = NE_l,
                       "E_l" = E_l,
                       "SE_l" = SE_l,
                       "S_l" = S_l,
                       "SW_l" = SW_l,
                       "W_l" = W_l,
                       "NW_l" = NW_l,
                       "cyclo_l" = cyclo_l,
                       "anticyclo_l" = anticyclo_l,
                       "hyb_CN_l" = hyb_CN_l,
                       "hyb_CNE_l" = hyb_CNE_l,
                       "hyb_CE_l" = hyb_CE_l,
                       "hyb_CSE_l" = hyb_CSE_l,
                       "hyb_CS_l" = hyb_CS_l,
                       "hyb_CSW_l" = hyb_CSW_l,
                       "hyb_CW_l" = hyb_CW_l,
                       "hyb_CNW_l" = hyb_CNW_l,
                       "hyb_AN_l" = hyb_AN_l,
                       "hyb_ANE_l" = hyb_ANE_l,
                       "hyb_AE_l" = hyb_AE_l,
                       "hyb_ASE_l" = hyb_ASE_l,
                       "hyb_AS_l" = hyb_AS_l,
                       "hyb_ASW_l" = hyb_ASW_l,
                       "hyb_AW_l" = hyb_AW_l,
                       "hyb_ANW_l" = hyb_ANW_l
  )

  CWT  <- list("LWT_D" = LWT_D,
               "LWT_C" = LWT_C,
               "LWT_CH" = LWT_CH,
               "LWT_U" = LWT_U,
               "LWT_G" = LWT_G)

  list <- list("Total_CWT" = CWT,
              "logical_days" = logical_vals)
}
