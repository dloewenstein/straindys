#' Finds peaks and valleys in the strain dataframe
#'
#' @param straindata dataframe in longformat with separate column for each strain segment
#' @param position.firstStraincolumn the index position of the first strain column
#'
#' @return A dataframe with 2 new columns added for each strain segment indicating
#' whether the corresponding strain value is a peak/valley or not
#' @export
#'
#' @examples findStrainPeaks(straindata, position.firstStrainColumn = 3)
findStrainPeaks <- function (straindata, position.firstStraincolumn) {

  endrow <- c(NA, rep(0, ncol(straindata)-1))

  straindata <- rbind(straindata, endrow)

  strain_segments <- names(straindata[position.firstStraincolumn : ncol(straindata)])

  strain_segments <- str_extract(strain_segments, "(?:(?!_).)*")

  for (i in seq_along(strain_segments)) {
          strain_peak <- paste(strain_segments[i], "peak", sep = "_")

          strain_valley <- paste(strain_segments[i], "valley", sep = "_")

          strain_value <- paste(strain_segments[i], "strain", sep = "_")

          find_strain_valley <- interp(~ a %in% a[findValleys(a)-1],

                                       a = as_name(strain_value))

          find_strain_peak   <- interp(~ a %in% a[findPeaks(a)-1],

                                       a = as_name(strain_value))

          set_valley_false_time_zero <- interp(~ ifelse(time == 0,
                                                        FALSE,
                                                        a),

                                               a = as_name(strain_valley))

          set_peak_true_time_zero <- interp(~ ifelse(time == 0,
                                                      TRUE,
                                                     a),

                                            a = as_name(strain_peak))

          straindata <- straindata %>%
                  mutate_(.dots = setNames(list(find_strain_valley,

                                                find_strain_peak,

                                                set_valley_false_time_zero,

                                                set_peak_true_time_zero),

                                           list(strain_valley,

                                                strain_peak,

                                                strain_valley,

                                                strain_peak)))
  }


return(straindata)

  }
