
#' Find time to true strain peaks using percentage criteria for circumferential/longitudinal strain
#'
#' @param x dataframe containing the strain columns in a long format.
#' @param strain the strain variable you want to mutate, written inside "".
#' @param thresh percentage of the peak strain value that a valley has to reach to be
#' considered a true valley.
#' @param incr percentage of the peak strain value by which the strain increase between valley and the
#' following peak has to reach.
#' @param position.firstStrainColumn the index position for the first strain segment in the dataframe
#'
#' @return A dataframe with id and a straincolumn containing the time for the first peak
#' fulfilling the criteria
#'
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#'
timeFirstPeak <- function (x, strain, thresh, incr, position.firstStrainColumn) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install/load it.",
         call. = FALSE)
  }


  if (!requireNamespace("lazyeval", quietly = TRUE)) {
    stop("lazyeval needed for this function to work. Please install/load it.",
         call. = FALSE)
  }

  if (!incr <= 0)
    stop("increase has to be 0 or a negative percentage expressed in decimals")

  if (!thresh >= 0)
    stop("thresh has to be 0 pr a positive percentage expressed in decimals")

  if (!is.data.frame(x))
    stop("x has to be of class dataframe")



  x <- straindys::findStrainPeaks(x, position.firstStrainColumn)

# calculate the difference between a strain peak or valley and the
# following element, since a valley can only be followed immediately
# by a peak this ensures that each valley is compared to the first following peak.


# creates variable names from the strain argument

  peakvar <- paste(strain, "peak", sep = "_")
  valleyvar <- paste(strain, "valley", sep = "_")
  strainvar <- paste(strain, "strain", sep = "_")
  timevar <- paste(strain, "time", sep = "_")

# creates variable calls

  peak <- lazyeval::interp(~ a, a = as.name(peakvar))
  valley <- lazyeval::interp(~ b, b = as.name(valleyvar))
  strain <- lazyeval::interp(~ c, c = as.name(strainvar))
  straintime <- lazyeval::interp(~ a, a = as.name(timevar))

# filters for all rows were peak or valley for the segment is true

  peakvalley_true <- list(lazyeval::interp(~ a | b == TRUE, a = as.name(peakvar), b = as.name(valleyvar)))

# calculates the difference in strain between the current value and the following element and prints result to new column called diff
# adds NA in the end to keep correct vector lenght, this value will be given to the previous created dummy row.

  calculate_straindiff <- list(diff = lazyeval::interp(~ append(diff(c), values = NA),
                                             c = as.name(strainvar)))


# filter for all rows were valley is true AND strain <= the threshold * the min strain value AND diff >= the increase * min strain value.
  true_valleys <- list(lazyeval::interp(~ (a == TRUE &
                                 b <= (thresh*min(b)) &
                                 diff >= (incr*min(b))),

                              a = as.name(valleyvar),
                              b = as.name(strainvar)))

# filters for the earliest peak fulfilling previous criteria.
  earliest_valleys <- list(lazyeval::interp(~ a == min(a), a = as.name(timevar)))

x <-  x %>%
  dplyr::filter_(.dots = peakvalley_true) %>%
  dplyr::mutate_(.dots = calculate_straindiff) %>%
  dplyr::group_by(id)  %>%
  dplyr::filter_(.dots = true_valleys) %>%
  dplyr::rename_(.dots = setNames("time", timevar)) %>%
  dplyr::select_(.dots = list("id", straintime)) %>%
  dplyr::ungroup()

  return(x)
}
