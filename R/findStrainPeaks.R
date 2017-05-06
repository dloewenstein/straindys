
#' Find strain peaks using percentage criteria for circumferential/longitudinal strain
#'
#' @param x dataframe containing the strain columns in a long format.
#' @param strain the strain variable you want to mutate, written inside "".
#' @param thresh percentage of the peak strain value that a valley has to reach to be
#' considered a true valley.
#' @param incr percentage of the peak strain value by which the strain increase between valley and the
#' following peak has to reach.
#'
#' @return A dataframe with id and a straincolumn containing the time for the first peak
#' fulfilling the criteria
#' @export
#'
#' @examples
#' findStrainPeaks(strain_data, "mas", thresh = 0.2, incr = -0.3)

findStrainPeaks <- function (x, strain, thresh, incr) {

  if (incr > 0)
    stop("increase has to be a negative percentage expressed in decimals")

  if (thresh < 0)
    stop("thresh has to a positive percentage expressed in decimals")

  if (!is.data.frame(x))
    stop("x has to be of class dataframe")

# calculate the difference between a strain peak or valley and the
# following element, since a valley can only be followed immediately
# by a peak this ensures that each valley is compared to the first following peak.


# creates variable names from the strain argument

  peakvar <- paste(strain, "peak", sep = ".")
  valleyvar <- paste(strain, "valley", sep = ".")
  strainvar <- paste(strain, "strain", sep = ".")
  timevar <- paste(strain, "time", sep = ".")

# creates variable calls

  peak <- interp(~ a, a = as.name(peakvar))
  valley <- interp(~ b, b = as.name(valleyvar))
  strain <- interp(~ c, c = as.name(strainvar))
  straintime <- interp(~ a, a = as.name(timevar))

# filters for all rows were peak or valley for the segment is true

  peakvalley_true <- list(interp(~ a | b == TRUE, a = as.name(peakvar), b = as.name(valleyvar)))

# calculates the difference in strain between the current value and the following element and prints result to new column called diff
# adds NA in the end to keep correct vector lenght, this value will be given to the previous created dummy row.

  calculate_straindiff <- list(diff = interp(~ append(diff(c), values = NA),
                                             c = as.name(strainvar)))


# filter for all rows were valley is true AND strain <= the threshold * the min strain value AND diff >= the increase * min strain value.
  true_valleys <- list(interp(~ (a == TRUE &
                                 b <= (thresh*min(b)) &
                                 diff >= (incr*min(b))),

                              a = as.name(valleyvar),
                              b = as.name(strainvar)))

# filters for the earliest peak fulfilling previous criteria.
  earliest_valleys <- list(interp(~ a == min(a), a = as.name(timevar)))

x <-  x %>%
          filter_(.dots = peakvalley_true) %>%

          mutate_(.dots = calculate_straindiff) %>%

          group_by(id)  %>%

          filter_(.dots = true_valleys) %>%

          rename_(.dots = setNames("time", timevar)) %>%

          filter_(.dots = earliest_valleys) %>%

          select_(.dots = list("id", straintime)) %>%

          ungroup()

  return(x)
}
