
#' Find the time corresponding to the minimum strain value for a segment
#'
#' @param x Dataframe with strain data in a long format, one column for each
#' segment.
#' @param segment Name of the strain segment.
#'
#' @return A dataframe with time to minimum strain value for each patient and
#' the selected strain segment
#' @export
#'
#' @examples timeMinStrain(strain_data, "mas")

timeMinStrain <- function (x, segment) {

  # Uses the argument strain to create variables.

  strain_time <- paste(segment, "time", sep = "_")

  strain_value <- paste(segment, "strain", sep = "_")

  # Selects the strain variable.

  select_segment <- interp(~ a,
                           a = as.name(strain_value))

  # Filter for minimum strain.

  find_min_strain <- interp(~ a == min(a),
                            a = as.name(strain_value))

  # If strain value equals to or above 0, than corresponding time value gets NA

  na_time_positive_strain <- interp(~ ifelse(a >= 0, NA, time),
                                        a = as.name(strain_value))

 x <- x %>%

   select_(.dots = list(~ id, ~ time, select_segment)) %>%

   group_by(id) %>%

   filter_(.dots = list(find_min_strain)) %>%

   mutate_(.dots = list(time = na_time_positive_strain)) %>%

   rename_(.dots = setNames(list(~time), list(strain_time)))

 return(x)
  }
