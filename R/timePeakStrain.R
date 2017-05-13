
#' Find the time corresponding to the peak strain value for a segment
#'
#' @param data Dataframe with each strain segment in a separate column, the name
#' of the strain columns must include "strain".
#' @param id.column Name of the id column in quotes.
#' @param time.column Name of the time column in quotes.
#' @param RR.as.perc Logical parameter, if TRUE time will be presented as percent
#' of heart cycle.
#' @param radial.strain Logical parameter, set to TRUE if using radial strain.
#'
#' @return A dataframe with time to peak strain value for each patient and all
#' segments.
#' @export
#'
#' @examples timeMinStrain(strain_data, "id", "time", RR.as.perc = TRUE)

timePeakStrain <- function (data, id.column, time.column, RR.as.perc = TRUE, radial.strain = FALSE) {

  require(dplyr)

  require(lazyeval)

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install/load it.",
         call. = FALSE)
  }


  if (!requireNamespace("lazyeval", quietly = TRUE)) {
    stop("lazyeval needed for this function to work. Please install/load it.",
         call. = FALSE)
  }

  id_variable <- interp(~ a, a = as_name(id.column))

  time_variable <- interp(~a, a = as_name(time.column))

  time_as_rr_perc <- interp(~ a/max(a), a = as_name(time.column))

  if(RR.as.perc) {

    data <- data %>%
      group_by_(id_variable) %>%
      mutate_(.dots = setNames(list(time_as_rr_perc), c(time.column))) %>%
      ungroup()
  }

  if (!radial.strain) {
  data <- data %>%

    rename_(.dots = setNames(list(time_variable), c("time"))) %>%

    group_by_(id_variable) %>%

    mutate_at(.cols = vars(contains("strain")), .funs = funs(ifelse(. == min(.) & . <0, time, NA))) %>%

    summarise_at(.cols = vars(contains("strain")), min, na.rm = TRUE) %>%

    ungroup()

  } else {

    data <- data %>%

      rename_(.dots = setNames(list(time_variable), c("time"))) %>%

      group_by_(id_variable) %>%

      mutate_at(.cols = vars(contains("strain")), .funs = funs(ifelse(. == max(.) & . > 0, time, NA))) %>%

      summarise_at(.cols = vars(contains("strain")), min, na.rm = TRUE) %>%

      ungroup()
  }


  return(data)
}
