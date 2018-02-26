
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
#'
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#'
timePeakStrain <- function (data, id.column, time.column, RR.as.perc = TRUE, radial.strain = FALSE) {


  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install/load it.",
         call. = FALSE)
  }


  if (!requireNamespace("lazyeval", quietly = TRUE)) {
    stop("lazyeval needed for this function to work. Please install/load it.",
         call. = FALSE)
  }

  id_variable <- lazyeval::interp(~ a, a = as_name(id.column))

  time_variable <- lazyeval::interp(~a, a = as_name(time.column))

  time_as_rr_perc <- lazyeval::interp(~ a/max(a), a = as_name(time.column))

  removeInfinite <- function(x) {
    x[which(is.infinite(x))] <- NA

    return(x)
  }

  if(RR.as.perc) {

    data <- data %>%
      dplyr::group_by_(id_variable) %>%
      dplyr::mutate_(.dots = setNames(list(time_as_rr_perc), c(time.column))) %>%
      dplyr::ungroup()
  }

  if (!radial.strain) {
    data <- data %>%

      dplyr::rename_(.dots = setNames(list(time_variable), c("time"))) %>%

      dplyr::group_by_(id_variable) %>%

      dplyr::mutate_at(.vars = vars(contains("strain")),
                .funs = funs(ifelse(. == min(.) & . < 0,
                                    time,
                                    NA))) %>%

      dplyr::summarise_at(.vars = vars(contains("strain")),
                   min,
                   na.rm = TRUE) %>%

      dplyr::mutate_at(.vars = vars(contains("strain")),
                .funs = funs(removeInfinite(.))) %>%

      dplyr::ungroup()

  } else {

    data <- data %>%

      dplyr::rename_(.dots = setNames(list(time_variable), c("time"))) %>%

      dplyr::group_by_(id_variable) %>%

      dplyr::mutate_at(.vars = vars(contains("strain")),
                .funs = funs(ifelse(. == max(.) & . > 0,
                                    time,
                                    NA))) %>%

      dplyr::summarise_at(.vars = vars(contains("strain")),
                   min,
                   na.rm = TRUE) %>%

      dplyr::mutate_at(.vars = vars(contains("strain")),
                .funs = funs(removeInfinite(.))) %>%

      dplyr::ungroup()
  }


  return(data)
}
