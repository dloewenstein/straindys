#' Calculate Earliest to Latest Segment Delay
#' @description Given a dataframe with columns for each segment, each containing time
#' to peak contraction, this function converts the data to long format and calculates
#' the difference in time between peak contraction in the earliest and latest segment.
#' @param data Dataframe with one column for each segment.
#' @param subset.pattern Optional, if e.g dataframe holds data for different slices, eg. basal or midventricular.
#'
#' @return Numeric
#' @export
#'
#' @examples
#'
calc_ELSD <- function(data, subset.pattern = NULL){

  if(!requireNamespace("tidyr", call. = FALSE, quietly = TRUE)){
    stop("Package tidyr required")
  }

  data_long <- tidyr::gather(data, strain, time)

  if(is.null(subset.pattern)){
    ELSD <- max(unlist(data_long$time), na.rm = TRUE) -
      min(unlist(data_long$time), na.rm = TRUE)
  } else{
    data_long_subset <- subset(data_long, stringr::str_detect(strain, subset.pattern))

    ELSD <- max(unlist(data_long_subset$time), na.rm = TRUE) -
      min(unlist(data_long_subset$time), na.rm = TRUE)
  }

  if(ELSD == -Inf){
    return(NA)} else{
      return(ELSD)
    }
}
