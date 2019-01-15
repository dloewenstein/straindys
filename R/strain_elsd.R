#' Earliest-to-latest segment delay
#'
#' Time delay between earliest and latest contracting left ventricular wall
#' segments
#'
#' Non-uniform contraction of the left ventricular wall can be quantified by
#' means of dyssynchrony analysis. It is known that in case of left bundle branch
#' block, the septum normally contracts before the lateral free wall. However,
#' this is not always the case and therefore measureing the delay between earliest
#' and latest segment, independent of location might better characterize the
#' pattern of dyssynchrony.
#'
#' @inheritParams strain_cure
#' @param subset_pattern Regex pattern for subsetting over which columns to perform
#' analysis.
#'
#' @return
#' @export
#'
#' @examples
strain_elsd <- function(data, subset_pattern = NULL){
    data_long <- tidyr::gather(data, strain, time)

    if(is.null(subset_pattern)){
        ELSD <- max(unlist(data_long$time), na.rm = TRUE) -
            min(unlist(data_long$time), na.rm = TRUE)
    } else{
        data_long_subset <- subset(data_long, stringr::str_detect(strain, subset_pattern))

        ELSD <- max(unlist(data_long_subset$time), na.rm = TRUE) -
            min(unlist(data_long_subset$time), na.rm = TRUE)
    }

    if(ELSD == -Inf){
        return(NA)} else{
            return(ELSD)
        }
}
