#' Calculate time to peak strain
#'
#' @param data A \code{data.frame}
#' @param strain Name of strain column, wrapped in "".
#' @param time Name of time column, wrapped in "".
#' @param thresh Optional, threshold that the strain peak has to pass.
#' @param type Type of strain, circumferential or radial.
#' @param incr Optional, difference between the sought after peak and the following peak of opposite direction.
#' @param peak.criteria Which criteria to use for peak.
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
#'
calc_time_to_strain_peak <- function(data, strain, time, thresh = 0, incr = 0,
                                     type = c("circumferential", "radial"),
                                     peak.criteria = c("units", "percent"),
                                     ...){
    type <- match.arg(type)
    peak.criteria <- match.arg(peak.criteria)

  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("rlang package needs to be installed")
  }

  strainN <- as.name(strain)

  data <- data[, c(time, strain)]

  find_valleys <- function(x, thresh = 0) {
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) > 0) + 2
    if (!missing(thresh)) {
      if (sign(thresh) > 0)
        thresh <- -thresh
      pks[x[pks - 1] - zoo::coredata(x[pks]) < thresh]
    }
    else pks
  }

  find_peaks <- function(x, thresh = 0) {
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 0) + 2
    if (!missing(thresh)) {
      if (sign(thresh) < 0)
        thresh <- -thresh
      pks[x[pks - 1] - zoo::coredata(x[pks]) > thresh]
    }
    else pks
  }

  if (type == "circumferential") {
      endrow <- setNames(data.frame(NA, 0, FALSE, TRUE), c(time, strain, "valley", "peak"))
  }
  if (type == "radial") {
      endrow <- setNames(data.frame(NA, 0, TRUE, FALSE), c(time, strain, "valley", "peak"))
  }

    valleys_index <- find_valleys(data[[strain]]) - 1

    peaks_index <- find_peaks(data[[strain]]) - 1

    data <- cbind(data, data.frame(valley = FALSE, peak = FALSE))

    data <- rbind(data, endrow)

    data[valleys_index, "valley"] <- TRUE

    data[peaks_index, "peak"] <- TRUE

    data <- data[sort(c(peaks_index, valleys_index, nrow(data))), ]

    strain_diff <- diff(data[[strain]])

    strain_diff <- append(strain_diff, NA)

    data <- cbind(data, strain_diff)

    if (type == "circumferential") {
        if (peak.criteria == "units") {
            data <- data %>%
                dplyr::filter((!!strainN) <= thresh &
                                  .data$strain_diff >= incr &
                                  .data$valley == TRUE) %>%
                dplyr::slice(1)
        }

        if (peak.criteria == "percent") {

            .MIN_STRAIN <- min(data[[strain]], na.rm = TRUE)

            .PERC_THRESH <- .MIN_STRAIN * thresh
            .PERC_INCR   <- abs(.MIN_STRAIN) * incr

            data <- data %>%
                dplyr::filter((!!strainN) <= .PERC_THRESH &
                                  .data$strain_diff >= .PERC_INCR &
                                  .data$valley == TRUE) %>%
                dplyr::slice(1)
        }
    }

    if (type == "radial") {
        if (peak.criteria == "units") {
            data <- data %>%
                dplyr::filter((!!strainN) >= thresh &
                                  .data$strain_diff <= incr &
                                  .data$peak == TRUE) %>%
                dplyr::slice(1)
        }

        if (peak.criteria == "percent") {

            .MAX_STRAIN <- max(data[[strain]], na.rm = TRUE)

            .PERC_THRESH <- .MAX_STRAIN * thresh
            .PERC_INCR   <- -(.MAX_STRAIN) * incr

            data <- data %>%
                dplyr::filter((!!strainN) >= .PERC_THRESH &
                                  .data$strain_diff <= .PERC_INCR &
                                  .data$peak == TRUE) %>%
                dplyr::slice(1)
        }
    }

  if (length(data[[time]]) == 0) {
    return(NA)
  } else{
    return(data[[time]])
  }
}
