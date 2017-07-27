#' Calculate time to peak strain
#'
#' @param data A dataframe.
#' @param strain Name of strain column, wrapped in "".
#' @param time Name of time column, wrapped in "".
#' @param thresh Optional, threshold that the strain peak has to pass.
#' @param incr Optional, difference between the sought after peak and the following peak of opposite direction.
#' @param peak.criteria Which criteria to use for peak.
#' @importFrom rlang !!
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
calc_time_to_strain_peak <- function(data, strain, time, thresh = 0, incr = 0, peak.criteria = "first_true"){

  if(!requireNamespace("rlang", quietly = TRUE)){
    stop("rlang package needs to be installed")
  }

  strainN <- as.name(strain)

  data <- data[, c(time, strain)]

  find_valleys <- function (x, thresh = 0) {
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) >
                   0) + 2
    if (!missing(thresh)) {
      if (sign(thresh) > 0)
        thresh <- -thresh
      pks[x[pks - 1] - zoo::coredata(x[pks]) < thresh]
    }
    else pks
  }

  find_peaks <- function (x, thresh = 0) {
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) <
                   0) + 2
    if (!missing(thresh)) {
      if (sign(thresh) < 0)
        thresh <- -thresh
      pks[x[pks - 1] - zoo::coredata(x[pks]) > thresh]
    }
    else pks
  }

  if(peak.criteria == "first_true"){
    endrow <- setNames(data.frame(NA, 0, FALSE, TRUE), c(time, strain, "valley", "peak"))

    valleys_index <- find_valleys(data[[strain]])-1

    peaks_index <- find_peaks(data[[strain]])-1

    data <- cbind(data, data.frame(valley = FALSE, peak = FALSE))

    data <- rbind(data, endrow)

    data[valleys_index, "valley"] <- TRUE

    data[peaks_index, "peak"] <- TRUE

    data <- data[c(valleys_index, peaks_index, nrow(data)), ]

    strain_diff <- diff(data[[strain]])

    strain_diff <- append(strain_diff, NA)

    data <- cbind(data, strain_diff)

    data <- data %>%
      dplyr::filter((!!strainN) < thresh &
                      .data$strain_diff >= incr &
                      .data$valley == TRUE) %>%
      dplyr::slice(1)
  } else {
    stop(print("peak.criteria has not been correctly specified, as of now first_true is the only criteria supported"), call. = FALSE)
  }

  return(data[[time]])
}
