#' Circumferential uniformity ratio estimate
#'
#' Circumferential uniformity ratio estimate for dyssynchrony analysis of
#' cardiac strain.
#'
#' @section Background:
#' Circumferential uniformity ratio estimate (cure) is a measure typically used
#' for analysis of left ventricular (LV) mechanical dyssynchrony. Ranging from 0,
#' meaning complete dyssynchrony, that is, all opposing wall segments in LV move
#' in different directions. And 1 meaning complete synchrony, all walls contract
#' and shortens at the same time. Cure \eqn{\le 0.75} has previously been used to
#' define dyssynchrony.
#'
#' @param data Dataframe, columns containing strainvalues from LV wall segments
#' according to the AHA-17 segment model.
#'
#' @return
#' @export
#'
#' @examples
strain_cure <- function(data) {

    strain_columns <- which(grepl(".*strain", colnames(data)))
    strain_start <- strain_columns[1]
    strain_end <- strain_columns[length(strain_columns)]
    data$cure <- vector("numeric", nrow(data))
    for (i in 1:nrow(data)) {
        data$cure[i] <- .calc_cure(as.numeric(data[i, strain_start:strain_end]))
    }
    cure_data <-data %>% dplyr::summarise_at(dplyr::vars(cure),mean, na.rm = TRUE) %>%
        dplyr::ungroup()
    return(cure_data$cure)
}



#' Circumferential uniformity ratio estimate from singular value
#' decomposition
#'
#' Circumferential uniformity ratio estimate based on singular value decomposition
#' of cardiac strain for dyssynchrony analysis.
#'
#' @inheritSection strain_cure Background
#'
#' @inheritParams strain_cure
#'
#' @return
#' @export
#'
#' @examples
strain_cure_svd <- function (data){
        # if (!class(data) == "data.frame" & !length(class(data)) ==
        #     1) {
        #         stop("Check that your data is of class data.frame and only has one class\nassociated with it")
        # }
        if (sum(is.na(data)) > 0) {
                stop("Trying to apply svd to data with NA will\n                                    result in an error.")
        }
        if (!requireNamespace("dplyr", quietly = TRUE)) {
                stop("dplyr needed for this function to work. Please install/load it.",
                     call. = FALSE)
        }


        old <- options(stringsAsFactors = FALSE)
        cure_data <- data.frame()

        strain <- as.matrix(select(data, contains("strain")))

        strain_svd <- svd(strain, nu = nrow(strain), nv = ncol(strain))

        V <- as.matrix(strain_svd$v[, 1])

        U <- as.matrix(t(strain_svd$u[, 1]))

        D <- strain_svd$d[1]

        svd_matrix <- V %*% U

        rank1 <- (svd_matrix * D)[, 5]

        fourier_frq <- fft(rank1)

        cure_val <- Mod(fourier_frq[1])/sum(Mod(fourier_frq[1]),
                                            Mod(fourier_frq[2]))

        options(old)
        return(cure_val)
}

.calc_cure <- function(strain) {
    fourier_frq <- fft(strain)
    cure_val <- (Mod(fourier_frq[1]) / sum(Mod(fourier_frq[1]),Mod(fourier_frq[2])))
    return(cure_val)
}
