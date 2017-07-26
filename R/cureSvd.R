
#' Circumferential uniformity ratio estimate (CURE) computed by singular value decomposition (SVD).
#'
#' @param data Dataframe with id column, and columns for each strain
#' segment. Each column should represent a separate segment. The segments should
#'be in the following order: Antero-septal, Infero-septal, Inferior, Infero-lateral,
#'Antero-lateral and Anterior. Or equivalent.
#' @param id.column name of the id column in quotes.
#'
#' @return Returns a dataframe with CURE value for each patient.
#'
#' @importFrom magrittr %>%
#' @importFrom stats fft
#'
#' @export
#'
#' @examples
#'
cureSvd <- function(data, id.column) {

  # Given a dataframe with segmental strain values this function
  # calculates the Circumferential uniformity ratio estimate computed by
  # singular value decomposition for each patient.

  # Args:
      # data: Dataframe with id column, and columns for each strain
          # segment. Must contain strain in the columns that are supposed to
          # be included in the analysis.
      # id.column: name of the id column.

  # Libraries:
      # Requires: dplyr, svd.


  if (!class(data) == "data.frame" & !length(class(data)) == 1) {
    stop("Check that your data is of class data.frame and only has one class
associated with it")
    }

  if (sum(is.na(data)) >0) {
stop("Trying to apply svd to data with NA will
                                    result in an error.")
    }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install/load it.",
         call. = FALSE)
  }


  if (!requireNamespace("lazyeval", quietly = TRUE)) {
    stop("lazyeval needed for this function to work. Please install/load it.",
         call. = FALSE)
  }


  old_options <- options(stringsAsFactors = FALSE)

  # Extracts all unique identifiers.

  id_index <- unique(data[,"id"])

  # Creates an empty dataframe for future use.

  cure_data <- data.frame()

  id_variable <- lazyeval::interp(~ a, a = as_name(id.column))

  filter_for_id <- lazyeval::interp(~ a == id_index[i], a = as_name(id.column))

  for(i in seq_along(id_index)){

    strain <- as.matrix(data %>%

                      dplyr::group_by_(id_variable) %>%

                      dplyr::filter_(filter_for_id) %>%

                      dplyr::ungroup() %>%

                      dplyr::select(contains("strain")))


    # Applies singular value decomposition on to the data.
    # nu and nv have been set to maximum numbers as R as default uses a
    # scaled down version of svd.

    strain_svd <- svd(strain, nu = nrow(strain), nv = ncol(strain))

    # SVD data is given as a list, with V and U containing the eigenvectors and
    # D containing the singular values.

    V <- as.matrix(strain_svd$v[,1])

    U <- as.matrix(t(strain_svd$u[,1]))

    D <- strain_svd$d[1]


    svd_matrix <- V %*% U

    # Multiplies the 5th matrix column by the first singular value, as done by
    # Kenneth Bilchick and Dan Auger.

    rank1 <- (svd_matrix*D)[,5]

    # Applies fourier transform to transform data from time domain to frequency.

    fourier_frq <- fft(rank1)

    # Takes the modulus of the zero and first order terms, then divides the zero
    # order term by the sum of the zero and first order term.

    cure_val <- Mod(fourier_frq[1]) / sum( Mod(fourier_frq[1]), Mod(fourier_frq[2]) )

    # Combines the patient identifier and the corresponding cure value.

    id_cure_val <- cbind(as.data.frame(as.character(id_index[i])),
                         as.data.frame(cure_val))

    # Adds the current patient to joint data frame

    cure_data <- rbind(cure_data, id_cure_val)

  }

  colnames(cure_data) <- c(id.column, "cure")

  return(cure_data)

  on.exit(options(old_options))
}
