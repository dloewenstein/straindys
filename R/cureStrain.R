
#' Circumferential uniformity ratio estimate (CURE) averaged over all timeframes.
#'
#' @param data Dataframe with id column, and columns for each strain
#' segment. Each column should represent a separate segment. The segments should
#'be in the following order: Antero-septal, Infero-septal, Inferior, Infero-lateral,
#'Antero-lateral and Anterior. Or equivalent.
#' @param id.column name of the id column in quotes.
#'
#' @return Returns a dataframe with average CURE value for each patient.
#' @export
#'
#' @examples cureStrain(data = strain_data, id.column = "id")
#'
cureStrain <- function(data, id.column) {

  # Given a dataframe with segmental strain values this function
  # calculates the average Circumferential uniformity ratio estimate.

  # Args:
  # data: Dataframe with id column, and columns for each strain
  # segment. Must contain strain in the columns that are supposed to
  # be included in the analysis.
  # id.column: name of the id column.

  # Libraries:
  # Requires: dplyr, lazeyval.

  require(dplyr)

  require(lazyeval)

  if (sum(is.na(data)) >0) {stop("Trying to apply CURE to data with NA will
                                 cause erroneous results.")}

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install/load it.",
         call. = FALSE)
  }


  if (!requireNamespace("lazyeval", quietly = TRUE)) {
    stop("lazyeval needed for this function to work. Please install/load it.",
         call. = FALSE)
  }


  options(stringsAsFactors = FALSE)

  # Function for calculating strain. -----------------------------------------

  calcCure <- function(strain){

    # Applies fourier transform to transform data from time domain to frequency.

    fourier_frq <- fft(strain)

    # Takes the modulus of the zero and first order terms, then divides the zero
    # order term by the sum of the zero and first order term.

    cure_val <- (Mod(fourier_frq[1])/sum(Mod(fourier_frq[1]), Mod(fourier_frq[2])))

    return(cure_val)
  }

  # --------------------------------------------------------------------------

  strain_columns <- which(grepl("strain", colnames(data)))

  strain_start <- strain_columns[1]

  strain_end <- strain_columns[length(strain_columns)]

  id_variable <- interp(~a, a = as_name(id.column))

  data$cure <- c()

  for(i in 1:nrow(data)) {

    data$cure[i] <- calcCure(as.numeric(data[i,strain_start:strain_end]))
  }

  cure_data <- data %>%
    group_by_(id_variable) %>%
    summarise_at(vars(cure), mean, na.rm=TRUE) %>%
    ungroup()

  return(cure_data)

  }
