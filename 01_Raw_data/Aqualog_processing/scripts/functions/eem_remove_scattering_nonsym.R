
# source functions taken from github, required in the code to change other functions

.find_raman_peaks <- function(ex) {
  # For water, the Raman peak appears at a wavenumber 3600 cm lower than the
  # incident wavenumber. For excitation at 280 nm, the Raman peak from water
  # occurs at 311 nm. Source : Principles of Fluorescence Spectroscopy (2006) -
  # Third Edition.pdf
  
  ## Convert wavenumber from nm to cm
  ex_wave_number <- 1 / ex
  
  ## For water. 3600 nm = 0.00036 cm
  raman_peaks <- ex_wave_number - 0.00036 # I think Stedmon use 3400 TODO
  
  ## Bring back to nm
  raman_peaks <- 1 / raman_peaks
  
  # raman_peaks <- -(ex / (0.00036 * ex - 1))
  
  return(raman_peaks)
}

.is_eemlist <- function(eem) {
  ifelse(class(eem) == "eemlist", TRUE, FALSE)
}

.is_eem <- function(eem) {
  ifelse(class(eem) == "eem", TRUE, FALSE)
}

# function which allows to remove scattering non symmetrically

eem_remove_scattering_nonsym <- function(eem, type, order = 1, width_under = 5, width_above = 5) {
  stopifnot(.is_eemlist(eem) | .is_eem(eem), all(type %in%
                                                   c("raman", "rayleigh")), is.numeric(order),
            is.numeric(width_under), is.numeric(width_above), length(order) == 1, length(type) ==
              1, length(width_above) == 1, length(width_under) == 1)
  if (.is_eemlist(eem)) {
    res <- lapply(eem, eem_remove_scattering_nonsym, type = type,
                  order = order, width_under = width_under, width_above = width_above)
    class(res) <- class(eem)
    return(res)
  }
  x <- eem$x
  em <- eem$em
  ex <- eem$ex
  if (type == "raman") {
    ex <- .find_raman_peaks(eem$ex)
  }
  ind1 <- mapply(function(x) em <= x, order * ex - width_under)
  ind2 <- mapply(function(x) em <= x, order * ex + width_above)
  ind3 <- ifelse(ind1 == TRUE | ind2 == FALSE, 1, NA)
  x <- x * ind3
  res <- eem
  res$x <- x
  attributes(res) <- attributes(eem)
  attr(res, "is_scatter_corrected") <- TRUE
  class(res) <- class(eem)
  return(res)
}
