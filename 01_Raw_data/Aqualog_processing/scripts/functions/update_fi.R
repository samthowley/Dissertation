# FI Function Update ------------------------------------------------------

msg_warning_wavelength <- function() {
  msg <- "This metric uses either excitation or emission wavelengths that were not present in the data. Data has been interpolated to fit the requested wavelengths."
  return(msg)
}

is_eemlist <- function(eem) {
  ifelse(class(eem) == "eemlist", TRUE, FALSE)
}

is_eem <- function(eem) {
  ifelse(class(eem) == "eem", TRUE, FALSE)
}

eem_fluor_index <- function(eem, verbose = TRUE) {
  library(staRdom)
  
  stopifnot(is_eemlist(eem) | is_eem(eem))
  
  ## It is a list of eems, then call lapply
  if (is_eemlist(eem)) {
    res <- lapply(eem, eem_fluor_index, verbose = verbose)
    res <- dplyr::bind_rows(res)
    
    return(res)
  }
  
  if (!all(370 %in% eem$ex & c(470, 520) %in% eem$em) & verbose) {
    warning(msg_warning_wavelength(), call. = FALSE)
  }
  
  fluo_470 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 470)
  fluo_520 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 520)
  
  fi_cor <- fluo_470 / fluo_520
  
  return(data.frame(sample = eem$sample, fi_cor = fi_cor, stringsAsFactors = FALSE))
}