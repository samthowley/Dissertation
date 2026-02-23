remove_old_correction <- 
  function(eem_list, Excor, Emcor) 
  {
    x <- eem_getextreme(eem_list)
    xEx <- range(Excor[, 1])
    xEm <- range(Emcor[, 1])
    if (min(xEx) > min(x$ex) & max(xEx) < max(x$ex)) 
      stop("Excitation correction does not cover EEM excitation spectrum!")
    if (min(xEm) > min(x$em) & max(xEm) < max(x$em)) 
      stop("Emission correction does not cover EEM emission spectrum!")
    eem_list <- lapply(eem_list, function(eem) {
      Excor1 <- approx(x = Excor[[1]], y = Excor[[2]], xout = eem$ex)$y
      Emcor1 <- approx(x = Emcor[[1]], y = Emcor[[2]], xout = eem$em)$y
      mcor <- Emcor1 %*% t(Excor1)
      eem$x <- eem$x / mcor
      eem
    }) %>% `class<-`("eemlist")
  }