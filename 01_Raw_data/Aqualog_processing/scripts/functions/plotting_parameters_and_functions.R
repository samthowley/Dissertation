
# DOM EEM plots -----------------------------------------------------------


# 3-D Plots ---------------------------------------------------------------

eem_plot_3D <- function(data){
  x_raw <- data
  
  for (i in 1:length(x_raw))
  {
    dp <- persp3D(
      x = x_raw[[i]]$ex,
      y = x_raw[[i]]$em,
      z = t(x_raw[[i]]$x),
      theta = -37.5,
      phi = 20,
      facets = TRUE,
      xlab = "Excitation (nm)",
      ylab = "Emisison (nm)",
      zlab = "Fluorescence (A.U.)",
      main = paste(eem_list[[i]]$sample),
      ticktype = "detailed",
      box = TRUE,
      expand = 0.5
    )
    print(dp)
  }
  
}


# raman and rayleigh band plots -------------------------------------------

eem_bands <- function(data, bs1 = 12, bs2 = 12, bs3 = 12, bs4 = 12) {
  x_raw <- data
  
  for(i in 1:length(x_raw))
  {
    x_cor <- eem_remove_scattering(x_raw, "rayleigh", 1, bs1)
    x_cor <- eem_remove_scattering(x_cor, "raman", 1, bs2)
    x_cor <- eem_remove_scattering(x_cor, "raman", 2, bs3)
    x_cor <- eem_remove_scattering(x_cor, "rayleigh", 2, bs4)
    ex <- 350
    em <- x_raw[[i]]$em
    em_raw <- x_raw[[i]]$x[, which(x_raw[[i]]$ex == 350)]
    em_cor <- x_cor[[i]]$x[, which(x_cor[[i]]$ex == 350)]
    df <- data.frame(em, em_raw, em_cor)
    df$em_raman <- df$em_raw
    df$em_raman[df$em <= 375] <- NA
    df$em_rayleigh <- df$em_raw
    df$em_rayleigh[df$em > 375] <- NA
    plotrm <- ggplot2::ggplot(df, aes(x = em)) +
      ggplot2::geom_line(aes(y = em_rayleigh, color = "Rayleigh scattering"),
                size = 0.75,
                na.rm = TRUE
      ) +
      ggplot2::geom_line(aes(y = em_raman, color = "Raman scattering"),
                size = 0.75,
                na.rm = TRUE
      ) +
      ggplot2::geom_line(aes(y = em_cor, color = "Fluorescence signal"),
                size = 0.75,
                na.rm = TRUE
      ) +
      ggplot2::labs(color = "") +
      ggplot2::xlab("Emission (nm)") +
      ggplot2::ylab("Fluorescence (A.U)") +
      ggplot2::scale_color_manual(values = c("black", "#D55E00", "#0072B2")) +
      ggplot2::theme(legend.key = element_blank()) +
      ggplot2::theme(
        legend.justification = c(1, 1),
        legend.position = c(0.9, 0.9)
      )
    print(plotrm)
  }
  
}
