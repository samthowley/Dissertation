#Set your main Aqualog folder directory where EVERYTHING lives (projects, scripts, raw data)
setwd("01_Raw_data/Aqualog_processing")
project_name <- "05012025"
dilution_sheet_name <- paste("metatable_dilution_",project_name,".csv",sep = "")
review_sample <- c('5')
output_date <- "05012025"

librarian::shelf(
  tidyr,
  plyr,
  dplyr,
  staRdom,
  parallel,
  doParallel,
  foreach,
  rlist,
  plot3D,
  data.table,
  knitr,
  quiet = TRUE)
options(scipen = 999)
library(tcltk)

source("./scripts/functions/format_aqua_data.R")
source("./scripts/functions/plotting_parameters_and_functions.R")
source("./scripts/functions/spectral_corrections.R")
source("./scripts/functions/grid_size_adjustment.R")
source("./scripts/functions/eem_remove_scattering_nonsym.R")

#Emily's updated FI index and associated functions
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


#format data
convert_aqualog()

#1. Load in EEMs and ABS Data
cores <- parallel::detectCores(logical = FALSE)
#select project folder directory where abs and eem folders are stored
data_folder <- choose_directory()

eem_folder <- paste(data_folder,"/eem",sep="") # selects eem folder
eem_list <- eem_read(eem_folder, import_function = eem_csv, recursive = T) # list of all eem files
absorbance_path <- paste(data_folder,"/abs",sep="") # select abs folder
absorbance <- absorbance_read(absorbance_path, recursive = T, cores = cores) # load csv or txt tables in folder

#selecting eem files for only those you want to review
# eem_list_forReview <-
#   eem_extract(
#     eem_list,
#     review_sample,
#     keep = TRUE,
#     ignore_case = TRUE)
# #plot the review sample
# eem_overview_plot(eem_list_forReview)

#Step 1: create dilution sheet
eem_metatemplate(eem_list, absorbance) %>%
  mutate(dilution = "6") %>%
  write.csv(file = paste("./projects/Howley_Bradford",dilution_sheet_name,sep = "/"), row.names = FALSE)

library(tidyverse)
metatable_dilution_05012025 <- read_csv("projects/Howley_Bradford/metatable_dilution_05012025.csv")
metatable_dilution_06102024 <- read_csv("projects/Howley_Bradford/metatable_dilution_06102024.csv")
met<-rbind(metatable_dilution_06102024, metatable_dilution_05012025)%>% distinct(sample, .keep_all = T)

write_csv(met, "projects/Howley_Bradford/metatable_dilution_05012025.csv")

#Step 3: read in edited dilution sheet
meta <-
  read.table(paste("./projects/Howley_Bradford",dilution_sheet_name,sep = "/"),
             header = TRUE,
             sep = ",",
             dec = ".",
             row.names = 1)

absorbance <- abs_blcor(absorbance, wlrange = c(680, 700))
eem_list <- eem_range(eem_list,ex = c(239,800), em = c(246,800))

#double checking the min and max here
min(eem_list[[1]][["ex"]]) #239
max(eem_list[[1]][["ex"]]) #800

#selecting eem files for only those you want to review
# eem_list_forReview <-
#   eem_extract(
#     eem_list,
#     review_sample,
#     keep = TRUE,
#     ignore_case = TRUE)
# eem_list <-
#   eem_range(eem_list,
#             ex = c(245, 600),
#             em = c(245, 650))


# remove the blank scatter from sample scatter. if you have more than one blank, they will be averaged before removal.
eem_list <- eem_remove_blank(eem_list)
eem_list <- eem_interp(eem_list, type = 0, nonneg = TRUE)

# eem_list_forReview <-
#   eem_extract(
#     eem_list,
#     review_sample,
#     keep = TRUE,
#     ignore_case = TRUE)
# # View EEMs plots
# eem_overview_plot(eem_list_forReview, spp = 9, contour = TRUE)

#inner filter effect
eem_list <-
  eem_ife_correction(
    data = eem_list,
    abs_data = absorbance,
    cuvl = 1,
    unit = "absorbance"
  )

#raman normalization
eem_list <- eem_raman_normalisation2(eem_list, blank = "blank")
# eem_list_forReview <-
#   eem_extract(
#     eem_list,
#     review_sample,
#     keep = TRUE,
#     ignore_case = TRUE)

#remove blanks
eem_list <-
  eem_extract(eem_list,
              c("nano", "miliq", "milliq", "mq", "blank"),
              ignore_case = TRUE)
absorbance <-
  dplyr::select(absorbance,
                -dplyr::matches("nano|miliq|milliq|mq|blank", ignore.case = TRUE))

#remove Rayleigh Band 2
eem_list_rem <-
  eem_rem_scat(
    eem_list,
    remove_scatter = c(FALSE, FALSE, FALSE, TRUE),
    remove_scatter_width = c(5, 5, 0, 20),
    interpolation = TRUE
  )
# eem_list_rem_forReview <-
#   eem_extract(
#     eem_list_rem,
#     review_sample,
#     keep = TRUE,
#     ignore_case = TRUE)
# Check Band Removal Widths
#eem_overview_plot(eem_list_rem_forReview, spp = 9, contour = TRUE)

#remove Rayleigh Band 1
eem_list_rem <-
  eem_remove_scattering_nonsym(
    eem_list_rem,
    type = "rayleigh",
    order = 1,
    width_above = 10, #change this and the line below if you want a thinner/thicker chunk removed
    width_under = 50
  )
# eem_list_rem_forReview <-
#   eem_extract(
#     eem_list_rem,
#     review_sample,
#     keep = TRUE,
#     ignore_case = TRUE)
# Check Band Removal Widths
# eem_overview_plot(eem_list_rem_forReview, spp = 9, contour = T)


# Interpolate EEMs for removed scatter bands
eem_list_done <-
  eem_interp(
    eem_list_rem,
    cores = cores,
    type = 0,
    verbose = TRUE,
    nonneg = TRUE
  )
# eem_list_done_forReview <-
#   eem_extract(
#     eem_list_done,
#     review_sample,
#     keep = TRUE,
#     ignore_case = TRUE)
# View interpolated EEMs
#eem_overview_plot(eem_list_done_forReview, spp = 9, contour = T)

#dilution
dil_data <- meta["dilution"]

eem_list_done <- eem_dilution(eem_list_done,dil_data)
abs_dil <-
  eem_absdil(absorbance,
             eem_list = eem_list_done,
             dilution = dil_data,
             cor_data = NULL,
             auto = TRUE,
             verbose = FALSE)

#indices
slope_parms <-
  abs_parms(abs_dil, #absorbance, [INPUT EITHER THE DILUTED OR NON DILUTED DATA - HERE THE DILUTED ONE IS SELECTED]
            cuvle = 1,
            unit = "absorbance",
            add_as = c(250, 272, 340, 350, 365, 400, 405),
            limits = list(c(325, 375), c(275, 295), c(350, 400), c(300, 700)),
            l_ref = list(325, 275, 350, 300),
            cores = cores)

slope_parms <- slope_parms %>%
  mutate(E3_E4 = a340 / a254) # has been used to predict DOC; range: 0.20 - 0.38 (Tipping et al., 2009)

library(tidyverse)
slope_parms$site_date<-str_split_fixed(slope_parms$sample, "[A-Z]+", 4)[,4]
slope_parms$site<-str_split_fixed(slope_parms$site_date, "_", 2)[,1]
slope_parms$Date<-str_split_fixed(slope_parms$site_date, "_", 2)[,2]
slope_parms$Date<-mdy(slope_parms$Date)

write.csv(slope_parms, row.names = FALSE,
          file = paste("./projects/Howley_Bradford/output/indices_abs_",output_date,".csv",sep = ""))

eem_index <-
  cbind.data.frame(
    eem_biological_index(eem_list_done),
    eem_coble_peaks(eem_list_done),
    eem_fluorescence_index(eem_list_done), # old method
    eem_fluor_index(eem_list_done), # new method
    eem_humification_index(eem_list_done, scale = FALSE))

eem_index$site_date<-str_split_fixed(eem_index$sample, "[A-Z]+", 4)[,4]
eem_index$site<-str_split_fixed(eem_index$site_date, "_", 2)[,1]
eem_index$Date<-str_split_fixed(eem_index$site_date, "_", 2)[,2]
eem_index$Date<-mdy(eem_index$Date)

write.csv(eem_index, row.names = FALSE,
          file = paste("./projects/Howley_Bradford/output/","indices_eem_",output_date,".csv",sep = ""))
