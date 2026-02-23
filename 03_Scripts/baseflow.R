rm(list=ls())

library(tidyverse)
library(ggpmisc)
library(cowplot)
library(ggplot2)
library(EcoHydRology)

discharge <- read.csv('02_Clean_data/discharge.csv')

discharge$Date<-ymd_hms(discharge$Date)
discharge<-filter(discharge, Q != is.na(Q))

streams <- split(discharge, discharge$ID)
streams <- Filter(function(x) nrow(x) > 0, streams)

bf <- map2_dfr(
  names(streams), streams,
  function(id, df) {
    baseflow_result <- BaseflowSeparation(df$Q, filter_parameter = 0.9988431, passes = 3)

    tibble(
      ID = id,
      Date = df$Date,
      Q = df$Q,
      baseflow = baseflow_result$bt,
      quickflow = baseflow_result$qft
    )
  })



write_csv(bf, "04_Output/baseflow.csv")







