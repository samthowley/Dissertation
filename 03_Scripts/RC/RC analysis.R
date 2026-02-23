
#Prepare dataset###########
RC.gas<- read_csv('04_Output/RC/RC.gas.sample.flux.csv')%>%
  select(
    Date,Site,
    CO2.water_umol.L, CO2_molL, CO2_flux, CO2.water.ppm,
    CH4.water_umol.L, CH4_molL, CH4_flux, CH4.water.ppm)


RC.water<- read_csv("04_Output/RC/RC.DC.sample.flux.csv")%>%
  full_join(RC.gas)%>%
  distinct(Date, Site, ID, .keep_all = T)%>%
  filter(well_types=='RW')

write_csv(RC.water, "04_Output/RC/master.RC.csv")

split_list <- RC.water %>%
  group_by(Site) %>%
  group_split()

names(split_list) <- RC.water %>%
  group_by(Site) %>%
  group_keys() %>%
  pull(Site)

write_xlsx(split_list, path = "04_Output/RC/RC_by_well.xlsx")

library(openxlsx)
file_path <- "04_Output/RC/RC_by_well.xlsx"
sheet_names <- excel_sheets(file_path)

RC_df <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
}) %>%
  bind_rows()

#regressions#####

cols <- c('DOC_g.m2.day','DIC_g.m2.day','CO2_flux','qL_m2.sec','WTE','Site')
unique_sites <- unique(RC_df$Site[!is.na(RC_df$Site)])

RC <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- RC_df %>%
      filter(Site == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)

col_names<- c("ID", "pvalue", "slope", "r2", "type")


DOC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DOC_g.m2.day", "WTE")])) > 1
  DOC.elevation.p <- DOC.elevation.slope <- DOC.elevation.r2 <- NA

  if (valid_elev) {
    DOC.elevation <- lm(DOC_g.m2.day ~ WTE, data = df)
    DOC.elevation.cf <- summary(DOC.elevation)
    DOC.elevation.p <- DOC.elevation.cf$coefficients["WTE", "Pr(>|t|)"]
    DOC.elevation.slope <- DOC.elevation.cf$coefficients["WTE", "Estimate"]
    DOC.elevation.r2 <- DOC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DOC.elevation.p = as.numeric(DOC.elevation.p),
    DOC.elevation.slope = as.numeric(DOC.elevation.slope),
    DOC.elevation.r2 = as.numeric(DOC.elevation.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")%>%mutate(type='DOC')
colnames(DOC_table)<-col_names

DIC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DIC_g.m2.day", "WTE")])) > 1

  # Initialize with NA
  DIC.elevation.p <- DIC.elevation.slope <- DIC.elevation.r2 <- NA
  DIC.qL.p <- DIC.qL.slope <- DIC.qL.r2 <- NA

  if (valid_elev) {
    DIC.elevation <- lm(DIC_g.m2.day ~ WTE, data = df)
    DIC.elevation.cf <- summary(DIC.elevation)
    DIC.elevation.p <- DIC.elevation.cf$coefficients["WTE", "Pr(>|t|)"]
    DIC.elevation.slope <- DIC.elevation.cf$coefficients["WTE", "Estimate"]
    DIC.elevation.r2 <- DIC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DIC.elevation.p = as.numeric(DIC.elevation.p),
    DIC.elevation.slope = as.numeric(DIC.elevation.slope),
    DIC.elevation.r2 = as.numeric(DIC.elevation.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")%>%mutate(type='DIC')
colnames(DIC_table)<-col_names

CO2_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("CO2_flux", "WTE")])) > 1

  # Initialize with NA
  CO2.elevation.p <- CO2.elevation.slope <- CO2.elevation.r2 <- NA
  CO2.qL.p <- CO2.qL.slope <- CO2.qL.r2 <- NA

  if (valid_elev) {
    CO2.elevation <- lm(CO2_flux ~ WTE, data = df)
    CO2.elevation.cf <- summary(CO2.elevation)
    CO2.elevation.p <- CO2.elevation.cf$coefficients["WTE", "Pr(>|t|)"]
    CO2.elevation.slope <- CO2.elevation.cf$coefficients["WTE", "Estimate"]
    CO2.elevation.r2 <- CO2.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    CO2.elevation.p = as.numeric(CO2.elevation.p),
    CO2.elevation.slope = as.numeric(CO2.elevation.slope),
    CO2.elevation.r2 = as.numeric(CO2.elevation.r2)
  )
})
CO2_table <- bind_rows(CO2_relationships, .id = "ID")%>%mutate(type='CO2')
colnames(CO2_table)<-col_names


relationships<-rbind(DOC_table, DIC_table, CO2_table)

