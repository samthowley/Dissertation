source("03_Scripts/Streams/analysis/data for analysis.R")
library(posterior)
library(patchwork)
library(brms)
library(corrplot)
make_results_table <- function(model, model_name) {
  as.data.frame(posterior_summary(model)) %>%
    tibble::rownames_to_column("parameter") %>%
    filter(grepl("^b_", parameter)) %>%          # keep only regression coefficients
    filter(!grepl("Intercept", parameter)) %>%    # drop intercept
    mutate(
      parameter = gsub("^b_", "", parameter),     # clean parameter names
      Result = case_when(
        Q2.5 > 0  ~ "positive",
        Q97.5 < 0 ~ "negative",
        TRUE      ~ "unclear"
      ),
      model = model_name
    ) %>%
    select(
      model,
      Parameter = parameter,
      Result,
      Estimate,
      Lower = Q2.5,
      Upper = Q97.5
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

# Call in data ###########
df <- int.ext %>%
  left_join(DO %>%
              mutate(Date = as.Date(Date),
                     TempC = fahrenheit.to.celsius(Temp_DO)) %>%
              group_by(Date, ID) %>%
              summarise(
                across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop"
              ) %>%
              select(Date, ID, TempC),
            by = c('Date', 'ID')) %>%
  left_join(SpC) %>%
  left_join(pH) %>%
  drop_na(CO2_flux, Q) %>%
  mutate(
    lQ   = log10(Q),
    lext = log10(external),
    lint = log10(internal)
  )

df2 <- df %>%
  filter(
    is.finite(lQ), is.finite(TempC), is.finite(lint), is.finite(lext),
    is.finite(SpC), is.finite(depth), is.finite(pH)
  ) %>%
  droplevels()


# Load priors if a previous fit exists ###########
pri <- tryCatch(prior_summary(fit_full), error = function(e) NULL)

#spatial##########


##CO2############
CO2.interaction <- bf(CO2 ~ lQ * TempC + SpC * pH)

fit <- brm(
  CO2.interaction,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/CO2_spatial_interaction"
)
CO2_spatial_interaction



CO2.spat <- bf(CO2 ~ lQ + TempC + SpC + pH)

fit <- brm(
  CO2.spat,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/CO2_spatial"
)

CO2_spatial 



##CO2_flux####
CO2_flux.interaction <- bf(CO2_flux ~ lQ * TempC + SpC * pH)

fit <- brm(
  CO2_flux.interaction,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/CO2flux_spatial_interaction"
)

CO2flux_spatial_interaction

make_results_table(CO2flux_spatial_interaction,    "CO2flux")

CO2_flux.spat <- bf(CO2_flux ~ lQ + TempC + SpC + pH)

fit <- brm(
  CO2_flux.spat,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/CO2flux_spatial"
)

CO2flux_spatial 
make_results_table(CO2flux_spatial,    "CO2flux")




#temporal##########

##Separate univariate model for CO2#######
fit_CO2 <- brm(
  bf(CO2 ~ lQ + TempC + (1 | ID)),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/CO2"
)


##Separate univariate model for CO2 flux#######
fit_CO2 <- brm(
  bf(CO2_flux ~ lQ + TempC + (1 | ID)),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/CO2_flux"
)
