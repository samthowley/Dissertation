#call in data###########
source("03_Scripts/Streams/analysis/data for analysis.R")
library(posterior)
library(patchwork)
library(brms)
library(corrplot)
#make_results_table(ratio,    "int.ext.ratio")

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

df1 <- df %>%
  filter(
    is.finite(lQ), is.finite(TempC), is.finite(lint), is.finite(lext)
  ) %>%
  droplevels()

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

pri <- tryCatch(prior_summary(fit_full), error = function(e) NULL)

# Temporal Models ###########
# partial pooling ###########

bf_int_full.partial_pool <- bf(lint ~ lQ + TempC + (1 | ID))
bf_ext_full.partial_pool <- bf(lext ~ lQ + TempC + (1 | ID))


fit_temporal <- brm(
  bf_int_full.partial_pool + bf_ext_full.partial_pool + set_rescor(TRUE),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/full.rde"
)

full.rde <- readRDS("C:/Dissertation/04_Output/stream/models/temporal/full.rde.rds")

# Full model with partial pooling and interaction ###########
bf_int_full_interaction <- bf(lint ~ lQ * TempC + (1 | ID))
bf_ext_full_interaction <- bf(lext ~ lQ * TempC + (1 | ID))

fit_temporal_interaction <- brm(
  bf_int_full_interaction + bf_ext_full_interaction + set_rescor(TRUE),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/temporal_interaction"
)
temporal_interaction <- readRDS("C:/Dissertation/04_Output/stream/models/temporal/temporal_interaction.rds")

#no pooling, full model#########
bf_int_no.pooling <- bf(lint ~ lQ + TempC + ID)
bf_ext_no.pooling <- bf(lext ~ lQ + TempC + ID)

fit_no.pooling <- brm(
  bf_int_no.pooling + bf_ext_no.pooling + set_rescor(TRUE),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/temporal_no.pooling"
)

#no pooling, interaction, full model#########
bf_int_no.pooling <- bf(lint ~ lQ * TempC + ID)
bf_ext_no.pooling <- bf(lext ~ lQ * TempC + ID)

fit_no.pooling <- brm(
  bf_int_no.pooling + bf_ext_no.pooling + set_rescor(TRUE),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/temporal_no.pooling"
)


#dropformulas##########

# full grouped ny 
bf_int_noT  <- bf(lint ~ lQ + ID)
bf_ext_noT  <- bf(lext ~ lQ + ID)

# Drop Q 
bf_int_noQ  <- bf(lint ~ TempC + ID)
bf_ext_noQ  <- bf(lext ~ TempC + ID)


#remove one temp#
fit_int_noT <- brm(
  bf_int_noT + bf_ext_full + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  control = list(adapt_delta = 0.95),
  file = "04_Output/stream/models/drop/int_noT.rds"
)


fit_ext_noT <- brm(
  bf_ext_noT + bf_int_full + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/drop/ext_noT.rds"
)


#remove one Q#
fit_int_noQ <- brm(
  bf_int_noQ + bf_ext_full + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/drop/int_noQ.rds"
)

fit_ext_noQ <- brm(
  bf_int_full + bf_ext_noQ + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/drop/ext_noQ.rds"
)
#remove both Q#
fit_noQ <- brm(
  bf_int_noQ + bf_ext_noQ + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/drop/noQ.rds"
)

#remove both T#
fit_noT <- brm(
  bf_int_noT + bf_ext_noT + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/drop/noT.rds"
)


#model comparison###########
int_noT  <- readRDS("04_Output/stream/models/int_noT.rds")
ext_noT  <- readRDS("04_Output/stream/models/ext_noT.rds")
int_noQ  <- readRDS("04_Output/stream/models/int_noQ.rds")
ext_noQ  <- readRDS("04_Output/stream/models/ext_noQ.rds")
fit      <- readRDS("04_Output/stream/models/fit.rds")
bayes_R2(fit)
noQ      <- readRDS("04_Output/stream/models/noQ.rds")
noT      <- readRDS("04_Output/stream/models/noT.rds")
int.ext.ratio<- readRDS("04_Output/stream/models/int.ext.ratio.rds")
CO2flux<- readRDS("04_Output/stream/models/CO2flux.rds")
