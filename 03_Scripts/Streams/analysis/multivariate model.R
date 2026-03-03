source("03_Scripts/Streams/analysis/data for analysis.R")
library(posterior)
library(patchwork)
library(brms)

#call in data###########
df2<-int.ext%>%
  left_join(DO%>%
              mutate(Date=as.Date(Date),
                     TempC=fahrenheit.to.celsius(Temp_DO),
                     )%>%
              group_by(Date, ID) %>%
              summarise(
                across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop"
              ) %>%
              select(Date, ID, TempC),
            by=c('Date','ID'))%>%
  drop_na(CO2_flux, Q)%>%
  mutate(
    lQ=log10(Q),
    lext=log10(external),
    lint=log10(internal)
  )


df2 <- df2 %>%
  filter(
    is.finite(lQ), is.finite(TempC), is.finite(lint), is.finite(lext)) %>%
  droplevels()

pri <- tryCatch(prior_summary(fit_full), error = function(e) NULL)

#formulas##########

#patwhay
bf_int_full <- bf(lint ~ lQ + TempC + (1 | ID))
bf_ext_full <- bf(lext ~ lQ + TempC + (1 | ID))

# Drop TEMP 
bf_int_noT  <- bf(lint ~ lQ + (1 | ID))
bf_ext_noT  <- bf(lext ~ lQ + (1 | ID))

# Drop Q 
bf_int_noQ  <- bf(lint ~ TempC + (1 | ID))
bf_ext_noQ  <- bf(lext ~ TempC + (1 | ID))

# models

fit <- brm(
  bf_CO2flux_full +  + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/CO2flux"
)

#CO2 flux######
bf_CO2flux_full <- bf(CO2_flux ~ lQ + TempC + (1 | ID))

fit_int_noT <- brm(
  bf_CO2flux_full,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  control = list(adapt_delta = 0.95),
  file = "04_Output/stream/models/CO2flux.rds"
)

#int.ext#############
bf_ratio_full <- bf(int.ext.ratio ~ lQ + TempC + (1 | ID))

fit_ratio <- brm(
  bf_ratio_full,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  control = list(adapt_delta = 0.95),
  file = "04_Output/stream/models/int.ext.ratio.rds"
)

#remove one temp######
fit_int_noT <- brm(
  bf_int_noT + bf_ext_full + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  control = list(adapt_delta = 0.95),
  file = "04_Output/stream/models/int_noT.rds"
)


fit_ext_noT <- brm(
  bf_ext_noT + bf_int_full + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/ext_noT.rds"
)


#remove one Q############
fit_int_noQ <- brm(
  bf_int_noQ + bf_ext_full + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/int_noQ.rds"
)

fit_ext_noQ <- brm(
  bf_int_full + bf_ext_noQ + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/ext_noQ.rds"
)
#remove both Q##########
fit_noQ <- brm(
  bf_int_noQ + bf_ext_noQ + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/noQ.rds"
)

#remove both T############
fit_noT <- brm(
  bf_int_noT + bf_ext_noT + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/noT.rds"
)
# Extract LOO values#######
loo_df <- map_dfr(models, ~ {
  loo_val <- .x$criteria$loo
  data.frame(
    looic     = loo_val$estimates["looic", "Estimate"],
    looic_se  = loo_val$estimates["looic", "SE"]
  )
}, .id = "model")

#call in models#####
int_noT<- readRDS("04_Output/stream/models/int_noT.rds")
ext_noT<- readRDS("04_Output/stream/models/ext_noT.rds")
int_noQ<- readRDS("04_Output/stream/models/int_noQ.rds")
ext_noQ<- readRDS("04_Output/stream/models/ext_noQ.rds")
fit<- readRDS("04_Output/stream/models/fit.rds")
noQ<- readRDS("04_Output/stream/models/noQ.rds")
noT<- readRDS("04_Output/stream/models/noT.rds")


models <- list(
  "full"       = fit,
  "int_noT"    =int_noT,
  "ext_noT"    = ext_noT,
  "int_noQ"    = int_noQ,
  "ext_noQ"    = ext_noQ,
  "noQ"        = noQ,
  "noT"        = noT
)



# Parameters to extract
params_to_keep <- c(
  "lint_Intercept", "lint_lQ", "lint_TempC",
  "lext_Intercept", "lext_lQ", "lext_TempC",
  "rescor(lint,lext)", "sigma_lint", "sigma_lext"
)

# Extract posterior summaries
model_comparison_df <- map_dfr(models, function(mod) {
  as.data.frame(summary(mod)$fixed) %>%
    tibble::rownames_to_column("parameter") %>%
    bind_rows(
      as.data.frame(summary(mod)$cor_pars) %>%
        tibble::rownames_to_column("parameter")
    ) %>%
    filter(parameter %in% params_to_keep) %>%
    select(parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS)
}, .id = "model") %>%
  left_join(loo_df, by = "model")

# Compute delta LOOIC relative to best model
model_comparison_df <- model_comparison_df %>%
  group_by(parameter) %>%
  mutate(delta_looic = looic - min(looic)) %>%
  ungroup()

# --- Figure ---
#filter(parameter %in% c('lint_lQ',  'lint_TempC', 'lext_lQ', 'lext_TempC'),
       
#external pathway
model_comparison_df%>%
  filter(parameter %in% c('lext_lQ', 'lext_TempC'))%>%
  mutate(parameter='external')%>%
ggplot(aes(x = Estimate, y = model, color = delta_looic)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = `l-95% CI`, xmax = `u-95% CI`), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~ parameter, scales = "free") +
  scale_color_viridis_c(name = "ΔLOOIC", option = "plasma") +
  labs(
    x = "Posterior Estimate",
    y = "Model",
    title = "Model Comparison: Posterior Estimates with ΔLOOIC"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )
                                                                                                                                                                                                                                                                                                                                                                                                                                              