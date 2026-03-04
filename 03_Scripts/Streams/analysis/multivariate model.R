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
#model comparison###########
int_noT  <- readRDS("04_Output/stream/models/int_noT.rds")
ext_noT  <- readRDS("04_Output/stream/models/ext_noT.rds")
int_noQ  <- readRDS("04_Output/stream/models/int_noQ.rds")
ext_noQ  <- readRDS("04_Output/stream/models/ext_noQ.rds")
fit      <- readRDS("04_Output/stream/models/fit.rds")
noQ      <- readRDS("04_Output/stream/models/noQ.rds")
noT      <- readRDS("04_Output/stream/models/noT.rds")
int.ext.ratio<- readRDS("04_Output/stream/models/int.ext.ratio.rds")
CO2flux<- readRDS("04_Output/stream/models/CO2flux.rds")


models <- list(
  full      = fit,
  int_noT   = int_noT,
  ext_noT   = ext_noT,
  int_noQ   = int_noQ,
  ext_noQ   = ext_noQ,
  noQ       = noQ,
  noT       = noT
)

params_to_keep <- c(
  "lint_Intercept",  "lint_lQ",      "lint_TempC",
  "lext_Intercept",  "lext_lQ",      "lext_TempC",
  "rescor(lint,lext)",
  "sigma_lint",      "sigma_lext"
)


model_comparison_df <- map_dfr(models,
                               function(mod) {
                                 ## Fixed effects
                                 fix_df <- as.data.frame(summary(mod)$fixed) %>%
                                   tibble::rownames_to_column("parameter") %>%
                                   select(parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`,
                                          Rhat, Bulk_ESS, Tail_ESS)
                                 
                                 ## Correlation / random-effect covariance
                                 cor_df <- as.data.frame(summary(mod)$cor_pars) %>%
                                   tibble::rownames_to_column("parameter") %>%
                                   select(parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`,
                                          Rhat, Bulk_ESS, Tail_ESS)
                                 
                                 ## sigma -- now correctly pulled from $spec_pars
                                 sig_df <- as.data.frame(summary(mod)$spec_pars) %>%
                                   tibble::rownames_to_column("parameter") %>%
                                   select(parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`,
                                          Rhat, Bulk_ESS, Tail_ESS) %>%
                                   filter(parameter %in% c("sigma_lint", "sigma_lext"))  # drop nu rows
                                 
                                 bind_rows(fix_df, cor_df, sig_df) %>%
                                   filter(parameter %in% params_to_keep)
                               },
                               .id = "model"
) %>% relocate(model, .before = parameter)

## --------------------------------------------------------------------------- ##
## 4. Pull out σ estimates per model for colour scale ------------------------
## --------------------------------------------------------------------------- ##
sigma_df <- map_dfr(models,
                    function(mod) {
                      as.data.frame(summary(mod)$spec_pars) %>%
                        tibble::rownames_to_column("parameter") %>%
                        filter(parameter %in% c("sigma_lint", "sigma_lext")) %>%
                        select(parameter, Estimate) %>%
                        pivot_wider(names_from = parameter, values_from = Estimate)
                    },
                    .id = "model"
)

## Join σ values back for colour scale
model_comparison_df <- model_comparison_df %>%
  left_join(sigma_df, by = "model")

## --------------------------------------------------------------------------- ##
## 5. Plot -------------------------------------------------------------------
## --------------------------------------------------------------------------- ##


# Parameters without intercepts
params_to_plot <- c(
  "lint_lQ", "lint_TempC", 
  "lext_lQ", "lext_TempC"
)

# Clean parameter labels for the strip
plot_df <- model_comparison_df %>%
  filter(parameter %in% params_to_plot) %>%
  mutate(model = factor(model, levels = rev(c(
    "full",
    "int_noQ", "ext_noQ",
    "int_noT", "ext_noT",
    "noQ",     "noT"
  ))))%>%
  mutate(
    pathway = case_when(
      grepl("^lint|sigma_lint", parameter) ~ "Internal",
      grepl("^lext|sigma_lext", parameter) ~ "External"
    ),
    param_label = case_when(
      grepl("lQ",    parameter) ~ "lQ",
      grepl("TempC", parameter) ~ "TempC",
      grepl("sigma", parameter) ~ "σ"
    ),
    pathway = factor(pathway, levels = c("External", "Internal")),
    sigma=if_else(pathway=='Internal', sigma_lint, sigma_lext)
  )

ggplot(plot_df, aes(x = Estimate, y = factor(model), color = sigma)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = `l-95% CI`, xmax = `u-95% CI`), height = 0.2) +
  geom_text(aes(label = round(Estimate, 2)), vjust = -0.8, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_grid(pathway ~ param_label, scales = "free_x") +
  scale_color_viridis_c(name = "σ", option = "plasma") +
  labs(
    x = "Posterior Estimate",
    y = "Model",
    title = "Model Comparison: Posterior Estimates coloured by σ"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text  = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )
