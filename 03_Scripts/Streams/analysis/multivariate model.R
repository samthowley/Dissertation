
#call in data###########
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

# Spatial Models ###########

## No interaction#######
bf_int_full.complete_pooling <- bf(lint ~ lQ + TempC + SpC + pH)
bf_ext_full.complete_pooling <- bf(lext ~ lQ + TempC + SpC + pH)

fit_complete <- brm(
  bf_int_full.complete_pooling + bf_ext_full.complete_pooling + set_rescor(TRUE),
  data   = df2,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/spatial/complete"
)

complete_pooling.group <- readRDS("C:/Dissertation/04_Output/stream/models/spatial/complete_pooling.group.rds")

## Interaction###########
bf_int_full.interaction <- bf(lint ~ lQ * TempC + SpC * pH)
bf_ext_full.interaction <- bf(lext ~ lQ * TempC + SpC * pH)

fit_spatial_interaction <- brm(
  bf_int_full.interaction + bf_ext_full.interaction + set_rescor(TRUE),
  data   = df2,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/spatial/spatial_interaction"
)

spatial_interaction <- readRDS("C:/Dissertation/04_Output/stream/models/spatial/spatial_interaction.rds")

##Ratio###########
int.ext.ratio.interaction <- bf(int.ext.ratio ~ lQ * TempC + SpC * pH)

fit <- brm(
  int.ext.ratio.interaction,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/ratio_spatial_interaction"
)
ratio_spatial

make_results_table(ratio_spatial,    "int.ext.ratio")




int.ext.ratio.spat <- bf(int.ext.ratio ~ lQ + TempC + SpC + pH)

fit <- brm(
  int.ext.ratio.spat,
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/spatial/ratio_spatial"
)

ratio_spatial_interaction
make_results_table(ratio_spatial_interaction,    "int.ext.ratio")



# Temporal Models ###########
df1 <- df %>%
  filter(
    is.finite(lQ), is.finite(TempC), is.finite(lint), is.finite(lext)
  ) %>%
  droplevels()

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

## Full model with partial pooling and interaction ###########
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

##Separate univariate model for ratio#######
fit_ratio <- brm(
  bf(int.ext.ratio ~ lQ + TempC + (1 | ID)),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/ratio"
)


fit_ratio <- brm(
  bf(int.ext.ratio ~ lQ * TempC + (1 | ID)),
  data   = df1,
  family = student(),
  prior  = pri,
  cores  = 4,
  file   = "04_Output/stream/models/temporal/ratio_interaction"
)

ratio_interaction 
make_results_table(ratio_interaction,    "int.ext.ratio")

ratio 
make_results_table(ratio,    "int.ext.ratio")

#dropformulas##########

# full grouped ny 
bf_int_noT  <- bf(lint ~ lQ + (1 | ID))
bf_ext_noT  <- bf(lext ~ lQ + (1 | ID))

# Drop Q 
bf_int_noQ  <- bf(lint ~ TempC + (1 | ID))
bf_ext_noQ  <- bf(lext ~ TempC + (1 | ID))


bf_CO2flux_full <- bf(CO2_flux ~ lQ + TempC + (1 | ID))
bf_ratio_full <- bf(int.ext.ratio ~ lQ + TempC + (1 | ID))


#remove one temp#
fit_int_noT <- brm(
  bf_int_noT + bf_ext_full + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  control = list(adapt_delta = 0.95),
  file = "04_Output/stream/models/int_noT.rds"
)


fit_ext_noT <- brm(
  bf_ext_noT + bf_int_full + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/ext_noT.rds"
)


#remove one Q#
fit_int_noQ <- brm(
  bf_int_noQ + bf_ext_full + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/int_noQ.rds"
)

fit_ext_noQ <- brm(
  bf_int_full + bf_ext_noQ + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/ext_noQ.rds"
)
#remove both Q#
fit_noQ <- brm(
  bf_int_noQ + bf_ext_noQ + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/noQ.rds"
)

#remove both T#
fit_noT <- brm(
  bf_int_noT + bf_ext_noT + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models/noT.rds"
)


#CO2 flux#
fit <- brm(
  bf_CO2flux_full +  + set_rescor(TRUE),
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/CO2flux"
)

#int.ext
fit_ratio <- brm(
  bf_ratio_full,
  data = df1,
  family = student(),
  prior = pri,
  cores = 4,
  control = list(adapt_delta = 0.95),
  file = "04_Output/stream/models/int.ext.ratio.rds"
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



# df2.corr <- df2 %>%
#    select(ID, CO2_flux, lQ, TempC, lint, lext, SpC, pH, depth) %>%
#    drop_na() %>%
#    group_by(ID) %>%
#    mutate(across(where(is.numeric), ~ . - mean(., na.rm = TRUE))) %>%
#    ungroup() %>%
#    select(-ID)
# 
#  cor_matrix <- cor(df2.corr, use = "pairwise.complete.obs")
# 
#  corrplot(cor_matrix,
#           method      = "ellipse",
#           type        = "upper",
#           addCoef.col = "black",
#          tl.col      = "black",
#           tl.srt      = 45,
#          col         = COL2("RdBu", 200),
#           title       = "Within-site Correlations",
#           mar         = c(0, 0, 1, 0))