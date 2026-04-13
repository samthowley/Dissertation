library(glmmTMB)
library(performance)
library(dplyr)
library(flextable)  # for Word-friendly tables
source("03_Scripts/Streams/analysis/data for analysis.R")

# ── GLM and GLMM fitting function ─────────────────────────────────────────────
fit_glm_glmm <- function(formula, data, family = Gamma(link = "log")) {
  
  # Extract response and fixed predictor from formula
  formula_str <- deparse(formula)
  
  # GLM - no random effects
  glm_model <- glm(formula, data = data, family = family)
  
  # GLMM - with random intercept per site and AR(1)
  random_str  <- as.formula(paste("~", deparse(formula[[3]]), "+ (1 | ID) + ar1(factor(hour_index) + 0 | ID)"))
  glmm_formula <- as.formula(paste(deparse(formula[[2]]), "~", 
                                   deparse(formula[[3]]), 
                                   "+ (1 | ID) + ar1(factor(hour_index) + 0 | ID)"))
  
  glmm_model <- glmmTMB(glmm_formula, data = data, family = family)
  
  list(glm = glm_model, glmm = glmm_model)
}

# ── Extract results function ───────────────────────────────────────────────────
extract_results <- function(models, model_name) {
  
  # GLM results
  glm_sum   <- summary(models$glm)
  glm_coef  <- glm_sum$coefficients
  glm_pred  <- rownames(glm_coef)[2]  # fixed predictor
  
  glm_row <- data.frame(
    Model      = model_name,
    Type       = "GLM",
    Predictor  = glm_pred,
    Beta       = round(glm_coef[2, 1], 4),
    SE         = round(glm_coef[2, 2], 4),
    Statistic  = round(glm_coef[2, 3], 4),
    P_value    = ifelse(glm_coef[2, 4] < 0.0001, "< 0.0001",
                        round(glm_coef[2, 4], 4)),
    R2_marginal    = NA,
    R2_conditional = NA
  )
  
  # GLMM results
  glmm_sum  <- summary(models$glmm)
  glmm_coef <- glmm_sum$coefficients$cond
  glmm_pred <- rownames(glmm_coef)[2]
  glmm_r2   <- r2(models$glmm)
  glmm_pval <- glmm_coef[2, 4]
  
  glmm_row <- data.frame(
    Model      = model_name,
    Type       = "GLMM",
    Predictor  = glmm_pred,
    Beta       = round(glmm_coef[2, 1], 4),
    SE         = round(glmm_coef[2, 2], 4),
    Statistic  = round(glmm_coef[2, 3], 4),
    P_value    = ifelse(glmm_pval < 0.0001, "< 0.0001",
                        round(glmm_pval, 4)),
    R2_marginal    = round(glmm_r2$R2_marginal, 3),
    R2_conditional = round(glmm_r2$R2_conditional, 3)
  )
  
  bind_rows(glm_row, glmm_row)
}

# ── Fit all models ─────────────────────────────────────────────────────────────

# Temperature models
int.ext <- left_join(int.ext, temperature) %>%
  drop_na(Temp_PT) %>%
  distinct(Date, ID, .keep_all = TRUE)%>%
  group_by(ID) %>%
  mutate(hour_index = row_number()) %>%
  ungroup()
  

m_int_T      <- fit_glm_glmm(internal ~ TempC,        int.ext)
m_ext_T      <- fit_glm_glmm(external ~ TempC,        int.ext)
m_ratio_T    <- fit_glm_glmm(int.ext.ratio ~ TempC,   int.ext)
m_CO2_T      <- fit_glm_glmm(CO2 ~ TempC,             int.ext)
m_CO2flux_T  <- fit_glm_glmm(CO2_flux ~ TempC,        int.ext)

# Discharge models
m_int_Q      <- fit_glm_glmm(internal ~ Q,            int.ext)
m_ext_Q      <- fit_glm_glmm(external ~ Q,            int.ext)
m_ratio_Q    <- fit_glm_glmm(int.ext.ratio ~ Q,       int.ext)
m_CO2_Q      <- fit_glm_glmm(CO2 ~ Q,                 int.ext)
m_CO2flux_Q  <- fit_glm_glmm(CO2_flux ~ Q,            int.ext)


extract_results <- function(models, model_name) {
  
  # GLM results
  glm_sum   <- summary(models$glm)
  glm_coef  <- glm_sum$coefficients
  glm_pred  <- rownames(glm_coef)[2]
  glm_pval  <- glm_coef[2, 4]
  
  glm_row <- data.frame(
    Model          = model_name,
    Type           = "GLM",
    Predictor      = glm_pred,
    Beta           = round(glm_coef[2, 1], 4),
    SE             = round(glm_coef[2, 2], 4),
    Statistic      = round(glm_coef[2, 3], 4),
    P_value        = ifelse(glm_pval < 0.0001, "< 0.0001", as.character(round(glm_pval, 4))),
    R2_marginal    = as.character(NA),
    R2_conditional = as.character(NA)
  )
  
  # GLMM results
  glmm_sum  <- summary(models$glmm)
  glmm_coef <- glmm_sum$coefficients$cond
  glmm_pval <- glmm_coef[2, 4]
  glmm_r2   <- tryCatch(r2(models$glmm), error = function(e) NULL)
  
  r2m <- ifelse(!is.null(glmm_r2), as.character(round(glmm_r2$R2_marginal, 3)),    "NA")
  r2c <- ifelse(!is.null(glmm_r2), as.character(round(glmm_r2$R2_conditional, 3)), "NA")
  
  glmm_row <- data.frame(
    Model          = model_name,
    Type           = "GLMM",
    Predictor      = rownames(glmm_coef)[2],
    Beta           = round(glmm_coef[2, 1], 4),
    SE             = round(glmm_coef[2, 2], 4),
    Statistic      = round(glmm_coef[2, 3], 4),
    P_value        = ifelse(glmm_pval < 0.0001, "< 0.0001", as.character(round(glmm_pval, 4))),
    R2_marginal    = r2m,
    R2_conditional = r2c
  )
  
  bind_rows(glm_row, glmm_row)
}

# ── Recompile table ────────────────────────────────────────────────────────────
results_table <- bind_rows(
  extract_results(m_int_T,     "Internal ~ Temperature"),
  extract_results(m_ext_T,     "External ~ Temperature"),
  extract_results(m_ratio_T,   "Ratio ~ Temperature"),
  extract_results(m_CO2_T,     "CO2 ~ Temperature"),
  extract_results(m_CO2flux_T, "CO2 Flux ~ Temperature"),
  extract_results(m_int_Q,     "Internal ~ Discharge"),
  extract_results(m_ext_Q,     "External ~ Discharge"),
  extract_results(m_ratio_Q,   "Ratio ~ Discharge"),
  extract_results(m_CO2_Q,     "CO2 ~ Discharge"),
  extract_results(m_CO2flux_Q, "CO2 Flux ~ Discharge")
)

print(results_table)

