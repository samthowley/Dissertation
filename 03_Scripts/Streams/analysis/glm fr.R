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
  glm_sum  <- summary(models$glm)
  glm_coef <- glm_sum$coefficients
  glm_pval <- glm_coef[2, 4]
  glm_aic  <- round(AIC(models$glm), 2)
  
  glm_row <- data.frame(
    Model     = model_name,
    Type      = "GLM",
    Predictor = rownames(glm_coef)[2],
    Beta      = round(glm_coef[2, 1], 4),
    SE        = round(glm_coef[2, 2], 4),
    Statistic = round(glm_coef[2, 3], 4),
    P_value   = ifelse(glm_pval < 0.0001, "< 0.0001", as.character(round(glm_pval, 4))),
    AIC       = as.character(glm_aic),
    AR1       = "Not accounted for",
    Delta_AIC = as.character(round(glm_aic - round(AIC(models$glmm), 2), 2))
  )
  
  # Extract AR(1) correlation from GLMM
  glmm_sum  <- summary(models$glmm)
  glmm_coef <- glmm_sum$coefficients$cond
  glmm_pval <- glmm_coef[2, 4]
  glmm_aic  <- round(AIC(models$glmm), 2)
  
  # Extract AR1 phi value
  ar1_val <- tryCatch({
    vc <- VarCorr(models$glmm)
    ar1_corr <- attr(vc$cond[[2]], "correlation")
    as.character(round(ar1_corr[1, 2], 3))
  }, error = function(e) "NA")
  
  glmm_row <- data.frame(
    Model     = model_name,
    Type      = "GLMM",
    Predictor = rownames(glmm_coef)[2],
    Beta      = round(glmm_coef[2, 1], 4),
    SE        = round(glmm_coef[2, 2], 4),
    Statistic = round(glmm_coef[2, 3], 4),
    P_value   = ifelse(glmm_pval < 0.0001, "< 0.0001", as.character(round(glmm_pval, 4))),
    AIC       = as.character(glmm_aic),
    AR1       = ar1_val,
    Delta_AIC = "-"
  )
  
  bind_rows(glm_row, glmm_row)
}
# ── Fit all models ─────────────────────────────────────────────────────────────

# Temperature models
int.ext <- left_join(int.ext, temperature) %>%
  drop_na(Temp_PT, Q) %>%
  filter(Q>0, internal>0)%>%
  distinct(Date, ID, .keep_all = TRUE)%>%
  group_by(ID) %>%
  mutate(
    hour_index = row_number(),
    linternal=log10(internal),
    lexternal=log10(external),
    lQ=log10(Q)) %>%
  ungroup()


m_int_T      <- fit_glm_glmm(internal ~ TempC,        int.ext)
m_ext_T      <- fit_glm_glmm(external ~ TempC,        int.ext)
m_ratio_T    <- fit_glm_glmm(int.ext.ratio ~ TempC,   int.ext)
m_CO2_T      <- fit_glm_glmm(CO2 ~ TempC,             int.ext)
m_CO2flux_T  <- fit_glm_glmm(CO2_flux ~ TempC,        int.ext)

# Discharge models
m_int_Q      <- fit_glm_glmm(internal ~ lQ,            int.ext)
m_ext_Q      <- fit_glm_glmm(external ~lQ,            int.ext)
m_ratio_Q    <- fit_glm_glmm(int.ext.ratio ~ lQ,       int.ext)
m_CO2_Q      <- fit_glm_glmm(CO2 ~ lQ,                 int.ext)
m_CO2flux_Q  <- fit_glm_glmm(CO2_flux ~ lQ,            int.ext)


extract_results <- function(models, model_name) {
  
  # GLM results
  glm_sum  <- summary(models$glm)
  glm_coef <- glm_sum$coefficients
  glm_pval <- glm_coef[2, 4]
  glm_aic  <- round(AIC(models$glm), 2)
  
  glm_row <- data.frame(
    Model     = model_name,
    Type      = "GLM",
    Predictor = rownames(glm_coef)[2],
    Beta      = round(glm_coef[2, 1], 4),
    SE        = round(glm_coef[2, 2], 4),
    Statistic = round(glm_coef[2, 3], 4),
    P_value   = ifelse(glm_pval < 0.0001, "< 0.0001", as.character(round(glm_pval, 4))),
    AIC       = as.character(glm_aic),
    AR1       = "Not accounted for",
    Delta_AIC = as.character(round(glm_aic - round(AIC(models$glmm), 2), 2))
  )
  
  # Extract AR(1) correlation from GLMM
  glmm_sum  <- summary(models$glmm)
  glmm_coef <- glmm_sum$coefficients$cond
  glmm_pval <- glmm_coef[2, 4]
  glmm_aic  <- round(AIC(models$glmm), 2)
  
  # Extract AR1 phi value
  ar1_val <- tryCatch({
    vc <- VarCorr(models$glmm)
    ar1_corr <- attr(vc$cond[[2]], "correlation")
    as.character(round(ar1_corr[1, 2], 3))
  }, error = function(e) "NA")
  
  glmm_row <- data.frame(
    Model     = model_name,
    Type      = "GLMM",
    Predictor = rownames(glmm_coef)[2],
    Beta      = round(glmm_coef[2, 1], 4),
    SE        = round(glmm_coef[2, 2], 4),
    Statistic = round(glmm_coef[2, 3], 4),
    P_value   = ifelse(glmm_pval < 0.0001, "< 0.0001", as.character(round(glmm_pval, 4))),
    AIC       = as.character(glmm_aic),
    AR1       = ar1_val,
    Delta_AIC = "-"
  )
  
  bind_rows(glm_row, glmm_row)
}

# Recompile table
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



glm.tbl<-results_table%>% filter(Type=='GLM')
print(glm.tbl)
glmm.tbl<-results_table%>% filter(Type=='GLMM')
print(glmm.tbl)


