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

# Split data by site
site_data <- df1 %>%
  group_by(ID) %>%
  group_split() %>%
  set_names(unique(df1$ID))
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

site_data <- df1 %>%
  group_by(ID) %>%
  group_split() %>%
  set_names(unique(df1$ID))

# Loop through each site and fit a bivariate model
site_models <- imap(site_data, function(data, site_name) {
  
  message("Fitting model for site: ", site_name)
  
  bf_int <- bf(lint ~ lQ + TempC)
  bf_ext <- bf(lext ~ lQ + TempC)
  
  brm(
    bf_int + bf_ext + set_rescor(TRUE),
    data   = data,
    family = student(),
    prior  = pri,
    cores  = 4,
    file   = paste0("04_Output/stream/models/Site Specific No Interaction/", site_name)
  )
})

model_path <- "04_Output/stream/models/Site Specific No Interaction/"

site_models <- list.files(model_path, pattern = "\\.rds$", full.names = TRUE) %>%
  set_names(gsub("\\.rds$", "", basename(.))) %>%
  map(readRDS)

extract_results <- function(mod, site_name) {
  
  fix_df <- as.data.frame(summary(mod)$fixed) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(!grepl("Intercept", parameter))
  
  r2 <- as.data.frame(bayes_R2(mod)) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `Q2.5`, `Q97.5`) %>%
    rename(`l-95% CI` = Q2.5, `u-95% CI` = Q97.5)
  
  sigma_df <- as.data.frame(summary(mod)$spec_pars) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(grepl("sigma", parameter))
  
  bind_rows(fix_df, r2, sigma_df) %>%
    mutate(site = site_name)
}

results_df <- imap_dfr(site_models, extract_results) 

r2<-results_df%>%filter(parameter %in% c('R2lint', 'R2lext'))%>%
  separate(parameter, into = c("R2", "pathway"), sep = "(?<=R2)")%>%
  select(site, pathway, Estimate)%>%
  rename(R2=Estimate)

sigma<-results_df%>%filter(parameter %in% c('sigma_lint', 'sigma_lext'))%>%
  separate(parameter, into = c("sigma", "pathway"), sep = "_")%>%
  select(site, pathway, Estimate)%>%
  rename(sigma=Estimate)

variance<-left_join(sigma, r2)

parameter<-results_df%>%
  separate(parameter, into = c("pathway", "indep.var"), sep = "_")%>%
  filter(pathway %in% c('lext', 'lint'))%>%
  rename(upper.bound=`u-95% CI`, lower.bound=`l-95% CI`,
         Estimate=Estimate)#%>%






no.pooling<-left_join(parameter, variance)%>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(no.pooling, "04_Output/stream/models/site_specific_results.csv")

no.pooling.long<-results_df%>%
  separate(parameter, into = c("pathway", "indep.var"), sep = "_")%>%
  filter(pathway %in% c('lint', 'lext'))%>%
  left_join(sigma)%>%left_join(r2)

write_csv(no.pooling.long, "04_Output/stream/models/site_specific_results.long.csv")


#no pooling, interaction, full model#########
site_models <- imap(site_data, function(data, site_name) {
  
  message("Fitting model for site: ", site_name)
  
  bf_int <- bf(lint ~ lQ * TempC)
  bf_ext <- bf(lext ~ lQ * TempC)
  
  brm(
    bf_int + bf_ext + set_rescor(TRUE),
    data   = data,
    family = student(),
    prior  = pri,
    cores  = 4,
    file   = paste0("04_Output/stream/models/Site Specific Interaction/", site_name)
  )
})

model_path <- "04_Output/stream/models/Site Specific Interaction/"

site_models <- list.files(model_path, pattern = "\\.rds$", full.names = TRUE) %>%
  set_names(gsub("\\.rds$", "", basename(.))) %>%
  map(readRDS)

extract_results <- function(mod, site_name) {
  
  fix_df <- as.data.frame(summary(mod)$fixed) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(!grepl("Intercept", parameter))
  
  r2 <- as.data.frame(bayes_R2(mod)) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `Q2.5`, `Q97.5`) %>%
    rename(`l-95% CI` = Q2.5, `u-95% CI` = Q97.5)
  
  sigma_df <- as.data.frame(summary(mod)$spec_pars) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(grepl("sigma", parameter))
  
  bind_rows(fix_df, r2, sigma_df) %>%
    mutate(site = site_name)
}

results_df <- imap_dfr(site_models, extract_results) 


parameter_T<-results_df%>%filter(!parameter %in% c('R2lint', 'R2lext'))%>%
  separate(parameter, into = c("pathway", "indep.var"), sep = "_")%>%
  filter(indep.var=='TempC')%>%
  rename(upper.bound_T=`u-95% CI`, lower.bound_T=`l-95% CI`,
         Estimate_T=Estimate)%>%
  select(-indep.var)


parameter_Q<-results_df%>%filter(!parameter %in% c('R2lint', 'R2lext'))%>%
  separate(parameter, into = c("pathway", "indep.var"), sep = "_")%>%
  filter(indep.var=='lQ')%>%
  rename(upper.bound_Q=`u-95% CI`, lower.bound_Q=`l-95% CI`,
         Estimate_Q=Estimate)%>%
  select(-indep.var)


r2<-results_df%>%filter(parameter %in% c('R2lint', 'R2lext'))%>%
  separate(parameter, into = c("R2", "pathway"), sep = "(?<=R2)")%>%
  select(site, pathway, Estimate)%>%
  rename(R2=Estimate)



interaction.no.pooling<-left_join(parameter_Q, parameter_T)%>%
  left_join(r2)%>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(interaction.no.pooling, "04_Output/stream/models/site_specific_results_interact.csv")

#drop models##########

site_data <- df1 %>%
  group_by(ID) %>%
  group_split() %>%
  set_names(unique(df1$ID))

site_models_noT <- imap(site_data, function(data, site_name) {
  message("Fitting model for site: ", site_name)
  bf_int_noT <- bf(lint ~ TempC)
  bf_ext_noT <- bf(lext ~ TempC)
  bf_int     <- bf(lint ~ lQ + TempC)
  bf_ext     <- bf(lext ~ lQ + TempC)
  
  fit_int_noT <- brm(
    bf_int_noT + bf_ext + set_rescor(TRUE),
    data    = data,
    family  = student(),
    prior   = pri,
    cores   = 4,
    control = list(adapt_delta = 0.95),
    file    = paste0("04_Output/stream/models/drop/noT/int_noT_", site_name)
  )
  fit_ext_noT <- brm(
    bf_ext_noT + bf_int + set_rescor(TRUE),
    data   = data,
    family = student(),
    prior  = pri,
    cores  = 4,
    file   = paste0("04_Output/stream/models/drop/noT/ext_noT_", site_name)
  )
  fit_noT <- brm(
    bf_int_noT + bf_ext_noT + set_rescor(TRUE),
    data   = data,
    family = student(),
    prior  = pri,
    cores  = 4,
    file   = paste0("04_Output/stream/models/drop/noT/boffa_Q_", site_name)
  )
  
  list(fit_int_noT = fit_int_noT, fit_ext_noT = fit_ext_noT, fit_noT = fit_noT)
})

site_models_noQ <- imap(site_data, function(data, site_name) {
  message("Fitting model for site: ", site_name)
  bf_int_noQ <- bf(lint ~ TempC)
  bf_ext_noQ <- bf(lext ~ TempC)
  bf_int     <- bf(lint ~ lQ + TempC)
  bf_ext     <- bf(lext ~ lQ + TempC)
  
  fit_int_noQ <- brm(
    bf_int_noQ + bf_ext + set_rescor(TRUE),
    data    = data,
    family  = student(),
    prior   = pri,
    cores   = 4,
    control = list(adapt_delta = 0.95),
    file    = paste0("04_Output/stream/models/drop/noQ/int_noQ_", site_name)
  )
  fit_ext_noQ <- brm(
    bf_ext_noQ + bf_int + set_rescor(TRUE),
    data   = data,
    family = student(),
    prior  = pri,
    cores  = 4,
    file   = paste0("04_Output/stream/models/drop/noQ/ext_noQ_", site_name)
  )
  fit_noQ <- brm(
    bf_int_noQ + bf_ext_noQ + set_rescor(TRUE),
    data   = data,
    family = student(),
    prior  = pri,
    cores  = 4,
    file   = paste0("04_Output/stream/models/drop/noQ/boffa_Q_", site_name)
  )
  
  list(fit_int_noQ = fit_int_noQ, fit_ext_noQ = fit_ext_noQ, fit_noQ = fit_noQ)
})


# drop T#################

model_path <- "04_Output/stream/models/drop/noT"

site_models_noT <- list.files(model_path, pattern = "\\.rds$", full.names = TRUE) %>%
  set_names(gsub("\\.rds$", "", basename(.))) %>%
  map(readRDS) %>%
  {
    sites <- unique(gsub("^(int_noT|ext_noT|boffa)_", "", names(.)))
    map(sites, function(s) {
      list(
        fit_int_noT = .[[paste0("int_noT_", s)]],
        fit_ext_noT = .[[paste0("ext_noT_", s)]],
        fit_noT     = .[[paste0("boffa_", s)]]
      )
    }) %>% set_names(sites)
  }

extract_results <- function(mod, site_name, drop = NULL) {
  fix_df <- as.data.frame(summary(mod)$fixed) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(!grepl("Intercept", parameter))
  r2 <- as.data.frame(bayes_R2(mod)) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `Q2.5`, `Q97.5`) %>%
    rename(`l-95% CI` = Q2.5, `u-95% CI` = Q97.5)
  sigma_df <- as.data.frame(summary(mod)$spec_pars) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(grepl("sigma", parameter))
  bind_rows(fix_df, r2, sigma_df) %>%
    mutate(site = site_name, drop = drop)
}


results_noT <- imap_dfr(site_models_noT, function(mods, site_name) {
  bind_rows(
    extract_results(mods$fit_int_noT, site_name, drop = "noT_lint"),
    extract_results(mods$fit_ext_noT, site_name, drop = "noT_lext"),
    extract_results(mods$fit_noT,     site_name, drop = "noT_both")
  )
})

  
R2_T<-results_noT%>%  
  filter(parameter %in% c('R2lint', 'R2lext'))%>%
  separate(parameter, into = c("R2", "pathway"), sep = "(?<=R2)")%>%
  select(pathway, Estimate, site, drop)%>%
  rename(R2=Estimate)

sigma_T<-results_noT%>%  
  filter(parameter %in% c('sigma_lint', 'sigma_lext'))%>%
  separate(parameter, into = c("R2", "pathway"), sep = "_")%>%
  select(pathway, Estimate, site, drop)%>%
  rename(sigma=Estimate)

variance<-left_join(R2_T, sigma_T)

wip<-results_noT%>%  
  filter(!parameter %in% c('sigma_lint', 'R2lext', 'R2lint', 'sigma_lext'))%>%
  separate(parameter, into = c("pathway", "indep"), sep = "_")%>%
  left_join(variance)


site_specific_results_long <- read_csv("04_Output/stream/models/site_specific_results.long.csv")%>%
  mutate(drop='full')%>%
  rename(indep=indep.var)
  
no_T<-rbind(wip, site_specific_results_long)%>%
  separate(drop, into = c("test", "dropped_from"), sep = "_")


write_csv(no_T, "04_Output/stream/models/dropT.csv")

# drop Q##############

model_path <- "04_Output/stream/models/drop/noQ"

site_models_noQ <- list.files(model_path, pattern = "\\.rds$", full.names = TRUE) %>%
  set_names(gsub("\\.rds$", "", basename(.))) %>%
  map(readRDS) %>%
  {
    sites <- unique(gsub("^(int_noQ_|ext_noQ_|boffa_Q_)", "", names(.)))
    map(sites, function(s) {
      list(
        fit_int_noQ = .[[paste0("int_noQ_", s)]],
        fit_ext_noQ = .[[paste0("ext_noQ_", s)]],
        fit_noQ     = .[[paste0("boffa_Q_", s)]]
      )
    }) %>% set_names(sites)
  }


extract_results <- function(mod, site_name, drop = NULL) {
  fix_df <- as.data.frame(summary(mod)$fixed) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(!grepl("Intercept", parameter))
  r2 <- as.data.frame(bayes_R2(mod)) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `Q2.5`, `Q97.5`) %>%
    rename(`l-95% CI` = Q2.5, `u-95% CI` = Q97.5)
  sigma_df <- as.data.frame(summary(mod)$spec_pars) %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, Estimate, `l-95% CI`, `u-95% CI`) %>%
    filter(grepl("sigma", parameter))
  bind_rows(fix_df, r2, sigma_df) %>%
    mutate(site = site_name, drop = drop)
}

results_noQ <- imap_dfr(site_models_noQ, function(mods, site_name) {
  bind_rows(
    extract_results(mods$fit_int_noQ, site_name, drop = "noQ_lint"),
    extract_results(mods$fit_ext_noQ, site_name, drop = "noQ_lext"),
    extract_results(mods$fit_noQ,     site_name, drop = "noQ_both")
  )
})


R2_T<-results_noQ%>%  
  filter(parameter %in% c('R2lint', 'R2lext'))%>%
  separate(parameter, into = c("R2", "pathway"), sep = "(?<=R2)")%>%
  select(pathway, Estimate, site, drop)%>%
  rename(R2=Estimate)

sigma_T<-results_noQ%>%  
  filter(parameter %in% c('sigma_lint', 'sigma_lext'))%>%
  separate(parameter, into = c("R2", "pathway"), sep = "_")%>%
  select(pathway, Estimate, site, drop)%>%
  rename(sigma=Estimate)

variance<-left_join(R2_T, sigma_T)

wip<-results_noQ%>%  
  filter(!parameter %in% c('sigma_lint', 'R2lext', 'R2lint', 'sigma_lext'))%>%
  separate(parameter, into = c("pathway", "indep"), sep = "_")%>%
  left_join(variance)


site_specific_results_long <- read_csv("04_Output/stream/models/site_specific_results.long.csv")%>%
  mutate(drop='full')%>%
  rename(indep=indep.var)

no_Q<-rbind(wip, site_specific_results_long)%>%
  separate(drop, into = c("test", "dropped_from"), sep = "_")

write_csv(no_Q, "04_Output/stream/models/dropQ.csv")

# noQ models
# results_noQ <- imap_dfr(site_models_noQ, function(mods, site_name) {
#   bind_rows(
#     extract_results(mods$fit_int_noQ, site_name, drop = "noQ_lext"),
#     extract_results(mods$fit_ext_noQ, site_name, drop = "noQ_lint"),
#     extract_results(mods$fit_noQ,     site_name, drop = "noQ_both")
#   )
# })
# 
# # combine
# all_results <- bind_rows(results_noT, results_noQ)

