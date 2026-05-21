source("03_Scripts/Streams/analysis/data for analysis.R")
library(brms)
library(knitr)

NRESAMPLE <- 999999   # set to 9999 for quick dev runs; restore to 999999 for final results

site_order <- c("3", "5", "5a", "6", "7", "9", "13", "15")


# =============================================================================
# SETUP — PATHWAY DOMINANCE AND FULL-MODEL R²
# =============================================================================

# Pathway dominance: mean(internal / CO2_flux) per site
# Higher = internally dominant; lower = externally dominant
pathway_dominance <- int.ext %>%
  mutate(site = as.character(ID)) %>%
  filter(internal > 0, CO2_flux > 0) %>%
  mutate(int_frac = internal / CO2_flux) %>%
  group_by(site) %>%
  summarise(mean_int_frac = mean(int_frac, na.rm = TRUE), .groups = "drop")

cat("--- Pathway dominance per site (mean internal / CO2_flux) ---\n")
print(pathway_dominance)
cat("  > 0.5 = internally dominant;  < 0.5 = externally dominant\n\n")

# Full-model R² per site per pathway (one value per site × pathway)
full_raw <- read_csv("04_Output/stream/models/site_specific_results.csv")

full_r2 <- full_raw %>%
  select(site, pathway, R2) %>%
  distinct() %>%
  mutate(site = as.character(site)) %>%
  rename(R2_full = R2)

# Permutation Spearman (mirrors Ch1_CO2_spatial_analysis.R)
run_perm_spearman <- function(response_vec, predictor_vec, ID_labels,
                               resp_name, pred_name,
                               nresample = NRESAMPLE) {
  d <- data.frame(resp = response_vec, pred = predictor_vec, ID = ID_labels) %>%
    filter(!is.na(resp) & !is.na(pred))

  if (nrow(d) < 4) {
    warning("Too few observations: ", resp_name, " ~ ", pred_name)
    return(data.frame(response = resp_name, predictor = pred_name,
                      rho = NA, p_raw = NA, n = nrow(d)))
  }

  rho <- cor(d$resp, d$pred, method = "spearman")
  pt  <- spearman_test(resp ~ pred, data = d,
                       distribution = approximate(nresample = nresample))

  data.frame(
    response  = resp_name,
    predictor = pred_name,
    rho       = round(rho, 3),
    p_raw     = round(pvalue(pt)[[1]], 5),
    n         = nrow(d)
  )
}


# =============================================================================
# SECTION 1 — FULL MODEL: SLOPES ~ PATHWAY DOMINANCE
# =============================================================================
# Tests whether the slope of Q (lQ) or T (TempC) in the full model is
# correlated with pathway dominance across sites. For example, internally
# dominant sites may show steeper T slopes if metabolism drives more of the
# flux signal, while externally dominant sites may show steeper Q slopes.

full_slopes <- full_raw %>%
  select(site, pathway, indep.var, Estimate) %>%
  mutate(site = as.character(site)) %>%
  pivot_wider(names_from = indep.var, values_from = Estimate) %>%
  rename(slope_lQ = lQ, slope_TempC = TempC) %>%
  left_join(pathway_dominance, by = "site")

cat("--- Section 1: Full model slopes per site per pathway ---\n")
print(full_slopes %>% select(site, pathway, slope_lQ, slope_TempC, mean_int_frac),
      row.names = FALSE)

slopes_lint <- full_slopes %>% filter(pathway == "lint")
slopes_lext <- full_slopes %>% filter(pathway == "lext")

perm_slopes <- bind_rows(
  run_perm_spearman(slopes_lint$slope_lQ,    slopes_lint$mean_int_frac,
                    slopes_lint$site, "slope_lQ_lint",    "mean_int_frac"),
  run_perm_spearman(slopes_lext$slope_lQ,    slopes_lext$mean_int_frac,
                    slopes_lext$site, "slope_lQ_lext",    "mean_int_frac"),
  run_perm_spearman(slopes_lint$slope_TempC, slopes_lint$mean_int_frac,
                    slopes_lint$site, "slope_TempC_lint", "mean_int_frac"),
  run_perm_spearman(slopes_lext$slope_TempC, slopes_lext$mean_int_frac,
                    slopes_lext$site, "slope_TempC_lext", "mean_int_frac")
)
perm_slopes$p_BH <- round(p.adjust(perm_slopes$p_raw, method = "BH"), 5)
perm_slopes$sig  <- ifelse(perm_slopes$p_BH < 0.05, "*", "")

cat("\n--- Section 1 Spearman results ---\n")
print(perm_slopes, row.names = FALSE)
cat("\nNote: power at n=8 requires |rho| >= ~0.74 for p < 0.05 (two-tailed).\n\n")


# =============================================================================
# SECTION 2 — INTERACTION MODEL
# =============================================================================
# Adds a TempC × Q interaction term to the full model.
# R² sourced from Bayesian models (bayes_R2) stored as RDS files.
# Both lint and lext R² are extracted because the interaction is model-wide.
# delta_R2 = R²(interaction) - R²(full) per pathway per site.
# Positive delta = adding the interaction improves fit.

int_sites <- c("13", "15", "3", "5", "5a", "6", "7", "9")

int_r2 <- map_dfr(int_sites, function(s) {
  m  <- readRDS(paste0("04_Output/stream/models/Site Specific Interaction/", s, ".rds"))
  r2 <- bayes_R2(m)
  tibble(
    site    = s,
    pathway = c("lint", "lext"),
    R2_new  = c(r2["R2lint", "Estimate"], r2["R2lext", "Estimate"])
  )
})

interaction_df <- int_r2 %>%
  left_join(full_r2, by = c("site", "pathway")) %>%
  mutate(delta_R2 = R2_new - R2_full) %>%
  left_join(pathway_dominance, by = "site")

cat("--- Section 2: Interaction model delta R² ---\n")
print(interaction_df %>%
        select(site, pathway, R2_full, R2_new, delta_R2, mean_int_frac),
      row.names = FALSE)

int_lint <- interaction_df %>% filter(pathway == "lint")
int_lext <- interaction_df %>% filter(pathway == "lext")

perm_int <- bind_rows(
  run_perm_spearman(int_lint$delta_R2, int_lint$mean_int_frac,
                    int_lint$site, "delta_R2_lint", "mean_int_frac"),
  run_perm_spearman(int_lext$delta_R2, int_lext$mean_int_frac,
                    int_lext$site, "delta_R2_lext", "mean_int_frac")
)
perm_int$p_BH <- round(p.adjust(perm_int$p_raw, method = "BH"), 5)
perm_int$sig  <- ifelse(perm_int$p_BH < 0.05, "*", "")

cat("\n--- Section 2 Spearman results ---\n")
print(perm_int, row.names = FALSE)
cat("\nNote: power at n=8 requires |rho| >= ~0.74 for p < 0.05 (two-tailed).\n\n")


# =============================================================================
# SECTION 3 — DROP-Q MODEL
# =============================================================================
# Q removed from the full model for one pathway at a time.
# Only the R² of the pathway from which Q was dropped is used
# (pathway == dropped_from), per the user's intent.
# delta_R2 = R²(drop-Q) - R²(full) for the affected pathway only.
# Positive delta = dropping Q improved fit (Q was not contributing at this site).

dropQ_raw <- read_csv("04_Output/stream/models/dropQ.csv") %>%
  filter(!is.na(dropped_from),
         dropped_from %in% c("lint", "lext"),
         pathway == dropped_from) %>%
  group_by(site, pathway) %>%
  summarise(R2_new = first(R2), .groups = "drop") %>%
  mutate(site = as.character(site))

dropQ_df <- dropQ_raw %>%
  left_join(full_r2, by = c("site", "pathway")) %>%
  mutate(delta_R2 = R2_new - R2_full) %>%
  left_join(pathway_dominance, by = "site")

cat("--- Section 3: Drop-Q delta R² (affected pathway only) ---\n")
print(dropQ_df %>%
        select(site, pathway, R2_full, R2_new, delta_R2, mean_int_frac),
      row.names = FALSE)

dropQ_lint <- dropQ_df %>% filter(pathway == "lint")
dropQ_lext <- dropQ_df %>% filter(pathway == "lext")

perm_dropQ <- bind_rows(
  run_perm_spearman(dropQ_lint$delta_R2, dropQ_lint$mean_int_frac,
                    dropQ_lint$site, "delta_R2_lint_dropQ", "mean_int_frac"),
  run_perm_spearman(dropQ_lext$delta_R2, dropQ_lext$mean_int_frac,
                    dropQ_lext$site, "delta_R2_lext_dropQ", "mean_int_frac")
)
perm_dropQ$p_BH <- round(p.adjust(perm_dropQ$p_raw, method = "BH"), 5)
perm_dropQ$sig  <- ifelse(perm_dropQ$p_BH < 0.05, "*", "")

cat("\n--- Section 3 Spearman results ---\n")
print(perm_dropQ, row.names = FALSE)
cat("\nNote: power at n=8 requires |rho| >= ~0.74 for p < 0.05 (two-tailed).\n\n")


# =============================================================================
# SECTION 4 — DROP-T MODEL
# =============================================================================
# TempC removed from the full model for one pathway at a time.
# Only the R² of the pathway from which T was dropped is used
# (pathway == dropped_from).
# delta_R2 = R²(drop-T) - R²(full) for the affected pathway only.
# Positive delta = dropping T improved fit (T was not contributing at this site).

dropT_raw <- read_csv("04_Output/stream/models/dropT.csv") %>%
  filter(!is.na(dropped_from),
         dropped_from %in% c("lint", "lext"),
         pathway == dropped_from) %>%
  group_by(site, pathway) %>%
  summarise(R2_new = first(R2), .groups = "drop") %>%
  mutate(site = as.character(site))

dropT_df <- dropT_raw %>%
  left_join(full_r2, by = c("site", "pathway")) %>%
  mutate(delta_R2 = R2_new - R2_full) %>%
  left_join(pathway_dominance, by = "site")

cat("--- Section 4: Drop-T delta R² (affected pathway only) ---\n")
print(dropT_df %>%
        select(site, pathway, R2_full, R2_new, delta_R2, mean_int_frac),
      row.names = FALSE)

dropT_lint <- dropT_df %>% filter(pathway == "lint")
dropT_lext <- dropT_df %>% filter(pathway == "lext")

perm_dropT <- bind_rows(
  run_perm_spearman(dropT_lint$delta_R2, dropT_lint$mean_int_frac,
                    dropT_lint$site, "delta_R2_lint_dropT", "mean_int_frac"),
  run_perm_spearman(dropT_lext$delta_R2, dropT_lext$mean_int_frac,
                    dropT_lext$site, "delta_R2_lext_dropT", "mean_int_frac")
)
perm_dropT$p_BH <- round(p.adjust(perm_dropT$p_raw, method = "BH"), 5)
perm_dropT$sig  <- ifelse(perm_dropT$p_BH < 0.05, "*", "")

cat("\n--- Section 4 Spearman results ---\n")
print(perm_dropT, row.names = FALSE)
cat("\nNote: power at n=8 requires |rho| >= ~0.74 for p < 0.05 (two-tailed).\n\n")


# =============================================================================
# SUMMARY TABLE — ALL SECTIONS COMBINED
# =============================================================================

print_table <- function(title, note, data) {
  cat(paste0("\n", strrep("-", 70), "\n"))
  cat(paste0(title, "\n"))
  if (!is.null(note)) cat(paste0("Note: ", note, "\n"))
  cat(strrep("-", 70), "\n")
  print(kable(data, format = "simple", na = "—"))
  cat("\n")
}

perm_all <- bind_rows(
  perm_slopes %>% mutate(section = "Full model slopes"),
  perm_int    %>% mutate(section = "Interaction"),
  perm_dropQ  %>% mutate(section = "Drop Q"),
  perm_dropT  %>% mutate(section = "Drop T")
) %>%
  mutate(
    pathway  = ifelse(grepl("_lint", response), "Internal (lint)", "External (lext)"),
    variable = case_when(
      grepl("slope_lQ",    response) ~ "Q slope",
      grepl("slope_TempC", response) ~ "T slope",
      TRUE                           ~ "delta R²"
    )
  )

perm_all$p_BH_global <- round(p.adjust(perm_all$p_raw, method = "BH"), 5)
perm_all$sig_global  <- ifelse(perm_all$p_BH_global < 0.05, "*", "")

tbl_combined <- perm_all %>%
  select(section, pathway, variable, rho, p_raw, p_BH_global, sig_global, n) %>%
  rename(
    `Section`      = section,
    `Pathway`      = pathway,
    `Response`     = variable,
    `Spearman rho` = rho,
    `p (raw)`      = p_raw,
    `p (BH-adj)`   = p_BH_global,
    `Sig.`         = sig_global,
    `n sites`      = n
  ) %>%
  arrange(`Section`, `Pathway`, `Response`)

print_table(
  "TABLE — Full Model Slopes and Delta R² ~ Pathway Dominance",
  paste0(
    "IV: mean(internal/CO2_flux) per site. ",
    "Section 1 DV: Q or T slope from full model. ",
    "Sections 2-4 DV: R² change vs full model (drop sections use only affected pathway). ",
    "Positive delta = new model improves fit. BH correction across all 10 tests."
  ),
  tbl_combined
)
