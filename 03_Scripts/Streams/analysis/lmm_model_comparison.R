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

