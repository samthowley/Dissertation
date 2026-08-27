
library(tidyverse)
library(coin)         # permutation-based Spearman tests (project convention)
library(FSA)          # dunnTest — post-hoc for Kruskal-Wallis
library(pwr)           # post-hoc power for the Spearman/KW tests
library(flextable)
library(officer)

# ── Data preparation ──────────────────────────────────────────────────────────###########

meta <- read_csv("01_Raw_data/meta_analysis_extraction_GENERATED_v2.csv", show_col_types = FALSE)%>%
  select(1:4, 6:11, 14, 17:18)%>%
  mutate(
    Discharge_m3s = as.numeric(Discharge_m3s),
    Temperature_C=as.numeric(Temperature_C),
    CO2_flux_gCm2day=as.numeric(CO2_flux_gCm2day),
    Internal_Pathway_gCm2day=as.numeric(Internal_Pathway_gCm2day),
    External_Pathway_gCm2day=as.numeric(External_Pathway_gCm2day)
  )

# This study's own site data (int.ext.summary), needed to bring 4 of the 8
# Florida sites back into the analysis per explicit user instruction.
source("03_Scripts/Streams/analysis/data for analysis.R")
df <- meta %>%
  filter(!is.na(Internal_Pathway_gCm2day), !is.na(External_Pathway_gCm2day))%>%
  mutate(
    Internal.Contrib=(Internal_Pathway_gCm2day / (Internal_Pathway_gCm2day + External_Pathway_gCm2day))*100,
    Internal.Contrib=ifelse(Internal.Contrib>100, 100, Internal.Contrib),
    Internal.Contrib=ifelse(Internal.Contrib<0, 0, Internal.Contrib),
    Biome_Category = ifelse(Biome_Category %in% c("Alpine", "Boreal", "Arctic"),
                             "Cryospheric Zone", Biome_Category),
    Biome_Category = ifelse(Biome_Category %in% c("Mediterranean", "Arid"),
                            "Drylands", Biome_Category),
    Source_Water_Brief=ifelse(Source_Water_Brief=="Glacial/snow melt", "Surface runoff", Source_Water_Brief)
  )%>%
  filter(Source_Water_Brief != 'Regulated flow')
unique(df$Source_Water_Brief)

# Rows that are multiple reaches/time-periods of ONE river/site get collapsed to a
# paper-level average
collapse_dois <- c(
  "10.1029/2019JG005047",   # Horgby - 4 seasons, same river
  "10.5194/bg-22-4923-2025" # same site, 3 time periods
)

category_override <- c(
  "10.1029/2019JG005047" = "Groundwater-fed",
  "10.1029/2022JG006855" = "Wetland seepage"
)

collapsed <- df %>%
  filter(DOI %in% collapse_dois) %>%
  group_by(DOI) %>%
  summarise(
    Citation = first(Citation),
    Site_ID = "paper-average",
    Source_Water_Brief = ifelse(DOI[1] %in% names(category_override),
                                 category_override[DOI[1]], first(Source_Water_Brief)),
    Biome_Category = first(Biome_Category),
    across(c(Internal_Pathway_gCm2day, External_Pathway_gCm2day, Internal.Contrib, Temperature_C,
             pH, Mean_Annual_Precipitation_cm_yr, Discharge_m3s, CO2_flux_gCm2day),
           ~ mean(., na.rm = TRUE)),
    .groups = "drop"
  )

df_final <- df %>%
  filter(!DOI %in% collapse_dois) %>%
  bind_rows(collapsed) %>%
  mutate(
    Source_Water_Brief = factor(Source_Water_Brief),
    Biome_Category      = factor(Biome_Category)
  )

n_papers <- length(unique(df_final$DOI))

# ── Permutation Spearman helper ###########

run_perm_spearman <- function(response_vec, predictor_vec, resp_name, pred_name,
                               nresample = 999999) {
  d <- data.frame(resp = response_vec, pred = predictor_vec) %>%
    filter(!is.na(resp) & !is.na(pred))

  if (nrow(d) < 4) {
    warning("Too few obs: ", resp_name, " ~ ", pred_name)
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
# ── Kruskal-Wallis + Dunn's post-hoc helper ###################

run_kw <- function(response_vec, group_vec, resp_name, group_name) {
  d <- data.frame(resp = response_vec, grp = group_vec) %>%
    filter(!is.na(resp) & !is.na(grp)) %>%
    mutate(grp = droplevels(factor(grp)))

  kw <- kruskal.test(resp ~ grp, data = d)

  posthoc <- tryCatch(
    dunnTest(resp ~ grp, data = d, method = "bh")$res %>%
      rename(comparison = Comparison, Z = Z, p_raw = P.unadj, p_BH = P.adj) %>%
      mutate(response = resp_name, group_var = group_name),
    error = function(e) NULL
  )

  list(
    omnibus = data.frame(
      response  = resp_name,
      group_var = group_name,
      chi_sq    = round(unname(kw$statistic), 3),
      df        = unname(kw$parameter),
      p_raw     = round(kw$p.value, 5),
      n_groups  = nlevels(d$grp),
      n         = nrow(d)
    ),
    posthoc = posthoc
  )
}


# ── Visual constants (matches spearman_rank_analysis.R house style) ────────────

NEAR_SIG_COLOR <- "#FFF9C4"   # soft yellow  — near-significant only (p_raw < 0.05, p_BH >= 0.05)
SIG_COLOR      <- "#C8E6C9"   # soft green   — BH-significant (p_BH < 0.05)

# Applies two-tier shading to a flextable: green if p_BH < 0.05 (survives correction),
# yellow if only p_raw < 0.05 (nominal/near-significant, doesn't survive correction).
# row_lookup(data_row) must return the matching row index (or integer(0)) in tbl_data.
shade_significance <- function(ft, tbl_data, stats_df, cols, row_lookup) {
  for (k in seq_len(nrow(stats_df))) {
    row_k <- row_lookup(stats_df[k, ])
    if (length(row_k) == 0) next
    if (!is.na(stats_df$p_BH[k]) && stats_df$p_BH[k] < 0.05) {
      ft <- ft %>% bg(i = row_k, j = cols, bg = SIG_COLOR, part = "body")
    } else if (!is.na(stats_df$p_raw[k]) && stats_df$p_raw[k] < 0.05) {
      ft <- ft %>% bg(i = row_k, j = cols, bg = NEAR_SIG_COLOR, part = "body")
    }
  }
  ft
}

spearman_note <- paste0(
  "p = uncorrected permutation p-value; ",
  "ph = Benjamini-Hochberg adjusted p-value. ",
  "Shaded cells: green = p < 0.05 after BH correction (significant); ",
  "yellow = p < 0.05 before BH correction only (near-significant, does not survive correction)."
)

kw_note <- paste0(
  "p = uncorrected Kruskal-Wallis p-value; ",
  "ph = Benjamini-Hochberg adjusted p-value across the 6 response x factor tests. ",
  "Shaded cells: green = p < 0.05 after BH correction (significant); ",
  "yellow = p < 0.05 before BH correction only (near-significant, does not survive correction)."
)


# =============================================================================
# TABLE 1 — Spearman Rank: Temperature, Rainfall, pH, Discharge vs. Pathway Magnitude
# =============================================================================
cat("\n=== TABLE 1: Spearman Rank (Temp, Rainfall, pH, Discharge) ===\n")

perm_1 <- bind_rows(
  run_perm_spearman(df_final$Internal_Pathway_gCm2day, df_final$Temperature_C,
                     "Internal", "Temperature_C"),
  run_perm_spearman(df_final$External_Pathway_gCm2day, df_final$Temperature_C,
                     "External", "Temperature_C"),
  run_perm_spearman(df_final$Internal_Pathway_gCm2day, df_final$Mean_Annual_Precipitation_cm_yr,
                     "Internal", "Rainfall"),
  run_perm_spearman(df_final$External_Pathway_gCm2day, df_final$Mean_Annual_Precipitation_cm_yr,
                     "External", "Rainfall"),
  run_perm_spearman(df_final$Internal_Pathway_gCm2day, df_final$pH,
                     "Internal", "pH"),
  run_perm_spearman(df_final$External_Pathway_gCm2day, df_final$pH,
                     "External", "pH"),
  run_perm_spearman(df_final$Internal.Contrib, df_final$Temperature_C,
                     "Internal Contribution %", "Temperature_C"),
  run_perm_spearman(df_final$Internal.Contrib, df_final$Mean_Annual_Precipitation_cm_yr,
                     "Internal Contribution %", "Rainfall"),
  run_perm_spearman(df_final$Internal.Contrib, df_final$pH,
                     "Internal Contribution %", "pH"),
  run_perm_spearman(df_final$Internal_Pathway_gCm2day, df_final$Discharge_m3s,
                     "Internal", "Discharge_m3s"),
  run_perm_spearman(df_final$External_Pathway_gCm2day, df_final$Discharge_m3s,
                     "External", "Discharge_m3s"),
  run_perm_spearman(df_final$Internal.Contrib, df_final$Discharge_m3s,
                     "Internal Contribution %", "Discharge_m3s")
)

perm_1$p_BH <- round(p.adjust(perm_1$p_raw, method = "BH"), 3)
perm_1$sig  <- ifelse(perm_1$p_BH < 0.05, "*", "")
print(perm_1[, c("response", "predictor", "rho", "p_raw", "p_BH", "n", "sig")], row.names = FALSE)

predictor_labels_1 <- c(Temperature_C  = "Temperature (°C)",
                         Rainfall       = "Mean annual precipitation (cm yr⁻¹)",
                         pH             = "pH",
                         Discharge_m3s  = "Discharge (m3 s-1)")

response_levels_1 <- c("Internal", "External", "Internal Contribution %")

tbl_1_data <- perm_1 %>%
  mutate(Response  = factor(response, levels = response_levels_1),
         Predictor = unname(predictor_labels_1[predictor]),
         rho = round(rho, 3), p_raw = round(p_raw, 3), p_BH = round(p_BH, 3)) %>%
  select(Response, Predictor, rho, p_raw, p_BH, n) %>%
  mutate(Predictor = factor(Predictor, levels = unname(predictor_labels_1))) %>%
  arrange(Response, Predictor) %>%
  mutate(Response = as.character(Response), Predictor = as.character(Predictor))

ft_1 <- flextable(tbl_1_data) %>%
  set_header_labels(Response = "Pathway", Predictor = "Predictor",
                     rho = "rho", p_raw = "p", p_BH = "ph", n = "n") %>%
  merge_v(j = "Response") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1:2, align = "left",   part = "all") %>%
  align(j = 3:6, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  valign(j = 1, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,   width = 1.0) %>%
  width(j = 2,   width = 2.4) %>%
  width(j = 3:6, width = 0.7) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table 1. Do temperature, rainfall, pH, or discharge relate to internal vs. external CO2 ",
    "pathway magnitude, or the internal pathway's percent contribution to total CO2 flux, ",
    "across streams (literature meta-analysis)? Permutation Spearman rank correlation, ",
    "n = ", n_papers, " papers (same-river reaches averaged); ", nrow(df_final),
    " rows total, per-test n varies with each predictor's missing data (see n column). ",
    "999,999 resamples, BH-corrected across 12 tests."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Flux in g C m-2 day-1. Internal Contribution % = 100 x Internal / (Internal + ",
    "External), clamped to [0, 100]; higher values indicate internal-pathway dominance, ",
    "lower values indicate external-pathway dominance, 50 = pathways contribute equally. ",
    "Used in place of the log10(Internal/External) ratio from earlier drafts because that ",
    "ratio is undefined (or sign-flipped in a way that doesn't mean 'more external') when ",
    "either pathway is a negative net-uptake estimate, which forced dropping those rows; ",
    "the percent-contribution form stays interpretable at 0/100 for those cases instead. ",
    spearman_note
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)



ft_1 <- shade_significance(ft_1, tbl_1_data, perm_1, cols = 3:6, row_lookup = function(r) {
  which(tbl_1_data$Response == r$response &
        tbl_1_data$Predictor == unname(predictor_labels_1[r$predictor]))
})


# =============================================================================
# TABLE 2 — Kruskal-Wallis: Source Water & Biome vs. Pathway Magnitude
# =============================================================================
cat("\n=== TABLE 2: Kruskal-Wallis (Source Water, Biome) ===\n")

kw_internal_sw    <- run_kw(df_final$Internal_Pathway_gCm2day, df_final$Source_Water_Brief, "Internal", "Source water")
kw_external_sw    <- run_kw(df_final$External_Pathway_gCm2day, df_final$Source_Water_Brief, "External", "Source water")
kw_internal_biome <- run_kw(df_final$Internal_Pathway_gCm2day, df_final$Biome_Category,      "Internal", "Biome")
kw_external_biome <- run_kw(df_final$External_Pathway_gCm2day, df_final$Biome_Category,      "External", "Biome")
kw_ratio_sw        <- run_kw(df_final$Internal.Contrib, df_final$Source_Water_Brief, "Internal Contribution %", "Source water")
kw_ratio_biome     <- run_kw(df_final$Internal.Contrib, df_final$Biome_Category,     "Internal Contribution %", "Biome")

kw_2 <- bind_rows(kw_internal_sw$omnibus, kw_external_sw$omnibus,
                   kw_internal_biome$omnibus, kw_external_biome$omnibus,
                   kw_ratio_sw$omnibus, kw_ratio_biome$omnibus)

kw_2$p_BH <- round(p.adjust(kw_2$p_raw, method = "BH"), 3)
kw_2$sig  <- ifelse(kw_2$p_BH < 0.05, "*", "")
print(kw_2[, c("response", "group_var", "chi_sq", "df", "p_raw", "p_BH", "n_groups", "n", "sig")],
      row.names = FALSE)

tbl_2_data <- kw_2 %>%
  mutate(Response = factor(response, levels = response_levels_1), `Grouping factor` = group_var,
         `H` = chi_sq, df = df, p_raw = round(p_raw, 3), p_BH = round(p_BH, 3)) %>%
  select(Response, `Grouping factor`, H, df, p_raw, p_BH, n_groups, n) %>%
  arrange(Response, `Grouping factor`) %>%
  mutate(Response = as.character(Response))

ft_2 <- flextable(tbl_2_data) %>%
  set_header_labels(Response = "Pathway", `Grouping factor` = "Grouping factor",
                     H = "H", df = "df", p_raw = "p", p_BH = "ph",
                     n_groups = "k groups", n = "n") %>%
  merge_v(j = "Response") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1:2, align = "left",   part = "all") %>%
  align(j = 3:8, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  valign(j = 1, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,   width = 1.0) %>%
  width(j = 2,   width = 1.6) %>%
  width(j = 3:8, width = 0.65) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table 2. Does dominant source water or biome relate to internal vs. external CO2 ",
    "pathway magnitude, or the internal pathway's percent contribution to total CO2 flux, ",
    "across streams (literature meta-analysis)? Kruskal-Wallis rank-sum test, same-river ",
    "reaches averaged, BH-corrected across 6 tests."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Flux in g C m-2 day-1. Internal Contribution % = 100 x Internal / (Internal + ",
    "External), clamped to [0, 100] (n = ", nrow(df_final), "); higher values indicate ",
    "internal-pathway dominance, lower values indicate external-pathway dominance. Source ",
    "water = Source_Water_Brief (", nlevels(df_final$Source_Water_Brief), " levels); Biome = ",
    "Biome_Category (", nlevels(df_final$Biome_Category), " levels; Alpine/Boreal/Arctic pooled ",
    "into one 'Cryospheric Zone' category, each individually a 1-2-paper group). Small groups ",
    "(e.g. Arid) limit power for these factors -- treat as exploratory. ", kw_note
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

ft_2 <- shade_significance(ft_2, tbl_2_data, kw_2, cols = 3:8, row_lookup = function(r) {
  which(tbl_2_data$Response == r$response &
        tbl_2_data$`Grouping factor` == r$group_var)
})


# =============================================================================
# TABLE 3 — Dunn's Post-hoc Pairwise Comparisons (Source Water & Biome)
# =============================================================================
cat("\n=== TABLE 3: Dunn's Post-hoc Comparisons ===\n")

posthoc_3 <- bind_rows(kw_internal_sw$posthoc, kw_external_sw$posthoc,
                        kw_internal_biome$posthoc, kw_external_biome$posthoc,
                        kw_ratio_sw$posthoc, kw_ratio_biome$posthoc)

tbl_3_data <- posthoc_3 %>%
  mutate(Response = factor(response, levels = response_levels_1), `Grouping factor` = group_var,
         Comparison = comparison, Z = round(Z, 3),
         p_raw = round(p_raw, 3), p_BH = round(p_BH, 3)) %>%
  select(Response, `Grouping factor`, Comparison, Z, p_raw, p_BH) %>%
  arrange(Response, `Grouping factor`, p_BH) %>%
  mutate(Response = as.character(Response))

ft_3 <- flextable(tbl_3_data) %>%
  set_header_labels(Response = "Pathway", `Grouping factor` = "Grouping factor",
                     Comparison = "Comparison", Z = "Z", p_raw = "p", p_BH = "ph") %>%
  merge_v(j = c("Response", "Grouping factor")) %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  align(j = 1:3, align = "left",   part = "all") %>%
  align(j = 4:6, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  valign(j = 1:2, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,   width = 0.9) %>%
  width(j = 2,   width = 1.3) %>%
  width(j = 3,   width = 2.2) %>%
  width(j = 4:6, width = 0.7) %>%
  height_all(height = 0.22) %>%
  add_header_lines(paste0(
    "Table 3. Dunn's pairwise post-hoc comparisons following the Kruskal-Wallis tests in ",
    "Table 2 (Benjamini-Hochberg adjusted within each response x factor combination). See ",
    "Table 6 for a category-level rollup of these same comparisons."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Comparisons involving groups with very small n (e.g. Regulated flow, Arid) ",
    "are exploratory only. p = uncorrected Dunn's test p-value; ph = Benjamini-Hochberg ",
    "adjusted p-value, corrected separately within each response x grouping-factor block ",
    "(i.e. within the set of pairwise comparisons for a given pathway x factor combination, ",
    "not across the whole table). Shaded cells: green = p < 0.05 after this within-block BH ",
    "correction (significant); yellow = p < 0.05 before correction only (near-significant)."
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

ft_3 <- shade_significance(ft_3, tbl_3_data, posthoc_3, cols = 4:6, row_lookup = function(r) {
  which(tbl_3_data$Response == r$response &
        tbl_3_data$`Grouping factor` == r$group_var &
        tbl_3_data$Comparison == r$comparison)
})


# =============================================================================
# TABLE 4 — Post-hoc (Observed) Statistical Power
# =============================================================================
cat("\n=== TABLE 4: Post-hoc Power ===\n")

# Spearman power via the Pearson-r approximation (pwr has no dedicated Spearman
# power function; this is standard practice and adequate for an exploratory report).
power_spearman <- function(rho, n, sig.level = 0.05) {
  if (is.na(rho) || is.na(n) || n < 4 || abs(rho) >= 1) return(NA_real_)
  tryCatch(pwr.r.test(n = n, r = abs(rho), sig.level = sig.level)$power,
           error = function(e) NA_real_)
}

kw_effect_and_power <- function(chi_sq, df, n, sig.level = 0.05) {
  k <- df + 1
  if (is.na(chi_sq) || n <= k) return(c(f = NA_real_, power = NA_real_))
  eta_sq <- max((chi_sq - k + 1) / (n - k), 0)
  f <- sqrt(eta_sq / (1 - eta_sq))
  if (!is.finite(f) || f <= 0) return(c(f = round(f, 3), power = NA_real_))
  power <- tryCatch(pwr.anova.test(k = k, n = n / k, f = f, sig.level = sig.level)$power,
                     error = function(e) NA_real_)
  c(f = round(f, 3), power = round(power, 3))
}

power_1 <- perm_1 %>%
  mutate(power = mapply(power_spearman, rho, n))

kw_power_mat <- t(mapply(kw_effect_and_power, kw_2$chi_sq, kw_2$df, kw_2$n))
power_2 <- kw_2 %>%
  mutate(f = as.numeric(kw_power_mat[, "f"]), power = as.numeric(kw_power_mat[, "power"]))

print(power_1[, c("response", "predictor", "rho", "n", "power")], row.names = FALSE)
print(power_2[, c("response", "group_var", "chi_sq", "f", "n", "power")], row.names = FALSE)

tbl_4_data <- bind_rows(
  power_1 %>%
    transmute(Test = "Spearman", Response = factor(response, levels = response_levels_1),
              `Predictor / factor` = unname(predictor_labels_1[predictor]),
              `Effect size` = paste0("rho=", sprintf("%.2f", rho)),
              n, Power = round(power, 2)),
  power_2 %>%
    transmute(Test = "Kruskal-Wallis", Response = factor(response, levels = response_levels_1),
              `Predictor / factor` = group_var,
              `Effect size` = ifelse(is.na(f), NA_character_, paste0("f=", sprintf("%.2f", f))),
              n, Power = round(power, 2))
) %>%
  arrange(Test, Response, `Predictor / factor`) %>%
  mutate(Response = as.character(Response))

ft_4 <- flextable(tbl_4_data) %>%
  set_header_labels(Test = "Test", Response = "Pathway", `Predictor / factor` = "Predictor / factor",
                     `Effect size` = "Effect size", n = "n", Power = "Power") %>%
  merge_v(j = c("Test", "Response")) %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1:3, align = "left",   part = "all") %>%
  align(j = 4:6, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  valign(j = 1:2, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,   width = 1.1) %>%
  width(j = 2,   width = 1.0) %>%
  width(j = 3,   width = 2.0) %>%
  width(j = 4:6, width = 0.75) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table 4. Post-hoc (observed) statistical power for the Tables 1-2 tests, at ",
    "alpha = 0.05. Reported for transparency given this is an exploratory analysis, ",
    "not as a design/validity check."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Spearman power approximated via the Pearson-r method (pwr.r.test) using the ",
    "observed rho. Kruskal-Wallis power approximated by converting H to an ANOVA-equivalent ",
    "eta-squared, then Cohen's f (pwr.anova.test), assuming balanced groups of size n/k -- ",
    "an approximation given this data's uneven group sizes. Shaded cells: power < 0.80 ",
    "(conventional adequacy threshold, Cohen 1988). Post-hoc power is mathematically tied ",
    "to the test's own p-value and should not be used to argue a non-significant result ",
    "would have been significant with more power."
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

UNDERPOWERED_COLOR <- "#FDEAEA"   # soft red — power < 0.80
low_power_rows <- which(!is.na(tbl_4_data$Power) & tbl_4_data$Power < 0.80)
if (length(low_power_rows) > 0) {
  ft_4 <- ft_4 %>% bg(i = low_power_rows, j = 6, bg = UNDERPOWERED_COLOR, part = "body")
}


# =============================================================================
# TABLE 5 — Papers Included and Their Contribution to Tables 1-2 (pseudoreplication)
# =============================================================================
cat("\n=== TABLE 5: Papers Included ===\n")

categories_5 <- df_final %>%
  group_by(DOI) %>%
  summarise(
    Biome        = paste(sort(unique(as.character(Biome_Category))), collapse = "; "),
    Source_Water = paste(sort(unique(as.character(Source_Water_Brief))), collapse = "; "),
    .groups = "drop"
  )
pathway_method <- c(
  "10.1111/gcb.14895"              = "O2 metabolism (GPP/ER, RQ=1.0): Internal solved first; External = CO2_flux - Internal (residual).",
  "10.1029/2022JG007048"           = "O2 metabolism (GPP/ER, RQ=1.0): Internal solved first; External = CO2_flux - Internal (residual).",
  "10.1029/2022JG006855"           = "No metabolism data. Both terms derived directly and simultaneously from the paper's own reach-level source split applied to CO2_flux: External = 81% (porewater), Internal = 17% (in-stream metabolism); remaining 2% (CH4 oxidation) excluded from both.",
  "10.1002/lno.12226"              = "O2 metabolism (GPP/ER, RQ=1.0): Internal solved first; External = CO2_flux - Internal (residual).",
  "10.1029/2022JG006954"           = "External solved first: RIP+TER+AQU, the paper's own published riparian/terrestrial/aquifer mass-balance partition (not a residual). Internal from GPP/AR via the O2 formula -- both independently derived; CO2_flux mass-balance residual reported as a check, not forced to zero.",
  "10.5194/bg-19-137-2022"         = "Internal = directly measured heterotrophic respiration (serum-bottle incubation), available at only 2 of 6 stations; External = CO2_flux - Internal (residual) where Internal available, else both NOT REPORTED.",
  "10.1002/lno.70372"              = "O2 metabolism (GPP/ER, RQ=1.0), reach-level (not split by site); External = residual. Same Internal/External/CO2_flux value applied to both the upstream and downstream site rows.",
  "10.1007/s10533-022-00954-4"     = "Internal = paper's own median NEP; External = paper's own median GWCO2 (groundwater CO2 flux) -- both independently measured, neither back-calculated from the other.",
  "10.1002/lno.11134"              = "External solved first: midpoint of the paper's own stated 34-66% groundwater CO2-contribution range x CO2_flux; Internal = CO2_flux - External (residual). Paper reports percentage ranges only, no absolute fluxes.",
  "10.1029/2018JG004912"           = "External solved first: paper's own 222Rn-traced groundwater CO2 flux (radon mass balance, not the GPP/ER formula); Internal = CO2_flux - External (residual).",
  "10.1016/j.jhydrol.2014.03.070"  = "NOT REPORTED. No metabolism (GPP/ER/NEP) data -- a delta13C-DIC geochemical study splitting carbon into biogenic vs. geogenic sources, both 'external' under this framework; computing a split would fabricate a number the paper doesn't report.",
  "10.5194/bg-22-4923-2025"        = "Already carbon-referenced by the authors: Internal = -NEP (paper's own sign convention, cross-checked against reported GPP/ER); External = paper's own stated 'External CO2 = FCO2 + NEP' (algebraically identical to the CO2_flux - Internal residual).",
  "10.1002/lno.70016"              = "Already carbon-referenced by the authors (1:1 O2:C conversion, paper's explicit choice): Internal = -NEP; External = CO2_flux - Internal (residual).",
  "10.1029/2019JG005047"           = "External = 100% of CO2_flux: paper's isotope mixing model partitions the entire CO2 source between soil respiration and carbonate weathering, both external. Internal NOT REPORTED -- paper only qualitatively concludes in-stream respiration 'contributed only marginally'.",
  "10.1002/lol2.10195"             = "O2 metabolism, raw streamMetabolizer GPP/ER (RQ=1.0), QA-filtered to physically valid days; CO2_flux computed via Fick's law (K600 + Schmidt number). Internal solved first; External = residual.",
  "10.1002/lno.12334"              = "Already carbon-referenced by the authors (PQ/RQ Monte Carlo): Internal = paper's own median NEP; External = CO2_flux - Internal (residual) -- negative here, a real reported finding (excess internal CO2 exported downstream as dissolved CO2/DIC rather than evading locally), not floored at zero.",
  "10.1016/j.scitotenv.2021.146230" = "Internal (O2 mass balance) and External (222Rn/water mass balance x groundwater CO2 concentration) both independently modeled by the paper's authors, neither a residual of the other; CO2_flux is a third independent term (Fick's law). Internal+External exceeds CO2_flux here -- paper attributes the gap to carbonate buffering and downstream dissolved-CO2 export.",
  "10.1016/j.ecolind.2021.108136"  = "Already carbon-referenced by the authors: Internal = -NEP (paper's own whole-study value); External = CO2_flux - Internal (residual).",
  "This Paper"                     = "This study's own 4 Florida sites (of 8 total; 5, 6, 9, 13 only, per explicit user instruction). Internal/External computed in the site-level processing script (int.ext.summary, data for analysis.R), not this literature-extraction pipeline."
)

papers_5 <- df %>%
  group_by(DOI) %>%
  summarise(Citation = first(Citation), n_reaches = n(), .groups = "drop") %>%
  left_join(categories_5, by = "DOI") %>%
  mutate(
    Rows_in_analysis = ifelse(DOI %in% collapse_dois, 1L, n_reaches),
    Collapsed = ifelse(DOI %in% collapse_dois,
                        "Yes — averaged to 1 paper-level row",
                        "No — reaches/time-periods kept separate"),
    Pathway_Method = unname(pathway_method[DOI])
  ) %>%
  arrange(desc(Rows_in_analysis), desc(n_reaches))

print(papers_5, n = Inf)

tbl_5_data <- papers_5 %>%
  transmute(Citation, DOI, Biome, `Source water` = Source_Water,
            `Reaches/time-periods extracted` = n_reaches,
            `Rows in Tables 1-2` = Rows_in_analysis, `Collapsed to paper average?` = Collapsed,
            `Internal/External estimation method` = Pathway_Method)

ft_5 <- flextable(tbl_5_data) %>%
  set_header_labels(Citation = "Citation", DOI = "DOI", Biome = "Biome",
                     `Source water` = "Source water",
                     `Reaches/time-periods extracted` = "Reaches/time-periods extracted",
                     `Rows in Tables 1-2` = "Rows in Tables 1-2",
                     `Collapsed to paper average?` = "Collapsed to paper average?",
                     `Internal/External estimation method` = "Internal/External estimation method") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  align(j = 1:4, align = "left",   part = "all") %>%
  align(j = 5:7, align = "center", part = "all") %>%
  align(j = 8,   align = "left",   part = "all") %>%
  bold(part = "header") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,   width = 1.4) %>%
  width(j = 2,   width = 1.1) %>%
  width(j = 3,   width = 0.9) %>%
  width(j = 4,   width = 1.2) %>%
  width(j = 5,   width = 0.7) %>%
  width(j = 6,   width = 0.7) %>%
  width(j = 7,   width = 1.3) %>%
  width(j = 8,   width = 2.8) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table 5. Papers contributing to the Internal/External site-level tests (Tables 1-2), ",
    "their biome/source-water categorization, replication structure, and how each paper's ",
    "Internal/External pathway values were originally derived. n = ", n_papers,
    " papers; ", nrow(df_final), " rows enter the Internal/External tests after paper-level ",
    "collapsing."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Biome/Source water = the category (or categories) actually assigned to that ",
    "paper's row(s) in Tables 1-2, i.e. after collapsing/category-override, not the raw ",
    "per-reach extraction. Six papers report reaches that split across two different source-",
    "water categories (e.g. one reach glacial/snow-melt-fed, another groundwater-fed further ",
    "downstream) -- both categories are listed and both rows remain in the Kruskal-Wallis ",
    "tests, which is part of the reach-level heterogeneity this analysis is designed to keep ",
    "visible rather than average away. Reaches/time-periods extracted = number of site/reach/",
    "time-period rows for that paper in the raw extraction with valid Internal and External ",
    "pathway values. Rows in Tables 1-2 = number of rows that paper contributes to the ",
    "Internal/External Spearman and Kruskal-Wallis tests after collapsing. Two papers ",
    "reporting repeated seasonal sampling of the SAME river reach were collapsed to one ",
    "paper-level average row each to avoid pseudoreplicating those tests; all other multi-row ",
    "papers report genuinely distinct named rivers/reaches and are retained as separate rows ",
    "by design (see comment above collapse_dois in the analysis script). Consequently, Tables ",
    "1-2's n is not n independent papers -- several papers each contribute multiple, non-",
    "independent rows, and results should be interpreted with that in mind. Internal/External ",
    "estimation method = condensed from meta_analysis_pipeline.R (the upstream extraction ",
    "script): most papers use its default O2-metabolism formula (Internal solved first from ",
    "GPP/ER, RQ=1.0, External = CO2_flux - Internal as a residual); papers noted 'External ",
    "solved first' instead derive External from an independently measured/modeled terrestrial ",
    "or groundwater CO2 term and back-calculate Internal as the residual; papers noted 'both ",
    "independently' measure/model Internal and External separately, so their sum need not ",
    "equal CO2_flux; 'NOT REPORTED' papers lack the data to compute a split without ",
    "fabricating a number. See that script's per-paper comments for full derivations and ",
    "exact source citations (table/page numbers)."
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

heavy_rows <- which(tbl_5_data$`Rows in Tables 1-2` > 1)
if (length(heavy_rows) > 0) {
  ft_5 <- ft_5 %>% bg(i = heavy_rows, j = 6, bg = NEAR_SIG_COLOR, part = "body")
}


# =============================================================================
# TABLE 6 — Category Rollup: Which Categories Enhance/Inhibit Each Pathway
# =============================================================================
cat("\n=== TABLE 6: Category Rollup ===\n")

category_stats <- function(value_vec, group_vec, resp_name, group_name) {
  data.frame(value = value_vec, category = group_vec) %>%
    filter(!is.na(value), !is.na(category)) %>%
    group_by(category) %>%
    summarise(n = n(), median = round(median(value), 3),
              min = round(min(value), 3), max = round(max(value), 3), .groups = "drop") %>%
    mutate(response = resp_name, group_var = group_name, category = as.character(category))
}

group_stats_6 <- bind_rows(
  category_stats(df_final$Internal_Pathway_gCm2day, df_final$Biome_Category,        "Internal", "Biome"),
  category_stats(df_final$Internal_Pathway_gCm2day, df_final$Source_Water_Brief,    "Internal", "Source water"),
  category_stats(df_final$External_Pathway_gCm2day, df_final$Biome_Category,        "External", "Biome"),
  category_stats(df_final$External_Pathway_gCm2day, df_final$Source_Water_Brief,    "External", "Source water"),
  category_stats(df_final$Internal.Contrib,         df_final$Biome_Category,        "Internal Contribution %", "Biome"),
  category_stats(df_final$Internal.Contrib,         df_final$Source_Water_Brief,    "Internal Contribution %", "Source water")
)

fmt_pair <- function(opponent, Z, p_raw, p_BH, tier) {
  if (tier == "confirmed") sprintf("%s (Z=%.2f, ph=%.3f)", opponent, Z, p_BH)
  else                     sprintf("%s (Z=%.2f, p=%.3f)~", opponent, Z, p_raw)
}

pairwise_long <- posthoc_3 %>%
  mutate(groupA = trimws(sub(" - .*$", "", comparison)),
         groupB = trimws(sub("^.* - ", "", comparison))) %>%
  { bind_rows(
      transmute(., response, group_var, category = groupA, opponent = groupB,
                win = Z > 0, Z = abs(Z), p_raw, p_BH),
      transmute(., response, group_var, category = groupB, opponent = groupA,
                win = Z < 0, Z = abs(Z), p_raw, p_BH)
    ) } %>%
  mutate(tier = case_when(p_BH < 0.05 ~ "confirmed", p_raw < 0.05 ~ "suggestive", TRUE ~ "none")) %>%
  filter(tier != "none") %>%
  rowwise() %>%
  mutate(entry = fmt_pair(opponent, Z, p_raw, p_BH, tier)) %>%
  ungroup() %>%
  arrange(p_BH, p_raw)

# Plain base-R lookups (deliberately not dplyr::filter chained inside a rowwise mutate --
# nesting a filter() on a second data frame inside a rowwise() mutate() creates a tidy-eval
# name collision when the join-key columns share names across the two frames).
pw_resp <- pairwise_long$response
pw_gvar <- pairwise_long$group_var
pw_cat  <- pairwise_long$category
pw_win  <- pairwise_long$win
pw_tier <- pairwise_long$tier

rollup_cell <- function(resp_val, gvar_val, cat_val, want_win, want_tier) {
  idx <- pw_resp == resp_val & pw_gvar == gvar_val & pw_cat == cat_val &
         pw_win == want_win & pw_tier == want_tier
  if (!any(idx)) return(NA_character_)
  paste(pairwise_long$entry[idx], collapse = "; ")
}
count_cell <- function(resp_val, gvar_val, cat_val, want_win, want_tier) {
  sum(pw_resp == resp_val & pw_gvar == gvar_val & pw_cat == cat_val &
      pw_win == want_win & pw_tier == want_tier)
}

tbl_6_data <- group_stats_6
tbl_6_data$n_conf_higher <- mapply(count_cell, tbl_6_data$response, tbl_6_data$group_var,
                                    tbl_6_data$category, TRUE, "confirmed")
tbl_6_data$n_conf_lower  <- mapply(count_cell, tbl_6_data$response, tbl_6_data$group_var,
                                    tbl_6_data$category, FALSE, "confirmed")
tbl_6_data$n_sugg_higher <- mapply(count_cell, tbl_6_data$response, tbl_6_data$group_var,
                                    tbl_6_data$category, TRUE, "suggestive")
tbl_6_data$n_sugg_lower  <- mapply(count_cell, tbl_6_data$response, tbl_6_data$group_var,
                                    tbl_6_data$category, FALSE, "suggestive")

higher_conf <- mapply(rollup_cell, tbl_6_data$response, tbl_6_data$group_var,
                       tbl_6_data$category, TRUE, "confirmed")
higher_sugg <- mapply(rollup_cell, tbl_6_data$response, tbl_6_data$group_var,
                       tbl_6_data$category, TRUE, "suggestive")
lower_conf  <- mapply(rollup_cell, tbl_6_data$response, tbl_6_data$group_var,
                       tbl_6_data$category, FALSE, "confirmed")
lower_sugg  <- mapply(rollup_cell, tbl_6_data$response, tbl_6_data$group_var,
                       tbl_6_data$category, FALSE, "suggestive")

paste_dash <- function(a, b) {
  parts <- c(a, b)[!is.na(c(a, b))]
  if (length(parts) == 0) "—" else paste(parts, collapse = "; ")
}
tbl_6_data$`Significantly/suggestively higher than` <- mapply(paste_dash, higher_conf, higher_sugg)
tbl_6_data$`Significantly/suggestively lower than`  <- mapply(paste_dash, lower_conf,  lower_sugg)

tbl_6_data$Verdict <- with(tbl_6_data, case_when(
  n_conf_higher > 0 & n_conf_lower == 0  ~ "Enhancing",
  n_conf_lower  > 0 & n_conf_higher == 0 ~ "Inhibiting",
  n_conf_higher > 0 & n_conf_lower  > 0  ~ "Mixed (confirmed both directions)",
  n_sugg_higher > 0 & n_sugg_lower == 0  ~ "Suggestive enhancing (not BH-confirmed)",
  n_sugg_lower  > 0 & n_sugg_higher == 0 ~ "Suggestive inhibiting (not BH-confirmed)",
  n_sugg_higher > 0 & n_sugg_lower  > 0  ~ "Mixed (suggestive only)",
  TRUE ~ "No signal"
))

# Ordinal scale from most enhancing to most inhibiting, "No signal" in the middle,
# so rows can be sorted by verdict tier rather than raw median.
verdict_levels <- c(
  "Enhancing",
  "Suggestive enhancing (not BH-confirmed)",
  "Mixed (confirmed both directions)",
  "Mixed (suggestive only)",
  "No signal",
  "Suggestive inhibiting (not BH-confirmed)",
  "Inhibiting"
)

# Order rows within each Pathway x Grouping factor block by verdict tier, most
# enhancing first, through No signal, to most inhibiting last; median (highest
# first) breaks ties within a tier.
tbl_6_data <- tbl_6_data %>%
  mutate(Response = factor(response, levels = response_levels_1),
         `Grouping factor` = group_var,
         Category = paste0(category, " (n=", n, ")"),
         Median = sprintf("%.2f", median),
         .verdict_rank = match(Verdict, verdict_levels)) %>%
  select(Response, `Grouping factor`, Category, Median, median, min, max,
         `Significantly/suggestively higher than`, `Significantly/suggestively lower than`,
         Verdict, .verdict_rank) %>%
  arrange(Response, `Grouping factor`, .verdict_rank, desc(median)) %>%
  select(-.verdict_rank) %>%
  mutate(Response = as.character(Response))

print(tbl_6_data[, c("Response","Grouping factor","Category","median","Verdict")], n = Inf)

ft_6 <- flextable(tbl_6_data, col_keys = c("Response","Grouping factor","Category","Median",
                                            "Significantly/suggestively higher than",
                                            "Significantly/suggestively lower than","Verdict")) %>%
  set_header_labels(Response = "Pathway", `Grouping factor` = "Grouping factor", Category = "Category (n)",
                     Median = "Median",
                     `Significantly/suggestively higher than` = "Higher than",
                     `Significantly/suggestively lower than` = "Lower than", Verdict = "Verdict") %>%
  compose(j = "Median", value = as_paragraph(
    as_chunk(sprintf("%.2f", median)),
    as_chunk(sprintf(" (%.2f–%.2f)", min, max), props = fp_text(font.size = 6, italic = TRUE))
  )) %>%
  merge_v(j = c("Response", "Grouping factor")) %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  align(j = 1:4, align = "left", part = "all") %>%
  align(j = 5:7, align = "left", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  valign(j = 1:2, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1, width = 0.8) %>%
  width(j = 2, width = 1.0) %>%
  width(j = 3, width = 1.1) %>%
  width(j = 4, width = 1.0) %>%
  width(j = 5, width = 1.8) %>%
  width(j = 6, width = 1.8) %>%
  width(j = 7, width = 1.3) %>%
  height_all(height = 0.3) %>%
  add_header_lines(paste0(
    "Table 6. Category-level rollup of the Dunn's pairwise post-hoc comparisons from Table 3: ",
    "for each category, its median, which other categories it is significantly or suggestively ",
    "higher/lower than, and the resulting verdict. Rows are ordered by verdict, from most ",
    "enhancing to No signal to most inhibiting (median, highest first, breaks ties within a ",
    "verdict tier), within each pathway x factor block."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Median in g C m-2 day-1 for Internal/External, percent for Internal Contribution %; ",
    "the smaller italicized figure in parentheses is that category's observed min-max range. ",
    "Entries with no ~ are BH-significant (p_BH < 0.05, corrected within each pathway x factor ",
    "block, as in Table 3); entries marked ~ are nominally significant only (uncorrected p < ",
    "0.05) and do not survive correction -- treat these as suggestive, not confirmed. Verdict is ",
    "based on the confirmed (non-~) comparisons only; where a category has no confirmed ",
    "comparisons but does have suggestive ones, the verdict is labeled accordingly. Categories ",
    "with very small n (e.g. Arid n=1, 1 paper; Mediterranean n=2, 1 paper) rest on a single ",
    "paper regardless of verdict and should be treated as anecdotal. Biome's 'Cryospheric Zone' ",
    "category pools the Alpine, Boreal, and Arctic biomes (Rocher-Ros, Horgby, Taillardat, and ",
    "Lupon et al. -- 4 papers), each individually a 1-2-paper group before pooling; its ",
    "comparisons are no longer a single-paper effect the way each sub-biome's were. Shaded cells: green = ",
    "Verdict is BH-confirmed (Enhancing/Inhibiting/Mixed, confirmed). Suggestive-only and No ",
    "signal rows are left unshaded -- absence of shading does not mean absence of a row, see the ",
    "Higher/Lower than columns for the underlying suggestive comparisons."
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

conf_rows_6 <- which(tbl_6_data$Verdict %in% c("Enhancing", "Inhibiting", "Mixed (confirmed both directions)"))
if (length(conf_rows_6) > 0) ft_6 <- ft_6 %>% bg(i = conf_rows_6, j = 5:7, bg = SIG_COLOR, part = "body")


# =============================================================================
# TABLE 7 — Category Counts: Sites and Papers per Biome / Source Water Category
# =============================================================================
cat("\n=== TABLE 7: Category Counts (Biome, Source Water) ===\n")

category_counts <- function(group_vec, doi_vec, group_name) {
  data.frame(category = as.character(group_vec), DOI = doi_vec, stringsAsFactors = FALSE) %>%
    filter(!is.na(category)) %>%
    group_by(category) %>%
    summarise(n = n(), Citations = n_distinct(DOI), .groups = "drop") %>%
    mutate(`Grouping factor` = group_name)
}

tbl_7_data <- bind_rows(
  category_counts(df_final$Biome_Category,     df_final$DOI, "Biome"),
  category_counts(df_final$Source_Water_Brief, df_final$DOI, "Source water")
) %>%
  transmute(`Grouping factor`, Category = category, n, Citations) %>%
  arrange(factor(`Grouping factor`, levels = c("Biome", "Source water")), desc(n))

print(tbl_7_data, n = Inf)

ft_7 <- flextable(tbl_7_data) %>%
  set_header_labels(`Grouping factor` = "Grouping factor", Category = "Category",
                     n = "n", Citations = "Citations") %>%
  merge_v(j = "Grouping factor") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  align(j = 1:2, align = "left",   part = "all") %>%
  align(j = 3:4, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  valign(j = 1, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1, width = 1.2) %>%
  width(j = 2, width = 2.0) %>%
  width(j = 3, width = 0.8) %>%
  width(j = 4, width = 0.9) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table 7. Number of site rows (n) and number of distinct papers/sources (Citations) ",
    "contributing to each Biome and Source water category used in Tables 1-2/6, after ",
    "same-river-reach collapsing (the same df_final rows tested in Tables 1-2). Rows within ",
    "each grouping factor are ordered by n, highest to lowest."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. n = number of rows (sites/reaches/time-periods, after same-river-reach collapsing, ",
    "as in Tables 1-2) assigned to that category. Citations = number of distinct papers/DOIs ",
    "contributing at least one row to that category -- a category with n > Citations has ",
    "multiple rows from the same paper (e.g. separate reaches or seasons), so its apparent ",
    "sample size is not n independent studies. Shaded cells: Citations = 1, i.e. the category ",
    "rests on a single paper regardless of n and should be treated as anecdotal (same caveat ",
    "as noted for specific categories in the Table 6 footnote)."
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

low_n_papers_7 <- which(tbl_7_data$Citations == 1)
if (length(low_n_papers_7) > 0) {
  ft_7 <- ft_7 %>% bg(i = low_n_papers_7, j = 4, bg = NEAR_SIG_COLOR, part = "body")
}


# =============================================================================
# TABLE 8 — Negative Pathway Estimates: Audit of Internal < 0 and External < 0
# =============================================================================
cat("\n=== TABLE 8: Negative Pathway Estimates ===\n")

# Built from `meta` (NA-filtered only), NOT `df`/`df_final`, so this audit also
# catches the one row `df` already silently drops via the Regulated-flow filter
# (Aho et al. 2021, Connecticut River) -- the point of this table is to show
# every negative estimate regardless of whether it entered Tables 1-2.
neg_reasons <- c(
  "Aho et al., 2021|Connecticut River (Thompsonville gauge)" =
    "Net autotrophic (GPP 20.9 > |ER| 17.8 gO2/m2/d); wide, reservoir-influenced mainstem, light not limiting.",
  "Kirk & Cohen, 2023|ICHE" =
    "Net autotrophic (GPP 7.8 ~ |ER| 7.4 gO2/m2/d); spring-fed clear-water karst river, dense submerged vegetation.",
  "Aho et al., 2021|Nepaug River" =
    "Internal = 165% of CO2_flux; RQ=1.0 assumption may overestimate Internal, and/or downstream export of dissolved CO2.",
  "Aho et al., 2021|Phelps Brook" =
    "Internal = 101% of CO2_flux; at the boundary, likely assumption/measurement noise rather than a real surplus.",
  "Carter et al., 2022|CBP" =
    "Internal = 106% of CO2_flux; at the boundary, likely assumption/measurement noise.",
  "Carter et al., 2022|PM" =
    "Internal = 129% of CO2_flux; groundwater-exchanging Piedmont stream, plausible downstream export.",
  "Carter et al., 2022|UNHC" =
    "Internal = 122% of CO2_flux; same system as PM, plausible downstream export.",
  "Rocher-Ros et al., 2019|M1" =
    "Internal = 185% of CO2_flux; Arctic tundra headwater, low gas-transfer velocity plausibly favors downstream export over local evasion.",
  "Rocher-Ros et al., 2019|M10" =
    "Internal = 176% of CO2_flux; same Arctic system as M1.",
  "Rocher-Ros et al., 2019|M6" =
    "Internal = 125% of CO2_flux; same Arctic system as M1.",
  "Solano et al., 2023|Manton Creek" =
    "Internal = 123% of CO2_flux; paper's OWN reported finding (Discussion) -- NEP exceeds local evasion, surplus attributed to downstream export as dissolved CO2/DIC. Not an artifact of this pipeline's assumptions."
)

neg_key <- paste(meta$Citation, meta$Site_ID, sep = "|")
neg_rows <- meta %>%
  mutate(key = neg_key,
         Internal_Pct_of_CO2flux = 100 * Internal_Pathway_gCm2day / CO2_flux_gCm2day) %>%
  filter((!is.na(Internal_Pathway_gCm2day) & Internal_Pathway_gCm2day < 0) |
         (!is.na(External_Pathway_gCm2day) & External_Pathway_gCm2day < 0))

n_per_citation_8 <- meta %>%
  filter(!is.na(Internal_Pathway_gCm2day), !is.na(External_Pathway_gCm2day)) %>%
  count(Citation, name = "n_sites_this_citation")

# A row can be negative on Internal, External, or (in principle) both -- keep
# one output row per pathway that is negative, so a hypothetical double-negative
# site would appear twice, once per pathway.
tbl_8_data <- neg_rows %>%
  left_join(n_per_citation_8, by = "Citation") %>%
  mutate(
    in_df_final = Source_Water_Brief != "Regulated flow",
    Exclusion_reason = ifelse(in_df_final, NA_character_,
                               "Excluded from Tables 1-2 by the Source_Water_Brief != 'Regulated flow' filter"),
    Reason = unname(neg_reasons[key])
  ) %>%
  { bind_rows(
      filter(., Internal_Pathway_gCm2day < 0) %>%
        transmute(Pathway = "Internal", Citation, Site_ID, key,
                  Estimate = Internal_Pathway_gCm2day, CO2_flux_gCm2day,
                  Internal_Pct_of_CO2flux, n_sites_this_citation, in_df_final,
                  Exclusion_reason, Reason),
      filter(., !is.na(External_Pathway_gCm2day) & External_Pathway_gCm2day < 0) %>%
        transmute(Pathway = "External", Citation, Site_ID, key,
                  Estimate = External_Pathway_gCm2day, CO2_flux_gCm2day,
                  Internal_Pct_of_CO2flux, n_sites_this_citation, in_df_final,
                  Exclusion_reason, Reason)
    ) } %>%
  mutate(
    `Sites remaining if removed` = n_sites_this_citation - 1L,
    `In Tables 1-2?` = ifelse(in_df_final, "Yes", "No"),
    Estimate = round(Estimate, 3),
    `Internal % of CO2 flux` = round(Internal_Pct_of_CO2flux, 1)
  ) %>%
  arrange(factor(Pathway, levels = c("Internal", "External")), Citation, Site_ID) %>%
  select(Pathway, Citation, `Site` = Site_ID, Estimate, `CO2 flux` = CO2_flux_gCm2day,
         `Internal % of CO2 flux`, `Sites for this citation` = n_sites_this_citation,
         `Sites remaining if removed`, `In Tables 1-2?`, Reason)

print(tbl_8_data, n = Inf)

ft_8 <- flextable(tbl_8_data) %>%
  set_header_labels(Pathway = "Pathway", Citation = "Citation", Site = "Site",
                     Estimate = "Estimate", `CO2 flux` = "CO2 flux",
                     `Internal % of CO2 flux` = "Internal % of CO2 flux",
                     `Sites for this citation` = "Sites (citation)",
                     `Sites remaining if removed` = "Remaining if removed",
                     `In Tables 1-2?` = "In Tables 1-2?", Reason = "Why negative") %>%
  merge_v(j = "Pathway") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  align(j = 1:3, align = "left",   part = "all") %>%
  align(j = 4:9, align = "center", part = "all") %>%
  align(j = 10,  align = "left",   part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  valign(j = 1, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",    border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1,  width = 0.8) %>%
  width(j = 2,  width = 1.5) %>%
  width(j = 3,  width = 1.3) %>%
  width(j = 4,  width = 0.7) %>%
  width(j = 5,  width = 0.7) %>%
  width(j = 6,  width = 0.9) %>%
  width(j = 7,  width = 0.8) %>%
  width(j = 8,  width = 0.9) %>%
  width(j = 9,  width = 0.8) %>%
  width(j = 10, width = 3.2) %>%
  height_all(height = 0.3) %>%
  add_header_lines(paste0(
    "Table 8. Every site with a negative Internal or negative External CO2 pathway estimate ",
    "(n = ", nrow(tbl_8_data), " estimates across ", n_distinct(tbl_8_data$Citation),
    " papers), with the estimate itself, how many other sites remain for that citation if the ",
    "row is dropped, whether the row survives into the Tables 1-2 tests, and why the estimate ",
    "is negative."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Flux in g C m-2 day-1. Internal % of CO2 flux = 100 x Internal / CO2_flux (uncapped, ",
    "unlike Internal.Contrib used elsewhere, which is capped to [0,100] against Internal+External); ",
    "shown here for context on how far the estimate departs from a closed mass balance. Negative ",
    "Internal = net autotrophic (GPP exceeds respiratory demand); negative External = Internal ",
    "alone exceeds measured CO2_flux, so the External_Pathway = CO2_flux - Internal residual goes ",
    "below zero -- this is the arithmetic signature of unmeasured downstream export of dissolved ",
    "CO2/DIC (or, for the smallest cases, RQ-assumption/measurement noise), not a modeled sink. ",
    "'Sites (citation)' / 'Remaining if removed' count only that citation's rows with valid ",
    "Internal AND External values (i.e. rows eligible for Tables 1-2 before any filtering). ",
    "'In Tables 1-2?' = No only for the one row (Aho et al. 2021, Connecticut River) excluded by ",
    "the Source_Water_Brief != 'Regulated flow' filter upstream of df_final; every other row here ",
    "is retained (the three Rocher-Ros sites are recoded from 'Glacial/snow melt' to 'Surface ",
    "runoff' there, per that same recoding step, but are not otherwise excluded or collapsed)."
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

excluded_rows_8 <- which(tbl_8_data$`In Tables 1-2?` == "No")
if (length(excluded_rows_8) > 0) {
  ft_8 <- ft_8 %>% bg(i = excluded_rows_8, j = 1:10, bg = UNDERPOWERED_COLOR, part = "body")
}


# ── Save publication-ready tables ───────────────────────────────────────────────############

save_as_docx(ft_1, ft_2, ft_3, ft_4, ft_5, ft_6, ft_7, ft_8, path = "05_Figures/Table_metaanalysis_spatiotempo.docx")

