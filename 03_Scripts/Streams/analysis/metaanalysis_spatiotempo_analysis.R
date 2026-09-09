
library(tidyverse)
library(readxl)
library(coin)         # permutation-based Spearman tests (project convention)
library(FSA)          # dunnTest — post-hoc for Kruskal-Wallis
library(pwr)           # post-hoc power for the Spearman/KW tests
library(flextable)
library(officer)

# ── Data preparation ──────────────────────────────────────────────────────────###########

meta_path  <- "01_Raw_data/meta_analysis_v3.xlsx"
data_raw   <- read_excel(meta_path, sheet = "Data")
paper_info <- read_excel(meta_path, sheet = "Paper_Info")

# Data's Row_ID is 1:1 with a Paper_Info row, which carries DOI plus the
# per-row extraction documentation (Partition_Origin, Fraction_Basis,
# Source, Needs_Hand_Check) that Data itself doesn't repeat.
meta <- data_raw %>%
  left_join(
    paper_info %>% select(Row_ID, DOI, Partition_Origin, Fraction_Basis,
                           Needs_Hand_Check),
    by = "Row_ID"
  ) %>%
  rename(Source_Water_Brief = Source_Water) %>%
  mutate(
    Discharge_m3s = as.numeric(Discharge_m3s),
    Temperature_C = as.numeric(Temperature_C),
    CO2_flux_gCm2day = as.numeric(CO2_flux_gCm2day),
    Internal_Pathway_gCm2day = as.numeric(Internal_Pathway_gCm2day),
    External_Pathway_gCm2day = as.numeric(External_Pathway_gCm2day),
    # Pooled biome -- same intent as the old Alpine/Boreal/Arctic -> "Cryospheric
    # Zone" and Mediterranean/Arid -> "Drylands" recode, adapted to this
    # workbook's WWF-style Biome labels, to avoid 1-2-paper singleton groups
    # in the Kruskal-Wallis tests below.
    Biome_Category = case_when(
      str_detect(Biome, "Boreal|Tundra")                             ~ "Cryospheric Zone",
      str_detect(Biome, "Desert|Mediterranean")                      ~ "Drylands",
      str_detect(Biome, "Temperate broadleaf|Temperate coniferous")  ~ "Temperate forest",
      str_detect(Biome, "Temperate grassland")                       ~ "Temperate grassland & shrubland",
      str_detect(Biome, "Tropical")                                  ~ "Tropical",
      str_detect(Biome, "Anthropogenic")                             ~ "Anthropogenic (canal)",
      TRUE                                                           ~ Biome
    )
  )

# This study's own site data (int.ext.summary) is sourced for downstream use
# elsewhere in the pipeline; not merged into df/df_final here (literature-only).
source("03_Scripts/Streams/analysis/data for analysis.R")
df <- meta %>%
  filter(!is.na(Internal_Pathway_gCm2day), !is.na(External_Pathway_gCm2day))%>%
  mutate(
    Internal.Contrib=(Internal_Pathway_gCm2day / (Internal_Pathway_gCm2day + External_Pathway_gCm2day))*100,
    Internal.Contrib=ifelse(Internal.Contrib>100, 100, Internal.Contrib),
    Internal.Contrib=ifelse(Internal.Contrib<0, 0, Internal.Contrib),
    # Kept for forward compatibility with future extractions -- neither
    # category is present in meta_analysis_v3.xlsx, so both are no-ops today.
    Source_Water_Brief=ifelse(Source_Water_Brief=="Glacial/snow melt", "Surface runoff", Source_Water_Brief)
  )%>%
  filter(Source_Water_Brief != 'Regulated flow')
unique(df$Source_Water_Brief)

# Same-river-reach / same-time-period collapsing now happens upstream during
# extraction (see each row's n_reaches, and Paper_Info's Is_Aggregate /
# N_rows_collapsed for the pre-collapse rows folded into it) -- df is
# already at the one-row-per-analysis-unit granularity Tables 1-2 need, so
# no manual collapse step is required here.
df_final <- df %>%
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
    "Biome_Category (", nlevels(df_final$Biome_Category), " levels; pooled from this workbook's ",
    "finer Biome labels -- see the Biome_Category recode in this script's Data preparation ",
    "section for the grouping rule). Small groups limit power for these factors -- treat as ",
    "exploratory. ", kw_note
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

# Per-paper estimation-method summary built mechanically from this workbook's
# own extraction documentation (Paper_Info: Partition_Origin, Fraction_Basis)
# rather than a hand-written per-paper narrative -- the prior version's
# narrative was tied to specific papers/DOIs from the old extraction, several
# of which (e.g. Horgby et al. 2019) are no longer in this dataset, and
# several current papers weren't in the old one at all. See Paper_Info's
# Source / Unit_Conversions_Notes columns in meta_analysis_v3.xlsx for the
# full per-row derivation notes this is condensed from.
partition_origin_labels <- c(
  AUTHOR   = "Internal/External taken directly from a split the paper's authors report.",
  DERIVED  = "Internal/External derived from the paper's reported metabolism (GPP/ER) or other measured terms.",
  FRACTION = "Internal/External derived by applying a reported fraction/percentage to CO2_flux.",
  NONE     = "No paper-reported Internal/External partition; not computed for this row."
)

pathway_method <- meta %>%
  filter(!is.na(Partition_Origin)) %>%
  mutate(
    method_txt = paste0(
      unname(partition_origin_labels[Partition_Origin]),
      ifelse(!is.na(Fraction_Basis) & Fraction_Basis != "", paste0(" Basis: ", Fraction_Basis, "."), "")
    )
  ) %>%
  group_by(DOI) %>%
  summarise(Pathway_Method = paste(unique(method_txt), collapse = " | "), .groups = "drop")

papers_5 <- df %>%
  group_by(DOI) %>%
  summarise(Citation = first(Citation),
            Reaches_extracted = sum(n_reaches, na.rm = TRUE),
            Rows_in_analysis  = n(),
            .groups = "drop") %>%
  left_join(categories_5, by = "DOI") %>%
  left_join(pathway_method, by = "DOI") %>%
  mutate(
    Collapsed = ifelse(Reaches_extracted > Rows_in_analysis,
                        paste0("Yes — ", Reaches_extracted, " reaches/time-periods collapsed to ",
                               Rows_in_analysis, " row(s) upstream, during extraction"),
                        "No — one row per reach/time-period"),
    Pathway_Method = ifelse(is.na(Pathway_Method),
                             "Not documented in Paper_Info for this paper.", Pathway_Method)
  ) %>%
  arrange(desc(Rows_in_analysis), desc(Reaches_extracted))

print(papers_5, n = Inf)

tbl_5_data <- papers_5 %>%
  transmute(Citation, DOI, Biome, `Source water` = Source_Water,
            `Reaches/time-periods extracted` = Reaches_extracted,
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
    "Internal/External pathway values were derived. n = ", n_papers,
    " papers; ", nrow(df_final), " rows enter the Internal/External tests."
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. Biome/Source water = the category (or categories) actually assigned to that ",
    "paper's row(s) in Tables 1-2, not the raw per-reach extraction. A paper reporting reaches ",
    "that split across two different source-water categories has both categories listed, and ",
    "both rows remain in the Kruskal-Wallis tests, which is part of the reach-level ",
    "heterogeneity this analysis is designed to keep visible rather than average away. ",
    "Reaches/time-periods extracted = number of original reach/time-period observations behind ",
    "that paper's row(s) (n_reaches, summed); this can exceed Rows in Tables 1-2 where the ",
    "extraction itself already averaged multiple reaches/time-periods into one row upstream ",
    "(see Collapsed to paper average?, and Paper_Info's Is_Aggregate / N_rows_collapsed in ",
    "meta_analysis_v3.xlsx for the underlying pre-collapse rows). Rows in Tables 1-2 = number ",
    "of rows that paper contributes to the Internal/External Spearman and Kruskal-Wallis tests. ",
    "Consequently, Tables 1-2's n is not n independent papers -- several papers each contribute ",
    "multiple, non-independent rows, and results should be interpreted with that in mind. ",
    "Internal/External estimation method = condensed from this workbook's own Partition_Origin/",
    "Fraction_Basis extraction fields (Paper_Info sheet): AUTHOR = the paper reports the split ",
    "directly; DERIVED = computed from reported metabolism (GPP/ER) or other measured terms; ",
    "FRACTION = a reported percentage/fraction applied to CO2_flux; NONE = no partition ",
    "computed for that row. See Paper_Info's Source / Unit_Conversions_Notes columns for the ",
    "full per-row derivation notes and exact source citations (table/page numbers)."
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
    "with very small n rest on a single paper regardless of verdict and should be treated as ",
    "anecdotal (see Table 7 for the paper count behind each category). Biome's pooled categories ",
    "(e.g. 'Cryospheric Zone' = Boreal forest + Tundra) combine several finer Biome labels that ",
    "are each individually a 1-2-paper group before pooling -- see the Biome_Category recode in ",
    "this script's Data preparation section. Shaded cells: green = ",
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
# catches any row `df` silently drops via the Regulated-flow filter -- the
# point of this table is to show every negative estimate regardless of
# whether it entered Tables 1-2.
#
# Reasons below are generated mechanically from each row's own GPP/ER (for
# negative Internal) or from the residual arithmetic (for negative External),
# rather than a hand-written per-site narrative -- the prior version's
# narrative (e.g. "paper's OWN reported finding (Discussion)...") was written
# against the previous extraction's specific values and sites, several of
# which have since changed or dropped out with this re-extraction, so it can
# no longer be trusted to still apply. Needs_Hand_Check flags rows the
# extraction itself already marked for manual review.
mechanical_neg_reason <- function(internal, external, gpp, er, needs_check) {
  reason <- if (!is.na(internal) && internal < 0) {
    if (!is.na(gpp) && !is.na(er)) {
      sprintf("Net autotrophic (GPP %.2f > |ER| %.2f gO2/m2/d) -> negative respiration-based Internal estimate.", gpp, er)
    } else {
      "Internal estimate is negative; GPP/ER not available in this extraction to confirm net autotrophy."
    }
  } else if (!is.na(external) && external < 0) {
    paste0("Internal estimate exceeds measured CO2_flux, so the External = CO2_flux - Internal residual is ",
           "negative (points to unmeasured downstream export of dissolved CO2/DIC, or method/assumption uncertainty).")
  } else {
    NA_character_
  }
  if (!is.na(needs_check) && needs_check == "YES") {
    reason <- paste0(reason, " Flagged Needs_Hand_Check = YES in the extraction.")
  }
  reason
}

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
    Reason = mapply(mechanical_neg_reason, Internal_Pathway_gCm2day, External_Pathway_gCm2day,
                     GPP_gCm2day, ER_gCm2day, Needs_Hand_Check)
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
    "'Why negative' is generated mechanically from each row's own GPP/ER or residual arithmetic ",
    "(see the mechanical_neg_reason() comment above in this script), not a hand-checked, paper- ",
    "specific explanation -- rows also flagged Needs_Hand_Check = YES in the extraction warrant a ",
    "closer read of the source paper before drawing conclusions. 'In Tables 1-2?' = No for rows ",
    "excluded by the Source_Water_Brief != 'Regulated flow' filter upstream of df_final (",
    sum(tbl_8_data$`In Tables 1-2?` == "No"), " of ", nrow(tbl_8_data),
    " rows here); every other row is retained."
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

