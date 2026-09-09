
source("03_Scripts/Streams/analysis/01_metaanalysis_mlm_dataprep.R")
library(car)  # car::vif()

predictor_labels <- c(
  z_Temperature_C = "Temperature_C",
  z_abs_latitude = "abs_latitude",
  z_pH = "pH",
  z_Mean_Annual_Precipitation_cm_yr = "Mean_Annual_Precipitation_cm_yr",
  z_Discharge_m3s = "Discharge_m3s"
)
predictors_z <- names(predictor_labels)

# =============================================================================
# 1. Correlation matrix / VIF across the five z-scored predictors
# =============================================================================

cor_mat <- cor(meta[, predictors_z], use = "pairwise.complete.obs")
print(round(cor_mat, 3))

# Same matrix, plot-labeled version (predictor_labels instead of raw z_ names)
# so the figure below reads on its own without a column-name key.
cor_mat_labeled <- cor_mat
dimnames(cor_mat_labeled) <- list(predictor_labels[rownames(cor_mat)], predictor_labels[colnames(cor_mat)])
corrplot::corrplot(cor_mat_labeled, method = "color", type = "upper", diag = FALSE,
                    addCoef.col = "black", number.cex = 0.9,
                    tl.col = "black", tl.srt = 45,
                    title = "Correlation matrix: z-scored predictors", mar = c(0, 0, 1, 0))

cor_pairs <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, r = Freq) %>%
  filter(as.character(var1) < as.character(var2)) %>%
  mutate(r = round(r, 3)) %>%
  arrange(desc(abs(r)))
print(cor_pairs, row.names = FALSE)

high_cor <- cor_pairs %>% filter(abs(r) > 0.7)
if (nrow(high_cor) > 0) print(high_cor, row.names = FALSE)


vif_data <- meta %>% filter(if_all(all_of(c(predictors_z, "logit_pct_internal")), ~!is.na(.)))
vif_model <- lm(reformulate(predictors_z, response = "logit_pct_internal"), data = vif_data)
vif_vals <- car::vif(vif_model)

print(round(vif_vals, 3))
high_vif <- vif_vals[vif_vals > 5]
cat("Predictor(s) with VIF > 5: ", if (length(high_vif) == 0) "none." else paste(names(high_vif), collapse = ", "), "\n", sep = "")

# =============================================================================
# 2. Distribution check for all response variables
# =============================================================================

response_vars <- c("CO2_flux_gCm2day", "Internal_Pathway_gCm2day",
                    "External_Pathway_gCm2day", "Internal_Pct_of_Flux", "logit_pct_internal")

dist_diag <- map_dfr(response_vars, function(v) {
  x <- meta[[v]]
  x <- x[!is.na(x)]
  sh_p <- if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else NA_real_
  tibble(variable = v, n = length(x), mean = mean(x), median = median(x), sd = sd(x),
         min = min(x), max = max(x), skew_approx = (mean(x) - median(x)) / sd(x), shapiro_p = sh_p)
}) %>%
  mutate(across(c(mean, median, sd, min, max, skew_approx, shapiro_p), ~round(., 3)),
         verdict = case_when(
           !is.na(shapiro_p) & shapiro_p < 0.05 & abs(skew_approx) > 0.2 ~ "Non-normal & skewed",
           !is.na(shapiro_p) & shapiro_p < 0.05 ~ "Non-normal (Shapiro p<0.05) but not strongly skewed -- OK given n",
           TRUE ~ "Reasonably normal -- usable as-is"
         ))
print(dist_diag, n = Inf, width = Inf)

# Skew approximated as (mean-median)/sd (a quick, dependency-free heuristic,
# not a formal skewness statistic); |skew_approx| > 0.2 is an informal cutoff.
op <- par(mfrow = c(length(response_vars), 2), mar = c(4, 4, 2, 1))
for (v in response_vars) {
  x <- meta[[v]]
  hist(x, main = paste("Histogram:", v), xlab = v, col = "grey80", border = "white", breaks = 15)
  qqnorm(x, main = paste("Q-Q:", v)); qqline(x, col = "red")
}
par(op)

# =============================================================================
# 3. Distribution check for the transformed response variables (flux magnitudes)
# =============================================================================

transform_spec <- tibble(
  variable  = c("CO2_flux_gCm2day", "Internal_Pathway_gCm2day", "External_Pathway_gCm2day"),
  transform = c("log10", "asinh", "asinh")
)


trans_dist_diag <- pmap_dfr(transform_spec, function(variable, transform) {
  x <- meta[[variable]]
  x <- x[!is.na(x)]
  if (transform == "log10") x <- x[x > 0]
  tx <- if (transform == "log10") log10(x) else asinh(x)
  sh_p <- if (length(tx) >= 3 && length(tx) <= 5000) shapiro.test(tx)$p.value else NA_real_
  tibble(variable = paste0(transform, "(", variable, ")"), n = length(tx), mean = mean(tx), median = median(tx),
         sd = sd(tx), min = min(tx), max = max(tx),
         skew_approx = (mean(tx) - median(tx)) / sd(tx), shapiro_p = sh_p)
}) %>%
  mutate(across(c(mean, median, sd, min, max, skew_approx, shapiro_p), ~round(., 3)),
         verdict = case_when(
           !is.na(shapiro_p) & shapiro_p < 0.05 & abs(skew_approx) > 0.2 ~ "Still non-normal & skewed after transform",
           !is.na(shapiro_p) & shapiro_p < 0.05 ~ "Non-normal (Shapiro p<0.05) but not strongly skewed -- OK given n",
           TRUE ~ "Reasonably normal after transform"
         ))
cat("\n")
print(trans_dist_diag, n = Inf, width = Inf)

op <- par(mfrow = c(nrow(transform_spec), 2), mar = c(4, 4, 2, 1))
for (i in seq_len(nrow(transform_spec))) {
  variable  <- transform_spec$variable[i]
  transform <- transform_spec$transform[i]
  x <- meta[[variable]][!is.na(meta[[variable]])]
  if (transform == "log10") x <- x[x > 0]
  tx <- if (transform == "log10") log10(x) else asinh(x)
  label <- paste0(transform, "(", variable, ")")
  hist(tx, main = paste("Histogram:", label), xlab = label, col = "grey80", border = "white", breaks = 15)
  qqnorm(tx, main = paste("Q-Q:", label)); qqline(tx, col = "red")
}
par(op)

# =============================================================================
# 4. Group sizes per random effect
# =============================================================================

cat("\nRows per Citation:\n")
citation_counts <- meta %>% count(Citation, name = "n_rows") %>% arrange(n_rows)
print(citation_counts, n = Inf)

singleton_citations <- citation_counts %>% filter(n_rows == 1)
cat("\n", nrow(singleton_citations), " of ", n_distinct(meta$Citation),
    " citation(s) contribute only 1 row -- cannot contribute to within-citation",
    " (Row_ID nested in Citation) variance:\n", sep = "")
print(singleton_citations, n = Inf)

cat("\nCitations per collapsed Biome (Biome_collapsed; see data-prep script for the recode):\n")
print(biome_citation_counts, n = Inf)
thin_biomes <- biome_citation_counts %>% filter(n_citations < 3)
cat("\nBiome level(s) resting on <3 citations (thin support for a biome-level variance component): ",
    if (nrow(thin_biomes) == 0) "none.\n" else "\n", sep = "")
if (nrow(thin_biomes) > 0) print(thin_biomes, n = Inf)

# =============================================================================
# 5. Weight distribution (n_reaches)
# =============================================================================


n_reaches_table <- meta %>% count(n_reaches, name = "n_rows") %>% arrange(desc(n_reaches))
print(n_reaches_table, n = Inf)

total_weight <- sum(meta$n_reaches, na.rm = TRUE)
weight_share <- meta %>%
  mutate(pct_of_total_weight = round(100 * n_reaches / total_weight, 1)) %>%
  arrange(desc(n_reaches)) %>%
  select(Row_ID, Citation, Site_ID, n_reaches, pct_of_total_weight)

cat("\nTop 5 heaviest rows by share of total n_reaches weight:\n")
print(head(weight_share, 5), n = Inf, width = Inf)

dominant_rows <- weight_share %>% filter(pct_of_total_weight > 10)
cat("\nRow(s) holding >10% of total weight (heuristic flag threshold): ",
    if (nrow(dominant_rows) == 0) "none.\n" else "\n", sep = "")
if (nrow(dominant_rows) > 0) print(dominant_rows, n = Inf, width = Inf)

