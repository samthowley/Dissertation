
source("03_Scripts/Streams/analysis/data for analysis.R")


df <- int.ext %>%
  mutate(ID = factor(ID))

spatial_df <- spatial_df %>%
  mutate(
    ID                  = factor(ID),
    total.wetland.cover = total.wetland.cover * 100
  )

stopifnot("All site IDs in df must appear in spatial_df" =
  all(as.character(unique(df$ID)) %in% as.character(spatial_df$ID)))


# =============================================================================
# SECTION 1 — SITE-LEVEL MEAN FLUX RATES
# =============================================================================
# Response: mean internal flux and mean external flux per site.
# No transformation applied — raw flux units.
# External = CO2_flux - internal (derived residual; not independently measured).

flux_rates <- df %>%
  group_by(ID) %>%
  summarise(
    n            = n(),
    mean_int     = mean(internal, na.rm = TRUE),
    sd_int       = sd(internal,   na.rm = TRUE),
    median_int   = median(internal, na.rm = TRUE),
    mean_ext     = mean(external, na.rm = TRUE),
    sd_ext       = sd(external,   na.rm = TRUE),
    median_ext   = median(external, na.rm = TRUE),
    .groups = "drop"
  )

print(flux_rates, row.names = FALSE)

rates_df <- flux_rates %>%
  left_join(spatial_df, by = "ID")


# =============================================================================
# SECTION 2 — PERMUTATION SPEARMAN: MEAN FLUX RATE ~ SPATIAL PREDICTORS
# =============================================================================

run_perm_spearman <- function(response_vec, predictor_vec, ID_labels,
                               resp_name, pred_name,
                               nresample = 99999) {
  d <- data.frame(
    resp = response_vec,
    pred = predictor_vec,
    ID   = ID_labels
  ) %>% filter(!is.na(resp) & !is.na(pred))

  if (nrow(d) < 4) {
    warning("Too few observations for permutation test: ", resp_name, " ~ ", pred_name)
    return(data.frame(response = resp_name, predictor = pred_name,
                      rho = NA, p_raw = NA, n = nrow(d)))
  }

  rho <- cor(d$resp, d$pred, method = "spearman")

  pt <- spearman_test(
    resp ~ pred,
    data         = d,
    distribution = approximate(nresample = nresample)
  )

  data.frame(
    response  = resp_name,
    predictor = pred_name,
    rho       = round(rho, 3),
    p_raw     = round(pvalue(pt)[[1]], 5),
    n         = nrow(d)
  )
}

NRESAMPLE <- 999999

responses_rates  <- c("mean_int", "mean_ext")
predictors_rates <- c("total.wetland.cover", "RB_index", "pH", "SpC")

perm_rates <- map2(
  rep(responses_rates,  each  = length(predictors_rates)),
  rep(predictors_rates, times = length(responses_rates)),
  ~ run_perm_spearman(
      rates_df[[.x]], rates_df[[.y]],
      rates_df$ID, .x, .y, NRESAMPLE
    )
) |> list_rbind()

perm_rates$p_BH <- round(p.adjust(perm_rates$p_raw, method = "BH"), 5)
perm_rates$sig  <- ifelse(perm_rates$p_BH < 0.05, "*", "")

print(perm_rates, row.names = FALSE)


# =============================================================================
# SECTION 3 — ADVISOR TABLES
# =============================================================================

library(knitr)

print_table <- function(title, note, data) {
  cat(paste0("\n", strrep("-", 70), "\n"))
  cat(paste0(title, "\n"))
  if (!is.null(note)) cat(paste0("Note: ", note, "\n"))
  cat(strrep("-", 70), "\n")
  print(kable(data, format = "simple", na = "—"))
  cat("\n")
}

# ------------------------------------------------------------------
# TABLE 1 — Site-level mean flux rates
# ------------------------------------------------------------------

tbl1 <- flux_rates %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(
    Site          = ID,
    `n (days)`    = n,
    `Mean Int.`   = mean_int,
    `SD Int.`     = sd_int,
    `Median Int.` = median_int,
    `Mean Ext.`   = mean_ext,
    `SD Ext.`     = sd_ext,
    `Median Ext.` = median_ext
  )

print_table(
  "TABLE 1 — Site-Level Mean Flux Rates: Internal and External Pathways",
  "Raw flux values (no transformation). External = CO2_flux - internal (derived residual).",
  tbl1
)

# ------------------------------------------------------------------
# TABLE 2 — Spearman results: mean flux rate ~ spatial predictors
# ------------------------------------------------------------------

tbl2 <- perm_rates %>%
  mutate(
    rho   = round(rho, 3),
    p_raw = round(p_raw, 4),
    p_BH  = round(p_BH, 4),
    sig   = ifelse(p_BH < 0.05, "*", ""),
    response = case_match(response,
                         "mean_int" ~ "Internal",
                         "mean_ext" ~ "External")
  ) %>%
  select(response, predictor, rho, p_raw, p_BH, sig, n) %>%
  rename(
    `Pathway`      = response,
    `Predictor`    = predictor,
    `Spearman rho` = rho,
    `p (raw)`      = p_raw,
    `p (BH-adj)`   = p_BH,
    `Sig.`         = sig,
    `n sites`      = n
  ) %>%
  arrange(Pathway, desc(abs(`Spearman rho`)))

print_table(
  "TABLE 2 — Spearman Results: Mean Flux Rate ~ Spatial Predictors",
  "BH correction across 8 tests (2 pathways x 4 predictors). Sig. (*) = p(BH) < 0.05.",
  tbl2
)
