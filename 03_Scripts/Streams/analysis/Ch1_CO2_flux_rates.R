
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
# RESEARCH QUESTION 3 — Do spatial factors influence the flux magnitude
# of either pathway?
# Analysis: site-level mean flux rates ~ spatial predictors
# =============================================================================

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

