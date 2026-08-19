# =============================================================================
# DISCHARGE x TEMPERATURE — facet_grid scatter, one panel per site, annotated
# with the additive-vs-interaction GLS-AR(1) AIC comparison per response
# =============================================================================
# One panel per site (ID): x = TempC, y = log10(Q). Each panel's subtitle
# lists, per response (Internal/External/Total CO2 Flux, Internal
# Contribution %), the AIC of the additive (No Interaction: resp ~ TempC +
# log10Q) GLS-AR(1) model, delta_AIC vs. the interaction model
# (resp ~ TempC * log10Q), and whether it "passed" (delta_AIC < 2, i.e. the
# interaction does not meaningfully improve the fit) -- matches the
# delta_AIC / AIC comparison logic in gls_temporal_analysis.R.
# =============================================================================

source("03_Scripts/Streams/analysis/data for analysis.R")
library(nlme)
library(ggplot2)

int.ext <- read_csv("04_Output/stream/external-internal.csv") %>%
  mutate(ID = as.character(ID))

site_order <- c("3", "5", "5a", "6", "7", "9", "13", "15")

responses <- c("internal", "external", "CO2_flux", "int.contrib")
response_labels <- c(
  internal    = "Internal",
  external    = "External",
  CO2_flux    = "Total",
  int.contrib = "Int. Contrib. %"
)

# ── Per-response, per-site data prep (Q, TempC, response) ───────────────────
# Response: log10() for the three flux categories. For int.contrib, this
# recomputes pct_internal on the fly EXACTLY as gls_temporal_analysis.R's own
# build_response() does (100 x internal/CO2_flux, clamped only at the upper
# bound) rather than using the pre-built int.contrib column from
# "chimney  pathway.R" (which clamps both bounds, <0 -> 0 as well) -- since
# these AIC numbers must reproduce that script's own GLS fits exactly.
# TempC is never log-transformed.
prep_data <- function(resp_name) {
  d <- int.ext %>%
    filter(Q > 0) %>%
    filter(is.finite(TempC)) %>%
    arrange(Date) %>%
    mutate(t = as.numeric(Date), log10Q = log10(Q))

  if (resp_name == "int.contrib") {
    d <- d %>% filter(is.finite(internal), is.finite(CO2_flux), CO2_flux != 0)
    d$y <- 100 * d$internal / d$CO2_flux
    d$y <- if_else(d$y > 100, 100, d$y)
  } else {
    d <- d %>% filter(.data[[resp_name]] > 0)
    d$y <- log10(d[[resp_name]])
  }

  d %>% filter(is.finite(y), is.finite(log10Q))
}

# ── Fit additive vs. interaction GLS-AR(1), one site x response at a time ───
fit_aic <- function(site_id, resp_name, min_n = 15) {
  d <- prep_data(resp_name) %>% filter(ID == site_id)
  if (nrow(d) < min_n) return(NULL)

  fit_gls <- function(f) tryCatch(
    gls(f, data = d, correlation = corCAR1(form = ~ t), method = "ML"),
    error = function(e) NULL
  )
  full_mod <- fit_gls(y ~ TempC + log10Q)
  int_mod  <- fit_gls(y ~ TempC * log10Q)
  if (is.null(full_mod) || is.null(int_mod)) return(NULL)

  aic_full  <- AIC(full_mod)
  aic_int   <- AIC(int_mod)
  delta_AIC <- round(aic_full - aic_int, 2)   # positive => interaction fits better

  tibble(
    site      = site_id,
    response  = resp_name,
    n         = nrow(d),
    AIC_full  = round(aic_full, 1),
    delta_AIC = delta_AIC,
    passed    = delta_AIC < 2
  )
}

sites <- sort(unique(int.ext$ID))

aic_summary <- expand_grid(site = sites, response = responses) %>%
  pmap_dfr(~ fit_aic(..1, ..2)) %>%
  mutate(
    site     = factor(site, levels = site_order),
    response = factor(response, levels = responses)
  )

# ── Scatter data: TempC vs. log10(Q), one point cloud per site ──────────────
# (Not split by response/pathway -- Q and TempC are the same two columns
# regardless of which response the AIC test above was computed for.)
scatter_data <- int.ext %>%
  filter(Q > 0, is.finite(TempC)) %>%
  mutate(site = factor(ID, levels = site_order), log10Q = log10(Q))

# One line of text per response, stacked top-down within each site panel,
# colored green if it passed (delta_AIC < 2) and red if it failed. Positioned
# in data units (not Inf) so lines stack at a fixed vertical spacing -- the
# y-axis (log10 Q) is shared across all site panels (no free scales).
y_range <- range(scatter_data$log10Q, na.rm = TRUE)
y_step  <- 0.075 * diff(y_range)
y_top   <- y_range[2] - 0.02 * diff(y_range)

aic_labels <- aic_summary %>%
  arrange(site, response) %>%
  group_by(site) %>%
  mutate(y_pos = y_top - (row_number() - 1) * y_step) %>%
  ungroup() %>%
  mutate(
    label = paste0(response_labels[as.character(response)], ": AIC=", AIC_full,
                    ", ΔAIC=", delta_AIC, " (", ifelse(passed, "Passed", "Failed"), ")")
  )

# ── Plot ─────────────────────────────────────────────────────────────────
p_discharge_temp_facetgrid <- ggplot(scatter_data, aes(x = TempC, y = log10Q)) +
  geom_point(size = 0.6, alpha = 0.35, color = "steelblue") +
  geom_text(
    data        = aic_labels,
    aes(x = -Inf, y = y_pos, label = label, color = passed),
    inherit.aes = FALSE,
    hjust = -0.02, size = 1.9, fontface = "italic"
  ) +
  scale_color_manual(values = c(`TRUE` = "#1B7837", `FALSE` = "#B2182B"), guide = "none") +
  facet_grid(cols = vars(site)) +
  labs(
    x        = expression("Temperature ("*degree*"C)"),
    y        = expression(log[10]~"(Discharge, L "*s^-1*")"),
    title    = "Temperature vs. Discharge, by Site",
    subtitle = "Per-panel text (one line per response): AIC of the additive GLS-AR(1) model and ΔAIC vs. the interaction model. Green = Passed (ΔAIC < 2, no meaningful improvement from adding the interaction); Red = Failed (ΔAIC ≥ 2)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text    = element_text(face = "bold", size = 9),
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 7)
  )

p_discharge_temp_facetgrid

ggsave("05_Figures/Figure_discharge_temp_facetgrid.png",
       p_discharge_temp_facetgrid, width = 20, height = 5.5, dpi = 300)
