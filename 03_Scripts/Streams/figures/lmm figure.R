source("03_Scripts/Streams/analysis/data for analysis.R")

library(brms)

# ============================================================
# Full-model slope figure: Estimate per variable per site
# ============================================================

slope_df <- full_raw %>%
  select(site, pathway, indep.var, Estimate, R2) %>%
  mutate(
    site      = factor(as.character(site), levels = site_order),
    indep.var = factor(indep.var, levels = c("lQ", "TempC"),
                       labels = c("Discharge (Q)", "Temperature (T)")),
    bar_label = paste0(round(Estimate, 2), "\n(", round(R2, 2), ")")
  )

var_colors <- c("Discharge (Q)" = "#2166ac", "Temperature (T)" = "#d6604d")

(slope_fig <- slope_df %>%
  ggplot(aes(x = site, y = Estimate, fill = indep.var)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(
    aes(
      label = bar_label,
      y     = ifelse(Estimate >= 0, Estimate + 0.007, Estimate - 0.007),
      vjust = ifelse(Estimate >= 0, 0, 1)
    ),
    position   = position_dodge(width = 0.75),
    size       = 3,
    lineheight = 0.85
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_fill_manual(values = var_colors, name = "Predictor") +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  facet_wrap(~pathway, ncol = 1,
             labeller = labeller(pathway = pathway_labs)) +
  labs(y = "Slope (Estimate)",
       title = "Full model: slopes and R² by site",
       caption = "Values above bars: slope (R²)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    strip.text         = element_text(size=12),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank()
  )
)


# ============================================================
# R² Summary Figure: Q-graph and T-graph
# ============================================================

full_raw <- read_csv("04_Output/stream/models/site_specific_results.csv")

full_df <- full_raw %>%
  select(site, pathway, indep.var, Estimate, R2) %>%
  pivot_wider(
    id_cols    = c(site, pathway, R2),
    names_from  = indep.var,
    values_from = Estimate
  ) %>%
  rename(est_lQ = lQ, est_TempC = TempC) %>%
  mutate(model_type = "full", site = as.character(site))

# --- 2. Interaction models (RDS files) -----------------------
int_sites <- c("13", "15", "3", "5", "5a", "6", "7", "9")

int_df <- map_dfr(int_sites, function(s) {
  m  <- readRDS(paste0("04_Output/stream/models/Site Specific Interaction/", s, ".rds"))
  r2 <- bayes_R2(m)
  fe <- as.data.frame(fixef(m)) %>% rownames_to_column("param")

  get_est <- function(p) {
    v <- fe$Estimate[fe$param == p]
    if (length(v) == 0) NA_real_ else v[1]
  }

  tibble(
    site       = s,
    model_type = "interaction",
    pathway    = c("lint", "lext"),
    R2         = c(r2["R2lint", "Estimate"], r2["R2lext", "Estimate"]),
    est_lQ     = c(get_est("lint_lQ"),    get_est("lext_lQ")),
    est_TempC  = c(get_est("lint_TempC"), get_est("lext_TempC"))
  )
})

# --- 3. Drop-Q conditions ------------------------------------
dropQ_proc <- read_csv("04_Output/stream/models/dropQ.csv") %>%
  filter(!is.na(dropped_from), dropped_from %in% c("lint", "lext"),
         pathway == dropped_from) %>%
  group_by(site, dropped_from, pathway) %>%
  summarise(
    R2        = first(R2),
    est_lQ    = Estimate[indep == "lQ"][1],
    est_TempC = Estimate[indep == "TempC"][1],
    .groups   = "drop"
  ) %>%
  mutate(model_type = paste0("Q_drop_", dropped_from), site = as.character(site)) %>%
  select(-dropped_from)

# --- 4. Drop-T conditions ------------------------------------
dropT_proc <- read_csv("04_Output/stream/models/dropT.csv") %>%
  filter(!is.na(dropped_from), dropped_from %in% c("lint", "lext"),
         pathway == dropped_from) %>%
  group_by(site, dropped_from, pathway) %>%
  summarise(
    R2        = first(R2),
    est_lQ    = Estimate[indep == "lQ"][1],
    est_TempC = Estimate[indep == "TempC"][1],
    .groups   = "drop"
  ) %>%
  mutate(model_type = paste0("T_drop_", dropped_from), site = as.character(site)) %>%
  select(-dropped_from)

# --- 5. Combine and build slope labels -----------------------
site_order <- c("3", "5", "5a", "6", "7", "9", "13", "15")

all_df <- bind_rows(full_df, int_df, dropQ_proc, dropT_proc) %>%
  mutate(
    site = factor(site, levels = site_order),
    slope_label = case_when(
      !is.na(est_lQ) & !is.na(est_TempC) ~
        paste0("Q:", round(est_lQ, 2), "\nT:", round(est_TempC, 2)),
      !is.na(est_lQ)   ~ paste0("Q:", round(est_lQ, 2)),
      !is.na(est_TempC) ~ paste0("T:", round(est_TempC, 2)),
      TRUE ~ ""
    )
  )

# --- 6. Shared plot helpers ----------------------------------
model_colors <- c(
  "full"        = "black",
  "interaction" = "gray50",
  "drop_lint"   = "red",
  "drop_lext"   = "blue"
)
model_labels <- c(
  "full"        = "Full model",
  "interaction" = "Interaction model",
  "drop_lint"   = "Dropped from internal",
  "drop_lext"   = "Dropped from external"
)

pathway_labs <- c(lint = "Internal", lext = "External")

make_r2_fig <- function(data, drop_prefix, iv_label) {
  data %>%
    filter(model_type %in% c("full", "interaction",
                              paste0(drop_prefix, "_drop_lint"),
                              paste0(drop_prefix, "_drop_lext"))) %>%
    mutate(
      fill_key = case_when(
        model_type == "full"                              ~ "full",
        model_type == "interaction"                       ~ "interaction",
        model_type == paste0(drop_prefix, "_drop_lint")   ~ "drop_lint",
        model_type == paste0(drop_prefix, "_drop_lext")   ~ "drop_lext"
      ),
      fill_key = factor(fill_key, levels = names(model_colors))
    ) %>%
    ggplot(aes(x = site, y = R2, fill = fill_key)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.85) +
    geom_text(
      aes(label = slope_label, y = R2),
      position  = position_dodge(width = 0.9),
      vjust     = -0.3,
      size      = 3,
      lineheight = 0.85
    ) +
    scale_fill_manual(values = model_colors, labels = model_labels, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    facet_wrap(~pathway, ncol = 1, labeller = labeller(pathway = pathway_labs)) +
    labs(y = expression(R^2), title = iv_label) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 9),
      strip.text       = element_text(size=11),
      panel.grid.major.x = element_blank(),
      axis.title.x = element_blank()
    )
}

# --- 7. Build and display figures ----------------------------
(q_fig <- make_r2_fig(all_df, "Q", "Discharge (Q)"))
(t_fig <- make_r2_fig(all_df, "T", "Temperature (T)"))

plot_grid(q_fig, t_fig, ncol = 2, labels = c("A", "B"))

