source("03_Scripts/Streams/analysis/data for analysis.R")

site_specific_results <- read_csv("04_Output/stream/models/site_specific_results.csv")%>%
  rename(ID=site)

site_specific_results%>%
  ggplot(aes(x=ID, y=Estimate, color=indep.var, shape=pathway))+
  geom_point(size=4)

basin_area <- read_csv("01_Raw_data/wetland cover/basin_area.csv")%>%
  select(Basin, Shape_Area)%>%rename(ID=Basin)

pH.avg<-pH%>%
  group_by(ID)%>%summarise(pH=mean(pH, na.rm=T))

left_join(site_specific_results, pH.avg)%>%
  filter(indep.var=='TempC')%>%
  ggplot(aes(x=pH, y=Estimate, color=pathway, shape = ID))+
  geom_point(size=4)



SpC.avg<-SpC%>%
  group_by(ID)%>%summarise(SpC=mean(SpC, na.rm=T))

Q.avg<-discharge%>%
  group_by(ID)%>%summarise(Q=mean(Q, na.rm=T))%>%
  left_join(basin_area)%>%
  mutate(q=Q/Shape_Area)

wetland_perc <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")

#drop############
dropT <- read_csv("04_Output/stream/models/dropT.csv")%>%
  mutate(
    dropped_from=if_else(is.na(dropped_from), 'full', dropped_from)
  )
dropQ <- read_csv("04_Output/stream/models/dropQ.csv")%>%
  mutate(
    dropped_from=if_else(is.na(dropped_from), 'full', dropped_from)
  )


#dropped Q                ################
shape_key <- c('full' = 16, 'lint' = 17, 'lext' = 15, 'both' = 18)

common_layers <- list(
  geom_point(size = 4),
  scale_color_viridis_c(),
  scale_shape_manual(values = c('full' = 16, 'lint' = 17, 'lext' = 15, 'both' = 18)),
  theme_minimal()
)

a<-dropQ %>%
  filter(dropped_from %in% c('lint', 'full'),
         pathway == 'lint',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  common_layers +
  ggtitle("Discharge Dropped: Internal")

b<-dropQ %>%
  filter(dropped_from %in% c('lext', 'full'),
         pathway == 'lext',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  common_layers +
  ggtitle("Discharge Dropped: External")

c<-dropQ %>%
  filter(dropped_from %in% c('full', 'both'),
         pathway == 'lext',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  common_layers +
  ggtitle("Discharge Dropped: Both Pathways")

(g<-plot_grid(a,b,c, ncol=3))

full.for.q<-site_specific_results%>%
  filter(indep.var=='TempC')%>%
  rename(indep=indep.var, site=ID)%>%
  select(site, pathway, Estimate, sigma)

dropQ_wide <- dropQ %>%
  pivot_wider(
    id_cols = c(pathway, indep, site, test),
    names_from = dropped_from,
    values_from = c(Estimate, sigma)
  ) %>%
  drop_na(Estimate_lint, Estimate_lext, Estimate_both) %>%
  select(-Estimate_full, -sigma_full) %>%
  left_join(full.for.q) %>%
  mutate(
    lint.diff = Estimate_lint - Estimate,
    lint.sigma.diff = sigma_lint - sigma,
    lext.diff = Estimate_lext - Estimate,
    lext.sigma.diff = sigma_lext - sigma,
    both.diff = Estimate_both - Estimate,
    both.sigma.diff = sigma_both - sigma
  )
  


d<-dropQ_wide %>%
  pivot_longer(cols = c(lint.diff, lext.diff, both.diff),
               names_to = "dropped_from",
               values_to = "diff") %>%
  ggplot(aes(x = as.factor(site), y = diff, color = dropped_from, shape = dropped_from)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis_d() +
  theme_minimal() +
  facet_wrap(~dropped_from + indep, scales = "free") +
  labs(y = "Difference in Estimate from Full Model", x = "Site")

e<-dropQ_wide %>%
  pivot_longer(cols = c(lint.sigma.diff, lext.sigma.diff, both.sigma.diff),
               names_to = "dropped_from",
               values_to = "sigma.diff") %>%
  ggplot(aes(x = as.factor(site), y = sigma.diff, color = dropped_from, shape = dropped_from)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis_d() +
  theme_minimal() +
  facet_wrap(~dropped_from + indep, scales = "free") +
  labs(y = "Difference in Sigma from Full Model", x = "Site")

plot_grid(g, d, e,ncol=1)


# ============================================================
# R² Summary Figure: Q-graph and T-graph
# ============================================================
library(brms)

# --- 1. Full model -------------------------------------------
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
        paste0("Q:", round(est_lQ, 3), "\nT:", round(est_TempC, 3)),
      !is.na(est_lQ)   ~ paste0("Q:", round(est_lQ, 3)),
      !is.na(est_TempC) ~ paste0("T:", round(est_TempC, 3)),
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

pathway_labs <- c(lint = "Internal (lint)", lext = "External (lext)")

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
      size      = 2.2,
      lineheight = 0.85
    ) +
    scale_fill_manual(values = model_colors, labels = model_labels, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    facet_wrap(~pathway, ncol = 1, labeller = labeller(pathway = pathway_labs)) +
    labs(x = "Site ID", y = expression(R^2), title = iv_label) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 9),
      strip.text       = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )
}

# --- 7. Build and display figures ----------------------------
q_fig <- make_r2_fig(all_df, "Q", "Discharge (Q)")
t_fig <- make_r2_fig(all_df, "T", "Temperature (T)")

plot_grid(q_fig, t_fig, ncol = 2, labels = c("A", "B"))
