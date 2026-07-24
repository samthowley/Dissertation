
source("03_Scripts/Streams/analysis/spearman_rank_analysis.R")

pubs <- pubs %>%
  mutate(Biome4 = case_when(
    grepl("Subtropical",           Biome, ignore.case = TRUE) ~ "Subtropical",
    grepl("Tropical|Neotropical",  Biome, ignore.case = TRUE) ~ "Tropical",
    grepl("Boreal|Arctic",         Biome, ignore.case = TRUE) ~ "Boreal",
    grepl("Temperate|Mediterranean", Biome, ignore.case = TRUE) ~ "Temperate",
    TRUE ~ NA_character_
  ))


pubs <- pubs %>%
  mutate(source.water = case_when(
    Source %in% c("Groundwater-dominated", "Groundwater-fed", "Spring-fed") ~ "Deep groundwater",
    Source %in% c("Riparian groundwater", "Shallow Aquifer")                ~ "Shallow groundwater",
    Source %in% c("Mixed", "Snowmelt + Groundwater Baseflow",
                  "Wetland Seepage", "Wetland porewater")                   ~ "Mixed",
    TRUE ~ NA_character_
  ))

pubs_biome <- pubs %>%
  group_by(Citation, Biome4) %>%
  summarise(log_ratio = mean(log_ratio, na.rm = TRUE), n_obs = n(), .groups = "drop")

pubs_source <- pubs %>%
  group_by(Citation, source.water) %>%
  summarise(log_ratio = mean(log_ratio, na.rm = TRUE), n_obs = n(), .groups = "drop")

# ── Effect size: eta-squared and omega-squared ────────────────────────────────

eta_omega_squared <- function(aov_fit) {
  a         <- summary(aov_fit)[[1]]
  ss_effect <- a[1, "Sum Sq"]
  ss_resid  <- a[2, "Sum Sq"]
  df_effect <- a[1, "Df"]
  ms_resid  <- a[2, "Mean Sq"]
  ss_total  <- ss_effect + ss_resid

  data.frame(
    eta_squared   = round(ss_effect / ss_total, 3),
    omega_squared = round((ss_effect - df_effect * ms_resid) / (ss_total + ms_resid), 3)
  )
}

# =============================================================================
# TABLE 1 — Landscape composition by biome / source-water category
# =============================================================================

build_composition_table <- function(pubs_grouped, group_col, title_text) {
  tbl_data <- pubs_grouped %>%
    arrange(.data[[group_col]], Citation) %>%
    group_by(.data[[group_col]]) %>%
    mutate(Group = paste0(.data[[group_col]], " (n = ", n(), ")")) %>%
    ungroup() %>%
    mutate(Citation   = if_else(Citation == "This Paper", "This study", Citation),
           log_ratio  = round(log_ratio, 2)) %>%
    select(Group, Citation, n_obs, log_ratio)

  flextable(tbl_data) %>%
    merge_v(j = "Group") %>%
    set_header_labels(Group = "Category", Citation = "Citation",
                      n_obs = "Raw obs.", log_ratio = "log10(μ int:ext)") %>%
    font(fontname = "Aptos", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    align(j = 1:2, align = "left",   part = "all") %>%
    align(j = 3:4, align = "center", part = "all") %>%
    valign(j = 1, valign = "top", part = "body") %>%
    bold(part = "header") %>%
    bold(j = 1, part = "body") %>%
    border_remove() %>%
    hline_top(part = "header",  border = fp_border(width = 2)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
    width(j = 1, width = 1.8) %>%
    width(j = 2, width = 2.2) %>%
    width(j = 3:4, width = 1.0) %>%
    height_all(height = 0.25) %>%
    add_header_lines(title_text) %>%
    bold(part = "header", i = 1) %>%
    align(part = "header", i = 1, align = "left") %>%
    add_footer_lines(paste0(
      "Note. log10(μ int:ext) = landscape-mean log10(internal / external CO2 flux); ",
      "positive = internal-dominant, negative = external-dominant. Raw obs. = number of ",
      "individual reported measurements averaged into that landscape."
    )) %>%
    italic(part = "footer") %>%
    align(part = "footer", align = "left") %>%
    fontsize(part = "footer", size = 10)
}

ft_1a <- build_composition_table(
  pubs_biome, "Biome4",
  "Table 1a. Landscape composition of the biome categories (literature meta-analysis + this study)."
)

ft_1b <- build_composition_table(
  pubs_source, "source.water",
  "Table 1b. Landscape composition of the source-water categories (literature meta-analysis + this study)."
)


# =============================================================================
# TABLE 2 — Omnibus tests + Tukey's HSD
# =============================================================================

biome_aov  <- aov(log_ratio ~ Biome4,       data = pubs_biome)
source_aov <- aov(log_ratio ~ source.water, data = pubs_source)

kw_biome  <- kruskal.test(log_ratio ~ Biome4,       data = pubs_biome)
kw_source <- kruskal.test(log_ratio ~ source.water, data = pubs_source)
eff_biome  <- eta_omega_squared(biome_aov)
eff_source <- eta_omega_squared(source_aov)
smry_biome  <- summary(biome_aov)[[1]]
smry_source <- summary(source_aov)[[1]]

tbl_2a_data <- data.frame(
  Factor = c("Biome", "Source water"),
  n      = c(nrow(pubs_biome), nrow(pubs_source)),
  F_df   = c(
    paste0(round(smry_biome[1, "F value"], 2),  " (", smry_biome[1, "Df"],  ", ", smry_biome[2, "Df"],  ")"),
    paste0(round(smry_source[1, "F value"], 2), " (", smry_source[1, "Df"], ", ", smry_source[2, "Df"], ")")
  ),
  p_anova = round(c(smry_biome[1, "Pr(>F)"], smry_source[1, "Pr(>F)"]), 3),
  H_df    = c(
    paste0(round(kw_biome$statistic, 2),  " (", kw_biome$parameter,  ")"),
    paste0(round(kw_source$statistic, 2), " (", kw_source$parameter, ")")
  ),
  p_kw    = round(c(kw_biome$p.value, kw_source$p.value), 3),
  omega2  = c(eff_biome$omega_squared, eff_source$omega_squared)
)

ft_2a <- flextable(tbl_2a_data) %>%
  add_header_row(values = c("", "", "ANOVA", "Kruskal-Wallis", ""),
                 colwidths = c(1, 1, 2, 2, 1)) %>%
  set_header_labels(Factor = "Factor", n = "n", F_df = "F (df)", p_anova = "p",
                    H_df = "H (df)", p_kw = "p", omega2 = "ω²") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1,   align = "left",   part = "all") %>%
  align(j = 2:7, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",  border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  hline(part = "header", i = 1, border = fp_border(width = 0.5)) %>%
  vline(j = 2, part = "all", border = fp_border(width = 0.5, style = "dashed")) %>%
  vline(j = 4, part = "all", border = fp_border(width = 0.5, style = "dashed")) %>%
  width(j = 1, width = 1.4) %>%
  width(j = 2:6, width = 0.85) %>%
  width(j = 7, width = 0.7) %>%
  height_all(height = 0.25) %>%
  add_header_lines(paste0(
    "Table 2a. Do biome and source water predict CO2 pathway predominance ",
    "(landscape-level log10(internal/external))?"
  )) %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(paste0(
    "Note. ω² = omega-squared effect size. Rank-based checks (Kruskal-Wallis; ",
    "pairwise Wilcoxon, not shown) support the same conclusions but have limited power ",
    "at this sample size."
  )) %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

tukey_mat <- TukeyHSD(biome_aov)$Biome4
tbl_2b_data <- data.frame(
  Comparison = rownames(tukey_mat),
  diff       = round(tukey_mat[, "diff"], 2),
  CI         = paste0("[", round(tukey_mat[, "lwr"], 2), ", ", round(tukey_mat[, "upr"], 2), "]"),
  p_adj      = round(tukey_mat[, "p adj"], 3)
) %>%
  arrange(p_adj)

ft_2b <- flextable(tbl_2b_data) %>%
  set_header_labels(Comparison = "Comparison", diff = "Difference",
                    CI = "95% CI", p_adj = "p (adj)") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1,   align = "left",   part = "all") %>%
  align(j = 2:4, align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  border_remove() %>%
  hline_top(part = "header",  border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1, width = 2.0) %>%
  width(j = 2:4, width = 1.1) %>%
  height_all(height = 0.25) %>%
  add_header_lines("Table 2b. Tukey's HSD pairwise comparisons for Biome (Source water omitted -- ANOVA not significant).") %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines("Note. Difference = mean log10(internal/external) difference between the two groups. 95% family-wise confidence level.") %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

save_as_docx(ft_1a, ft_1b, ft_2a, ft_2b, path = "05_Figures/TableS_categorical_metanalysis.docx")
