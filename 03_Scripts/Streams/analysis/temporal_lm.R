source("03_Scripts/Streams/analysis/data for analysis.R")

int.ext <- read_csv("04_Output/stream/external-internal.csv")

Q.lm<-rbind(
  site_lm_table_fun(int.ext, log10(internal), ID, log10(Q))%>%
    mutate(pathway='internal',
           significant=if_else(p_slope<0.01, 'Y', 'N')),
  site_lm_table_fun(int.ext, log10(external), ID, log10(Q))%>%
    mutate(pathway='external',
           significant=if_else(p_slope<0.01, 'Y', 'N'))
  )

Q.lm%>%filter(pathway=='internal')
Q.lm%>%filter(pathway=='external')


T.lm<-rbind(
  site_lm_table_fun(int.ext, log10(internal), ID, log10(TempC))%>%
    mutate(pathway='internal',
           significant=if_else(p_slope<0.01, 'Y', 'N')),
  site_lm_table_fun(int.ext, log10(external), ID, log10(TempC))%>%
    mutate(pathway='external',
           significant=if_else(p_slope<0.01, 'Y', 'N'))
  )


T.lm%>%filter(pathway=='internal')
T.lm%>%filter(pathway=='external')


test<-int.ext%>%filter(ID==6, Q<10)
range(test$Q, na.rm=T)

int.ext%>%
  pivot_longer(
    cols      = c(internal, external, CO2_flux),
    names_to  = "pathway",
    values_to = "flux"
  )%>%
  ggplot(
    aes(x = Q, y = flux,
        group = pathway, color=pathway)) +
  geom_point() +
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, ncol = 4, scales = "free") +
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Discharge'))+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  xlab(expression('Discharge'~'L'~s^-1))+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = " ~~ "), color=pathway,
        group=pathway),
    formula = y ~ x, parse = TRUE,
    size = 4, label.x = "right", label.y = "bottom", vstep = 0.05
  )+
  scale_colour_manual(
    name = "Pathway",
    values = col,
    labels = c( "Total","External", "Internal"))+
  theme_minimal()



int.ext%>%
  pivot_longer(
    cols      = c(internal, external, CO2_flux),
    names_to  = "pathway",
    values_to = "flux"
  )%>%
  ggplot(
    aes(x = TempC, y = flux,
        group = pathway, color=pathway)) +
  geom_point() +
  scale_y_log10()+
  facet_wrap(~ID, ncol = 4, scales = "free") +
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Temperature'))+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  xlab(expression("Temperature"))+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = " ~~ "), color=pathway,
        group=pathway),
    formula = y ~ x, parse = TRUE,
    size = 4, label.x = "right", label.y = "bottom", vstep = 0.05
  )+
  scale_colour_manual(
    name = "Pathway",
    values = col,
    labels = c( "Total","External", "Internal"))+
  theme_minimal()



# =============================================================================
# SITE x PATHWAY SUMMARY — slope and R2 for each CO2 category, per trend
# =============================================================================
# Two tables (discharge trend, temperature trend), sites as rows, one
# slope/R2/p-value block per CO2 category (internal, external, CO2_flux =
# total). Uses site_lm_table_fun() (same log-log per-site fits as Q.lm/T.lm
# above) so predictor definitions match this script: log10(Q) for discharge,
# log10(TempC) for temperature (NOT raw TempC — matches T.lm's convention
# above, but differs from the raw-TempC predictor used in
# lmm_outline_synthesis.R / gls_temporal_analysis.R).
# best_r2_pathway = CO2 category with the highest R2 at that site.
# largest_slope_pathway = CO2 category with the largest |slope| at that site.

build_pathway_trend_table <- function(x_col_expr) {
  cats <- c("internal", "external", "CO2_flux")
  fits <- map(cats, function(cat) {
    resp_expr <- rlang::parse_expr(paste0("log10(", cat, ")"))
    site_lm_table_fun(int.ext, !!resp_expr, ID, !!x_col_expr) %>%
      select(ID, slope, r2, p_slope) %>%
      # sig flag matches this script's own convention (Q.lm/T.lm above use
      # p_slope < 0.01), not the 0.05 threshold used elsewhere in the project
      mutate(sig = if_else(p_slope < 0.01, "Y", "N")) %>%
      rename_with(~ paste0(.x, "_", cat), c(slope, r2, p_slope, sig))
  })
  tbl <- reduce(fits, left_join, by = "ID")

  slope_cols <- paste0("slope_", cats)
  r2_cols    <- paste0("r2_", cats)

  tbl %>%
    rowwise() %>%
    mutate(
      best_r2_pathway       = cats[which.max(c_across(all_of(r2_cols)))],
      best_r2_value         = max(c_across(all_of(r2_cols))),
      largest_slope_pathway = cats[which.max(abs(c_across(all_of(slope_cols))))],
      largest_slope_value   = c_across(all_of(slope_cols))[which.max(abs(c_across(all_of(slope_cols))))]
    ) %>%
    ungroup() %>%
    rename(site = ID) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    arrange(site) %>%
    select(site,
           slope_internal, p_slope_internal, sig_internal, r2_internal,
           slope_external, p_slope_external, sig_external, r2_external,
           slope_CO2_flux, p_slope_CO2_flux, sig_CO2_flux, r2_CO2_flux,
           best_r2_pathway, best_r2_value, largest_slope_pathway, largest_slope_value)
}

discharge_trend_table   <- build_pathway_trend_table(quote(log10(Q)))
temperature_trend_table <- build_pathway_trend_table(quote(log10(TempC)))

# Print wide enough that R doesn't wrap p-value/sig columns onto an invisible
# second block (default 80-char console width was hiding them previously).
old_width <- options(width = 200)

cat("\n=====================================================================\n")
cat("DISCHARGE TREND — log10(CO2 category) ~ log10(Q), per site\n")
cat("(sig = p_slope < 0.01, matching Q.lm/T.lm's convention above)\n")
cat("=====================================================================\n")
print(as.data.frame(discharge_trend_table), row.names = FALSE)

cat("\n=====================================================================\n")
cat("TEMPERATURE TREND — log10(CO2 category) ~ log10(TempC), per site\n")
cat("(sig = p_slope < 0.01, matching Q.lm/T.lm's convention above)\n")
cat("=====================================================================\n")
print(as.data.frame(temperature_trend_table), row.names = FALSE)

options(old_width)


# =============================================================================
# PUBLICATION-STYLE FLEXTABLES — same style as drop_model_table_ft.R /
# lmm_outline_synthesis.R (Times New Roman, bold/centered header, thick
# top/bottom rules, italic 9pt footnote). save_as_docx() left commented out
# (not yet a manuscript-final table) — uncomment when ready.
# =============================================================================

style_ft <- function(ft, title, footnote) {
  ft %>%
    add_header_lines(title) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header") %>%
    align(i = 1, j = 1, align = "left", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(width = 1.5)) %>%
    hline_bottom(part = "header", border = fp_border(width = 0.75)) %>%
    hline_bottom(part = "body", border = fp_border(width = 1.5)) %>%
    add_footer_lines(footnote) %>%
    italic(part = "footer") %>%
    align(part = "footer", align = "left") %>%
    fontsize(part = "footer", size = 9) %>%
    font(fontname = "Times New Roman", part = "footer")
}

build_trend_ft <- function(tbl, predictor_label, predictor_unit) {
  flextable(tbl) %>%
    add_header_row(
      top       = TRUE,
      values    = c("Site", "Internal", "External", "Total CO2 Flux", "Best Fit", "Largest |Slope|"),
      colwidths = c(1, 4, 4, 4, 2, 2)
    ) %>%
    set_header_labels(
      site = "Site",
      slope_internal = "Slope", p_slope_internal = "p", sig_internal = "Sig.", r2_internal = "R2",
      slope_external = "Slope", p_slope_external = "p", sig_external = "Sig.", r2_external = "R2",
      slope_CO2_flux = "Slope", p_slope_CO2_flux = "p", sig_CO2_flux = "Sig.", r2_CO2_flux = "R2",
      best_r2_pathway = "Pathway", best_r2_value = "R2",
      largest_slope_pathway = "Pathway", largest_slope_value = "Slope"
    ) %>%
    merge_at(part = "header", i = 1:2, j = 1) %>%
    valign(part = "header", valign = "center") %>%
    bold(j = "site", part = "body") %>%
    align(j = "site", align = "left", part = "all") %>%
    align(j = setdiff(names(tbl), "site"), align = "center", part = "body") %>%
    fontsize(size = 9, part = "all") %>%
    width(j = "site", width = 0.4) %>%
    width(j = c("slope_internal","slope_external","slope_CO2_flux",
                 "largest_slope_value"), width = 0.6) %>%
    width(j = c("p_slope_internal","p_slope_external","p_slope_CO2_flux"), width = 0.55) %>%
    width(j = c("sig_internal","sig_external","sig_CO2_flux"), width = 0.4) %>%
    width(j = c("r2_internal","r2_external","r2_CO2_flux","best_r2_value"), width = 0.5) %>%
    width(j = c("best_r2_pathway","largest_slope_pathway"), width = 0.6) %>%
    style_ft(
      paste0("Site-level ", predictor_label, " trend: log10(CO2 category) ~ log10(", predictor_unit, "), per pathway."),
      paste0(
        "Note. Slope, p-value, and R2 from independent per-site, per-pathway log-log linear regressions ",
        "(site_lm_table_fun(), matching Q.lm/T.lm above); Total CO2 Flux = internal + external. Sig. = 'Y' if p < 0.01 ",
        "(this script's own threshold, distinct from the 0.05 threshold used in lmm_outline_synthesis.R / ",
        "gls_temporal_analysis.R). Best Fit = pathway with the highest R2 at that site. Largest |Slope| = pathway with ",
        "the largest-magnitude slope at that site, signed."
      )
    )
}

ft_discharge_trend   <- build_trend_ft(discharge_trend_table,   "discharge",   "Q")
ft_temperature_trend <- build_trend_ft(temperature_trend_table, "temperature", "TempC")

ft_discharge_trend
ft_temperature_trend

# out_dir <- "C:/Dissertation/05_Figures"
# save_as_docx(ft_discharge_trend,   path = file.path(out_dir, "TableSx_discharge_trend_by_site.docx"))
# save_as_docx(ft_temperature_trend, path = file.path(out_dir, "TableSx_temperature_trend_by_site.docx"))
