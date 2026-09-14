
library(tidyverse)
library(readxl)
library(flextable)
library(officer)

meta_path <- "01_Raw_data/meta_analysis_v3.xlsx"
data_raw  <- read_excel(meta_path, sheet = "Data")

# ── State/Country lookup ────────────────────────────────────────────────────
# Citation-level, hand-curated from each paper's Location description
# (Paper_Info sheet) -- the same source used for site_coords_lookup.R's
# lat/long, kept here as a companion table since that file is scoped to
# coordinates only. One override below (Gong et al., 2021) splits by
# Site_ID because that paper's two catchments sit in different provinces.
citation_location <- tribble(
  ~Citation,                       ~State,                ~Country,
  "(Aho et al., 2021)",            "Connecticut",         "USA",
  "(Bega et al., 2026)",           "São Paulo",           "Brazil",
  "(Bernal et al., 2022)",         "Catalonia",            "Spain",
  "(Bertuzzo et al., 2022)",       "Wisconsin",            "USA",
  "(Carter et al., 2022)",         "North Carolina",       "USA",
  "(Crawford et al., 2014)",       "Wisconsin",            "USA",
  "(Demars, 2019)",                "Aberdeenshire, Scotland", "UK",
  "(Duvert et al., 2019)",         "Northern Territory",   "Australia",
  "(Gong et al., 2021)",           NA_character_,          "China",
  "(Gómez-Gener et al., 2016)",    "Catalonia",            "Spain",
  "(Hall et al., 2026)",           "Montana",              "USA",
  "(Khadka et al., 2014)",         "Florida",              "USA",
  "(Kirk & Cohen, 2023)",          "Florida",              "USA",
  "(Leng et al., 2025)",           "Saxony-Anhalt",        "Germany",
  "(Liu et al., 2026)",            NA_character_,          "China",
  "(Lupon et al., 2019)",          "Västerbotten",         "Sweden",
  "(Marzolf et al., 2022)",        "Heredia",              "Costa Rica",
  "(Moustapha et al., 2022)",      NA_character_,          "Cameroon",
  "(Nguyen et al., 2025)",         "Centre-Val de Loire",  "France",
  "(Oviedo-Vargas et al., 2015)",  "Heredia",              "Costa Rica",
  "(Piatka et al., 2024)",         "Bavaria",              "Germany",
  "(Pu et al., 2017)",             "Guangxi",              "China",
  "(Rasilo et al., 2017)",         "Québec",               "Canada",
  "(Rexroade et al., 2026)",       "Northern Territory",   "Australia",
  "(Rocher-Ros et al., 2020)",     "Norrbotten",           "Sweden",
  "(Shangguan et al., 2026)",      "Montana",              "USA",
  "(Solano et al., 2023)",         "Northern Territory",   "Australia",
  "(Taillardat et al., 2022)",     "Québec",               "Canada",
  "(Wang et al., 2021)",           "Shaanxi",              "China",
  "(Wang et al., 2023)",           "Shaanxi",              "China"
)

# ── Predictor availability per row ──────────────────────────────────────────
predictor_cols <- c(Temperature_C = "Temperature",
                     pH = "pH",
                     Discharge_m3s = "Discharge",
                     Mean_Annual_Precipitation_cm_yr = "MAP")

predictors_available <- data_raw %>%
  select(Row_ID, all_of(names(predictor_cols))) %>%
  pivot_longer(-Row_ID, names_to = "predictor", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(label = predictor_cols[predictor]) %>%
  group_by(Row_ID) %>%
  summarise(Predictors = paste(label, collapse = ", "), .groups = "drop")

# ── Assemble inventory ───────────────────────────────────────────────────────
inventory <- data_raw %>%
  left_join(citation_location, by = "Citation") %>%
  left_join(predictors_available, by = "Row_ID") %>%
  mutate(
    State = case_when(
      Citation == "(Gong et al., 2021)" & str_detect(Site_ID, "TLC") ~ "Jiangsu",
      Citation == "(Gong et al., 2021)" & str_detect(Site_ID, "YRC") ~ "Zhejiang",
      TRUE ~ State
    ),
    Predictors = replace_na(Predictors, "")
  ) %>%
  transmute(
    Citation,
    Stream = Site_ID,
    State,
    Country,
    n = n_reaches,
    Predictors
  ) %>%
  arrange(Citation, Stream)

# ── Flextable ────────────────────────────────────────────────────────────────
base_ft_style <- function(ft) {
  ft %>%
    font(fontname = "Aptos", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    border_remove() %>%
    hline_top(part = "header",    border = fp_border(width = 2)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
    height_all(height = 0.25)
}

ft <- flextable(inventory) %>%
  set_header_labels(
    Citation    = "Citation",
    Stream      = "Stream",
    State       = "State",
    Country     = "Country",
    n           = "n",
    Predictors  = "Predictor variables reported"
  ) %>%
  base_ft_style() %>%
  align(align = "left", part = "all") %>%
  align(j = "n", align = "center", part = "all") %>%
  merge_v(j = c("Citation", "State", "Country")) %>%
  valign(valign = "top", part = "body") %>%
  width(j = "Citation",   width = 1.5) %>%
  width(j = "Stream",     width = 2.2) %>%
  width(j = "State",      width = 1.1) %>%
  width(j = "Country",    width = 0.9) %>%
  width(j = "n",          width = 0.4) %>%
  width(j = "Predictors", width = 1.8)

# ── Distribution histograms: NEP (GPP - ER) and CO2 flux ────────────────────
# NEP = GPP - ER (net ecosystem production; positive = net autotrophic,
# negative = net heterotrophic) -- not a column in the workbook itself, but
# derivable wherever both GPP_gCm2day and ER_gCm2day are reported (59 of 90
# rows; the other 31 lack one or both). CO2_flux_gCm2day has no missingness.
dist_data <- data_raw %>%
  mutate(NEP_gCm2day = GPP_gCm2day - ER_gCm2day)

hist_theme <- theme_classic(base_size = 12) %+replace%
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0))

library(cowplot)
plot_grid(
p_hist_co2 <- ggplot(dist_data, aes(x = CO2_flux_gCm2day)) +
  geom_histogram(bins = 20, fill = "grey70", color = "white") +
  labs(title = paste0("CO2 flux (n = ", sum(!is.na(dist_data$CO2_flux_gCm2day)), ")"),
       x = expression("CO"[2]~"flux (g C "*m^-2~day^-1*")"), y = "Count") +
  hist_theme
,
p_hist_nep <- ggplot(dist_data, aes(x = NEP_gCm2day)) +
  geom_histogram(bins = 20, fill = "grey70", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = paste0("NEP = GPP - ER (n = ", sum(!is.na(dist_data$NEP_gCm2day)), ")"),
       x = expression("NEP (g C "*m^-2~day^-1*")"), y = "Count") +
  hist_theme

,
ncol=2)
