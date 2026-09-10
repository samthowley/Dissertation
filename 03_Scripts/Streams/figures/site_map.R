
library(tidyverse)
library(sf)
library(maps)
library(dataRetrieval)
library(concaveman)
library(readxl)

# ─── Figure: Global Map of Meta-Analysis Study Site Locations ───────────────

# ─── Hotchkiss et al. (2015) USGS sampling extent ───────────────────────────
hotchkiss_ids <- readLines("01_Raw_data/hotchkiss_2015_usgs_site_ids.txt")
hotchkiss_sites <- readNWISsite(hotchkiss_ids) %>%
  filter(!is.na(dec_lat_va), !is.na(dec_long_va))

# Concave hull (not a plain convex hull) so the shaded region hugs the actual
# station footprint rather than smoothing over basins she didn't sample.
hotchkiss_hull <- concaveman(
  as.matrix(hotchkiss_sites[, c("dec_long_va", "dec_lat_va")]),
  concavity = 2
) %>%
  as.data.frame() %>%
  setNames(c("lon", "lat"))

# site_coords (Citation -> approximate lat/long): single source of truth,
# also used by the meta-analysis MLM scripts -- see site_coords_lookup.R.
source("03_Scripts/Streams/figures/site_coords_lookup.R")

this_paper_sites <- st_read("01_Raw_data/Ch1 Pub Map/sites.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_drop_geometry()

this_paper_coords <- this_paper_sites %>%
  filter(as.character(Site_ID) %in% c("5", "6", "9", "13")) %>%
  transmute(Citation = "This Paper", lat = Latitude, lon = Longitude)

meta_data <- read_excel("01_Raw_data/meta_analysis_v3.xlsx", sheet = "Data")

lit_reach_counts <- meta_data %>%
  group_by(Citation) %>%
  summarise(n_reaches = sum(n_reaches, na.rm = TRUE), .groups = "drop")

map_points <- lit_reach_counts %>%
  left_join(site_coords, by = "Citation") %>%
  bind_rows(this_paper_coords %>% mutate(n_reaches = 1))


map_points <- map_points %>%
  mutate(lat_bin = round(lat), lon_bin = round(lon)) %>%
  group_by(lat_bin, lon_bin) %>%
  mutate(
    n_cluster = n(),
    idx       = row_number(),
    angle     = 2 * pi * (idx - 1) / n_cluster,
    r         = if_else(n_cluster > 1, 1.3, 0),
    lat       = lat + r * sin(angle),
    lon       = lon + r * cos(angle) / cos(lat * pi / 180)
  ) %>%
  ungroup() %>%
  select(-lat_bin, -lon_bin, -n_cluster, -idx, -angle, -r)

n_lit_papers <- n_distinct(lit_reach_counts$Citation)
n_lit_reaches <- sum(lit_reach_counts$n_reaches)


# Shared Citation -> color palette (same assignment used by meta analysis.R's
# figures), so a paper's dot color here matches its color everywhere else.
source("03_Scripts/Streams/figures/citation_palette.R")
map_cols <- master_cit_cols

world_map <- map_data("world")

(p_site_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black", linewidth = 0.2) +
  geom_polygon(data = hotchkiss_hull, aes(x = lon, y = lat),
               fill = "#A8CCE0", color = "#5B8DB8", alpha = 0.45, linewidth = 0.4) +
  geom_point(data = map_points,
             aes(x = lon, y = lat, fill = Citation, size = n_reaches),
             shape = 21, color = "black", stroke = 0.6, alpha = 0.9) +
  scale_fill_manual(values = map_cols, name = "Citation") +
  scale_size_area(name = "Reaches", max_size = 9, breaks = c(1, 3, 6, 9)) +
  guides(fill = guide_legend(override.aes = list(size = 4))) +
  coord_fixed(xlim = c(-140, 155), ylim = c(-40, 75), expand = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Meta-Analysis Study Site Locations") +
  theme_classic(base_size = 13) +
  theme(
    axis.text     = element_blank(),
    axis.ticks    = element_blank(),
    axis.line     = element_blank(),
    plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.text   = element_text(size = 9)
  ))
