
library(tidyverse)
library(sf)
library(maps)
library(dataRetrieval)
library(concaveman)

# ─── Figure: Global Map of Meta-Analysis Study Site Locations ───────────────

# ─── Hotchkiss et al. (2015) USGS sampling extent ───────────────────────────
# Site IDs transcribed from the paper's own supplementary PDF (S4: USGS
# parameter codes and site IDs for CO2 estimates), 1,453 standard 8-digit
# NWIS site numbers (a handful of non-standard longer IDs at the end of the
# list were dropped -- likely groundwater wells / coordinate-based IDs, not
# comparable stream gauges). Coordinates pulled live from NWIS so this stays
# reproducible from the source ID list rather than a cached coordinate table.
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

# APPROXIMATE coordinates, catchment/study-area level (not exact sampling
# points) -- estimated from each paper's stated Location text, to be
# refined with precise site coordinates. "This Paper" uses real site
# coordinates from 01_Raw_data/Ch1 Pub Map/sites.shp instead of an estimate.
site_coords <- tribble(
  ~Citation,                    ~lat,    ~lon,
  "(Aho et al., 2021)",          41.90,  -72.90,  # Connecticut River Watershed, NW CT
  "(Bernal et al., 2022)",       41.75,    2.50,  # La Tordera catchment, Catalonia, Spain
  "(Carter et al., 2022)",       35.97,  -79.05,  # New Hope Creek, Duke Forest, NC
  "(Diamond et al., 2025)",      47.60,    2.60,  # Loire River at Dampierre, France
  "(Duvert et al., 2019)",      -13.50,  131.30,  # Daly/Howard River, Northern Territory, AUS
  "(Gong et al., 2021)",         31.30,  119.40,  # Tianmu Lake catchment, Zhejiang/Jiangsu, China
  "(Horgby et al., 2019)",       46.25,    7.10,  # Vallon de Nant, Swiss Alps
  "(Khadka et al., 2014)",       29.85,  -82.60,  # Santa Fe River watershed, north-central FL
  "(Kirk & Cohen, 2023)",        29.90,  -82.50,  # Santa Fe River network, north-central FL
  "(Lupon et al., 2019)",        64.21,   19.77,  # Krycklan catchment, near Umea, Sweden
  "(Marzolf et al., 2022)",      10.43,  -83.99,  # La Selva Biological Station, Costa Rica
  "(Moustapha et al., 2022)",     3.50,   11.50,  # Nyong watershed, Cameroon
  "(Nguyen et al., 2025)",       47.60,    2.60,  # Loire River at Dampierre, France (same site as Diamond)
  "(Rexroade et al., 2026)",    -13.13,  130.79,  # Litchfield National Park, Northern Territory, AUS
  "(Rocher-Ros et al., 2019)",   68.35,   18.82,  # Miellajokka catchment, near Abisko, Sweden
  "(Solano et al., 2023)",      -12.87,  131.12,  # Manton Creek, near Darwin, NT, AUS
  "(Taillardat et al., 2022)",   50.52,  -63.20,  # La Romaine watershed, Quebec, Canada
  "(Wang et al., 2021)",         38.28,  109.73   # Hailiutu River, Yulin City, Shaanxi, China
)

this_paper_coords <- st_read("01_Raw_data/Ch1 Pub Map/sites.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  filter(as.character(Site_ID) %in% c("5", "6", "9", "13")) %>%
  st_drop_geometry() %>%
  transmute(Citation = "This Paper", lat = Latitude, lon = Longitude)

# n_reaches = number of site/reach rows each paper contributes in the raw
# extraction (used to size each dot, so multi-site papers read as "heavier").
# This is every literature DOI regardless of the Regulated-flow/source-water
# filters applied for the statistical tests elsewhere -- a location map
# should show every paper, not just the subset that entered those tests.
lit_reach_counts <- read_csv("01_Raw_data/meta_analysis_extraction_GENERATED_v2.csv", show_col_types = FALSE) %>%
  count(Citation, name = "n_reaches")

map_points <- lit_reach_counts %>%
  left_join(site_coords, by = "Citation") %>%
  bind_rows(this_paper_coords %>% mutate(n_reaches = 1))

# Deterministic small offset for points that share (or nearly share) a
# location -- e.g. Diamond & Nguyen sample the identical Loire River site,
# and Duvert/Rexroade/Solano are all within ~0.5 degrees near Darwin, AUS --
# so every paper stays individually visible rather than fully overlapping.
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
  scale_size_area(name = "Reaches /\ntime-periods", max_size = 9, breaks = c(1, 3, 6, 9)) +
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
