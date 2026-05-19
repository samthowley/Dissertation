# Bradford Experimental Forest — Site Map
# Chapter 1 publication figure
#
# Packages needed (run once if not already installed):
# install.packages(c("sf", "ggspatial", "ggrepel", "cowplot"))

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)    # scale bar + north arrow
library(ggrepel)      # non-overlapping site labels
library(cowplot)      # inset map
library(maps)               # Florida outline for inset (bundled with R)


# ---- Load shapefiles --------------------------------------------------------

data_path <- "01_Raw_data/Ch1 Pub Map"

sites      <- st_read(file.path(data_path, "sites.shp"),      quiet = TRUE)
streams    <- st_read(file.path(data_path, "streams.shp"),    quiet = TRUE)
wetlands   <- st_read(file.path(data_path, "wetlands.shp"),   quiet = TRUE)
watersheds <- st_read(file.path(data_path, "New Watersheds/BradfordWatershedsApr2025.shp"),
                      quiet = TRUE)


# ---- Clean & prepare --------------------------------------------------------

# Remove site 6a
sites <- sites %>% filter(Site_ID != "6a")

# Dissolve all watershed polygons into a single forest boundary
forest_boundary <- watersheds %>% st_union()

# Drop Z/M dimensions if present (some shapefiles exported from 3D sources carry
# a vertical component that breaks st_transform)
sites           <- st_zm(sites,           drop = TRUE)
streams         <- st_zm(streams,         drop = TRUE)
wetlands        <- st_zm(wetlands,        drop = TRUE)
forest_boundary <- st_zm(forest_boundary, drop = TRUE)

# Reproject everything to UTM Zone 17N (EPSG:32617)
# UTM gives units in metres, which ggspatial needs for an accurate scale bar
utm17 <- 32617

sites           <- st_transform(sites,           utm17)
streams         <- st_transform(streams,         utm17)
wetlands        <- st_transform(wetlands,        utm17)
forest_boundary <- st_transform(forest_boundary, utm17)

# Clip streams to a small buffer around the forest so the map stays focused
streams <- st_intersection(streams, st_buffer(forest_boundary, 1500))

# Compute the forest bounding box — used to set the map extent
forest_bbox <- st_bbox(forest_boundary)
map_buffer  <- 1000  # metres of white space around the forest


# ---- Main map ---------------------------------------------------------------

main_map <- ggplot() +

  # Forest boundary — drawn first so everything else sits on top
  geom_sf(data = forest_boundary,
          fill = NA, color = "black", linewidth = 0.8) +

  # Wetland cover (green, semi-transparent)
  geom_sf(data = wetlands,
          aes(fill = "Wetland"), color = NA, alpha = 0.5) +

  # Streams
  geom_sf(data = streams,
          aes(color = "Stream"), linewidth = 0.55) +

  # Sampling sites — open circles so the white fill shows against the map
  geom_sf(data = sites,
          aes(color = "Sampling site"), shape = 21,
          fill = "white", size = 3.5, stroke = 1) +

  # Site ID labels — ggrepel keeps them from overlapping each other or the points
  geom_label_repel(
    data          = sites,
    aes(label     = Site_ID, geometry = geometry),
    stat          = "sf_coordinates",
    size          = 3,
    label.padding = unit(0.15, "lines"),
    min.segment.length = 0.3,
    seed          = 42
  ) +

  # Legend colours and fills
  scale_color_manual(
    name   = NULL,
    values = c("Stream" = "#4a90d9", "Sampling site" = "black")
  ) +
  scale_fill_manual(
    name   = NULL,
    values = c("Wetland" = "#2d8a4e")
  ) +

  # Scale bar (bottom-left) and north arrow (top-right)
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.75) +
  annotation_north_arrow(
    location    = "tl",
    which_north = "true",
    style       = north_arrow_minimal(text_size = 8)
  ) +

  # Zoom to forest extent + a small buffer so streams don't sprawl off-screen
  coord_sf(
    xlim = c(forest_bbox["xmin"] - map_buffer, forest_bbox["xmax"] + map_buffer),
    ylim = c(forest_bbox["ymin"] - map_buffer, forest_bbox["ymax"] + map_buffer)
  ) +

  labs(title = "Bradford Experimental Forest") +
  theme_minimal() +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 10),
    legend.box       = "horizontal"
  ) +
  guides(
    color = guide_legend(order = 1),
    fill  = guide_legend(order = 2)
  )


# ---- Inset map (Florida) ----------------------------------------------------

# maps::map() is bundled with R — no extra install needed
florida <- st_as_sf(maps::map("state", regions = "florida", plot = FALSE, fill = TRUE)) %>%
  st_transform(utm17)

# Single point representing the centre of the forest
forest_centroid <- st_centroid(forest_boundary)

inset <- ggplot() +
  geom_sf(data = florida, fill = "gray85", color = "gray50", linewidth = 0.3) +
  geom_sf(data = forest_centroid, color = "red", size = 3) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))


# ---- Combine main map + inset -----------------------------------------------
# Adjust x, y, width, height to reposition the inset if needed

final_map <- ggdraw() +
  draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA))) +
  draw_plot(main_map) +
  draw_plot(inset, x = 0.70, y = 0.12, width = 0.22, height = 0.22)

final_map


# ---- Save -------------------------------------------------------------------

ggsave("04_Output/Ch1_site_map.png",
       plot   = final_map,
       width  = 8, height = 7, dpi = 300)
