# Map for Olympia Oyster Insitu Filtration Publication
# Date created: 2022-12-02

library(sf)
library(grid)
library(ggrepel)
library(viridis)
library(ggplot2)
library(raster)

# Set file path to the downloaded raster file
# filepath <- file.path("GRAY_50M_SR_OB", "GRAY_50M_SR_OB.tif")
filepath <- file.path("./Data/map_rnaturalearth/GRAY_HR_SR_OB", "GRAY_HR_SR_OB.tif")

# Read in raster file
raster_file <- raster(filepath)

# Define extent of map
extent <- extent(-130, -114, 32, 50)

# Crop raster to extent
r_grayearth_cropped <- crop(raster_file, extent)

# Convert raster to data frame 
df_grayearth_cropped <- as.data.frame(r_grayearth_cropped, xy = TRUE)

# Define the points as a data frame
points <- data.frame(
  site_name = c("Yaquina Bay", "San Rafael Bay", "Morro Bay", "Newport Bay (Deanza)",
                "Newport Bay (Shellmaker)", "San Diego Bay"),
  lat = c(44.579247, 37.964179, 35.334707, 33.620291, 33.622097, 32.758429),
  lon = c(-123.993504, -122.487217, -120.844000, -117.89769, -117.892399, -117.237800),
  species = c("O_lurida", "O_lurida", "M_gigas", "O_lurida","O_lurida","O_lurida"),
  analysis = c("DTW, particle", "HCR", "HCR", "HCR, DTW", "HCR", "DTW")
)

# color scale consistent with Pub_Results.Rmd
# values for map
map_sites <- points$site_name
map_palette <- viridis(option = "C", n = 6, end = 0.9)
# show palette
#show_col(viridis_pal(option = "C", end = 0.9)(6))
map_site_colors <- setNames(map_palette, map_sites)

# Create ggplot base map
base_map <- ggplot(data = points, aes(x = lon, y = lat)) +
  geom_raster(data = df_grayearth_cropped, aes(x = x, y = y, fill = GRAY_HR_SR_OB)) +
  scale_fill_gradient(low = "grey45", high = "grey90") +
  # scale_color_manual(values = c("#39568CFF", "#73D055FF"),
  #                     labels = c(expression(italic("M. gigas")),
  #                                 expression(italic("O. lurida")))) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key=element_rect(fill="white"),
  ) +
  guides(fill = "none") +
  coord_sf(xlim = c(-130, -114), 
           ylim = c(32, 50), 
           crs = "+proj=longlat +datum=WGS84 +no_defs") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(-130, -125, -120, -115)) +
  geom_point(color = map_site_colors , 
             size = 2,
             position = position_jitter(width = 0.1, height = 0.1)) + 
  geom_text_repel(aes(label = site_name),
                  nudge_x = -7,
                  force = 0.6,
                  size = 3.5,
                  point.padding = 1,
                  segment.size = 0.3,
                  segment.color = "grey30",
                  box.padding = 0.25,
                  direction = "y",
                  color = "grey10") +
  labs(color = "Species") +
  ggspatial::annotation_scale(
    location = "tr",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    bar_cols = c("grey70", "grey40"),
    text_col = "black") +
  ggspatial::annotation_north_arrow(
    location = "tl", 
    which_north = "true",
    pad_x = unit(0.4, "in"), 
    pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(fill = c("grey70", "grey30"),
                                                      line_col = "grey20",
                                                      text_col = "grey10"
    ))

quartz(w = 5, h = 6)
base_map

ggsave(file.path(paste0("./Publication_Analysis/figures/Map_", 
                        Sys.Date(), 
                        ".eps")), 
       plot = base_map,  
       dpi = 600, 
       width = 6, 
       height = 6, 
       device = "eps")

