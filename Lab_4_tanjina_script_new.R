# Load necessary libraries
library(sf)
library(tidyverse)
library(tmap)
library(terra)

###Ohio Scale###
# Load data
oh2020 <- read_csv("./static_mapping/oh_counties_DP2020.csv")
counties <- read_sf("./static_mapping/oh_counties.gpkg")

# Check CRS (important for scalebar positioning)
st_crs(counties)  # If it's in degrees, consider transforming to a projected CRS
oh2020 <- oh2020 %>%
  mutate(GEOID = str_sub(geoid, -5))

# Join
counties_joined <- left_join(counties, oh2020, by = "GEOID")

# Map
tmap_mode("plot")

tm_shape(counties_joined) +
  tm_fill(col = "poptotal",
          palette = "brewer.Blues",
          style = "kmeans",
          title = "Total Population") +
  tm_borders(col = "black", lwd = 2, lty = "dashed") +
  tm_scalebar(position = c("left", "top"),
              breaks = c(0, 100, 200),   # Add scale values manually
              text.size = 1) +
  tm_compass(position = c("right", "top"), size = 2) +
  tm_layout(legend.outside = TRUE,
            frame = FALSE,
            main.title = "Ohio Counties by Population",
            main.title.size = 2)

###Local Scale###

library(sf)
library(tidyverse)
library(tmap)

# Read spatial data
maindt <- read_sf("./static_mapping/oh_places.gpkg")
counties <- read_sf("./static_mapping/oh_counties.gpkg")
parks <- read_sf("./static_mapping/oh_parks.gpkg")
streams <- read_sf("./static_mapping/oh_rivers.gpkg")

# Select Summit and Portage counties
rtcounty <- filter(counties, NAME %in% c("Summit", "Portage"))

# Transform CRS for consistent spatial operations
parks <- st_transform(parks, st_crs(rtcounty))
streams <- st_transform(streams, st_crs(rtcounty))
sf::sf_use_s2(FALSE)

# Clip data to county boundaries
dat <- st_intersection(maindt, rtcounty)
parkdat <- st_intersection(parks, rtcounty)
streamdat <- st_intersection(streams, rtcounty)

# Set tmap mode
tmap_mode("plot")

# Build each layer
boundary <- tm_shape(rtcounty) +
  tm_fill(col = I("gray90")) +
  tm_borders()

parkmap <- tm_shape(parkdat) +
  tm_polygons(fill = "FEATTYPE", palette = "brewer.greens",title = "Park Type") +
  tm_borders()

streammap <- tm_shape(streamdat) +
  tm_lines(col = I("darkblue"), lwd = 6)
streammap

# Add municipal boundaries with labels to the map
# Convert geometry collections to usable label positions
dat_label_points <- st_collection_extract(dat, "POLYGON") |> 
  st_point_on_surface()

# Collapse multiple geometries with same name into one
dat_dedup <- dat %>%
  group_by(NAME) %>%
  summarise(do_union = TRUE) %>%
  st_transform(26917)

label_points <- st_point_on_surface(dat_dedup)

munmap <- boundary +
  tm_shape(dat) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(label_points) +
  tm_text("NAME", size = 0.8, col = "red", remove.overlap = FALSE)

munmap

# Park maps
parkmap <- tm_shape(rtcounty) +
  tm_polygons(col = "gray80") +
  tm_shape(parkdat) +
  tm_polygons(fill = "FEATTYPE", palette = "brewer.greens",
              fill.legend = tm_legend(title = "Park Type"))

parkmap

#map with stream

stream <- tm_shape(rtcounty) +
  tm_polygons(col = "gray80") +
  
  tm_shape(parkdat) +
  tm_polygons(fill = "FEATTYPE", 
              fill.scale = tm_scale(values = "brewer.greens"),
              fill.legend = tm_legend(title = "Park Type")) +
  
  tm_shape(streamdat) +
  tm_lines(col = "skyblue", lwd = 3) +
  
  tm_shape(stream.park) +
  tm_lines(col = "blue", lwd = 6)

stream

###Puting it all together###

#1

# Set tmap mode
# Load DEM raster
neoh_dem <- rast("./static_mapping/neoh_dem.tif")

# Reproject county boundary to match DEM CRS
rtcounty_raster_crs <- st_transform(rtcounty, crs = crs(neoh_dem))

# Crop and mask the DEM to Portage & Summit counties
neoh_crop <- crop(neoh_dem, vect(rtcounty_raster_crs))
neoh_mask <- mask(neoh_crop, vect(rtcounty_raster_crs))


# Set tmap mode to plotting
tmap_mode("plot")

elevation_map <- tm_shape(neoh_mask) +
  tm_raster(style = "fixed",
            breaks = c(500, 700, 900, 1100, 1300, 1500),
            palette = rev(RColorBrewer::brewer.pal(7, "Spectral")),  # Reversed palette
            title = "Elevation (m)") +
  
  tm_shape(rtcounty_raster_crs) +
  tm_fill(col = "gray", alpha = 0.2) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_layout(main.title = "Elevation Map: Portage & Summit Counties",
            main.title.size = 1.4,
            legend.outside = TRUE,
            frame = FALSE)

elevation_map


#2

elevation_map_N <- tm_shape(neoh_mask) +
  tm_raster(style = "fixed",
            breaks = c(500, 700, 900, 1100, 1300, 1500),
            palette = rev(RColorBrewer::brewer.pal(7, "Spectral")),  # Reversed palette
            title = "Elevation (m)") +
  
  tm_shape(rtcounty_raster_crs) +
  tm_fill(col = "gray", alpha = 0.2) +
  tm_borders(col = "black", lwd = 2) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 3) +
  tm_layout(main.title = "Elevation Map: Portage & Summit Counties",
            main.title.size = 1.4,
            legend.outside = TRUE,
            frame = FALSE)

elevation_map_N

#3
library(grid)  # For viewport()

# === Add park and stream layers to elevation map ===
elevation_map <- elevation_map +
  tm_shape(parkdat) +
  tm_polygons(fill = "FEATTYPE", palette = "matplotlib.greens", title = "Park Type", fill_alpha = 0.2) +
  tm_borders() +
  
  tm_shape(streamdat) +
  tm_lines(col = "darkblue", lwd = 6) +
  
  tm_scale_bar(position = c("right", "top"),
               breaks = c(0, 10, 30),
               text.size = 0.8) +
  
  tm_layout(main.title = "Elevation Map: Portage & Summit Counties",
            main.title.size = 1.4,
            legend.outside = TRUE,
            frame = FALSE)

# === Plot main map ===
elevation_map

# === Add Ohio map as inset in top-left corner ===
print(oh_map, vp = grid::viewport(x = .7, y = 0.2, width = 0.4, height = 0.3))
-------------------------------------------------------------------------------------------------------------------------------
  
  
# === Load spatial layers ===
counties <- read_sf("./static_mapping/oh_counties.gpkg")
places <- read_sf("./static_mapping/oh_places.gpkg")
rivers <- read_sf("./static_mapping/oh_rivers.gpkg")
parks <- read_sf("./static_mapping/oh_parks.gpkg")
pop_data <- read_csv("./static_mapping/oh_counties_DP2020.csv")

# === Prepare population data ===
pop_data <- pop_data %>%
  mutate(GEOID = str_sub(geoid, -5))

counties_joined <- left_join(counties, pop_data, by = "GEOID") %>%
  mutate(pop_density = poptotal / (ALAND / 1e6))  # people per sq.km

# === Reproject all to a common CRS ===
common_crs <- st_crs(counties_joined)

places_proj <- st_transform(places, crs = common_crs)
rivers_proj <- st_transform(rivers, crs = common_crs)
parks_proj <- st_transform(parks, crs = common_crs)

rivers_proj <- rivers_proj[!st_is_empty(rivers_proj), ]
parks_proj <- parks_proj[!st_is_empty(parks_proj), ]

# === Add dummy attributes for legend categories ===
rivers_proj$type <- "Rivers / Streams"
parks_proj$type <- "Parks / Green Spaces"

# === Define and filter major cities ===
major_cities <- c("Columbus", "Cleveland", "Cincinnati", "Toledo", "Akron", "Dayton", "Youngstown")
major_places <- places_proj %>% filter(NAME %in% major_cities)
city_labels <- st_point_on_surface(major_places)

# === Build the map ===
tmap_mode("plot")

map <- tm_shape(counties_joined) +
  tm_polygons(
    col = "pop_density",
    palette = "Reds",
    style = "quantile",
    title = "Population Density\n(people/sq.km)",
    alpha = 0.75
  ) +
  tm_borders(col = "gray30", lwd = 0.7) +
  
  tm_shape(places_proj) +
  tm_borders(col = "black", lwd = 0.5, lty = "dotted") +
  
  tm_shape(parks_proj) +
  tm_polygons(
    col = "type",
    palette = c("Parks / Green Spaces" = "green"),
    alpha = 0.4,
    title = ""
  ) +
  
  tm_shape(rivers_proj) +
  tm_lines(
    col = "type",
    palette = c("Rivers / Streams" = "blue"),
    lwd = 0.8,
    title.col = ""
  ) +
  
  # Manual legend entry for municipal boundaries
  tm_add_legend(type = "line", col = "black", lty = "dotted", lwd = 0.7, label = "Municipal Boundaries") +
  
  # === Add major city labels ===
  tm_shape(city_labels) +
  tm_text("NAME", size = 0.6, col = "yellow", fontface = "bold", shadow = TRUE) +
  
  # === Layout with fixed outer margin for title ===
  tm_layout(
    main.title = "        Ohio: Population Density, Green Spaces, Water, and Major Cities",
    main.title.size = 1.2,
    main.title.position = "center",
    outer.margins = c(0.05, 0.01, 0.02, 0.01),  # top, right, bottom, left
    legend.outside = TRUE,
    legend.title.size = 1.1,
    frame = FALSE
  ) +
  tm_compass(position = c("right", "top"), type = "arrow", size = 2.5) +
  tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 50, 100), text.size = 0.8)

# === Display the map ===
map


