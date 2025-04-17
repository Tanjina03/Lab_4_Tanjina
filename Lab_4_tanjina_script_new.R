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

# Fix GEOID format mismatch
oh2020 <- oh2020 %>%
  mutate(GEOID = str_sub(geoid, -5))

# Join
counties_joined <- left_join(counties, oh2020, by = "GEOID")

# Optional: reproject to meters (e.g., Ohio North NAD83 / UTM Zone 17N)
# counties_joined <- st_transform(counties_joined, 26917)

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

# Turn off s2 geometry engine (helps with topological operations)
sf::sf_use_s2(FALSE)

# Clip data to county boundaries
dat <- st_intersection(maindt, rtcounty)
parkdat <- st_intersection(parks, rtcounty)
streamdat <- st_intersection(streams, rtcounty)

# Set tmap mode
tmap_mode("plot")

# Build each layer (v4 style with adjusted params)
boundary <- tm_shape(rtcounty) +
  tm_fill(col = I("gray90")) +
  tm_borders()

parkmap <- tm_shape(parkdat) +
  tm_polygons(fill = "FEATTYPE", palette = "brewer.greens",title = "Park Type") +
  tm_borders()

streammap <- tm_shape(streamdat) +
  tm_lines(col = I("darkblue"), lwd = 6)
streammap
# Combine maps
finmap <- tm_shape(rtcounty) +
  tm_polygons(col = "gray90", fill_alpha = 0.2) +
  
  tm_shape(parkdat) +
  tm_polygons(fill = "FEATTYPE", palette = "brewer.greens",title = "Park Type") +
  
  tm_shape(streamdat) +
  tm_lines(col = "darkblue", lwd = 6)

finmap

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

# Build the map
elevation_map <- tm_shape(neoh_mask) +
  tm_raster(style = "cont",
            palette = terrain.colors(10),
            legend.show = TRUE) +
  
  tm_shape(rtcounty_raster_crs) +
  tm_fill(col = "gray", alpha = 0.3) +  # semi-transparent county fill
  tm_borders(col = "black", lwd = 2) +
  
  tm_layout(main.title = "Elevation Map: Portage & Summit Counties",
            main.title.size = 1.4,
            legend.outside = TRUE,
            frame = FALSE)

# Show the map
elevation_map

#2

elevation_map <- tm_shape(neoh_mask) +
  tm_raster(style = "cont",
            palette = terrain.colors(10),
            legend.show = TRUE) +
  
  tm_shape(rtcounty_raster_crs) +
  tm_fill(col = "gray", alpha = 0.3) +  # semi-transparent county fill
  tm_borders(col = "black", lwd = 2) +
  
  tm_compass(type = "arrow", position = c("right", "top"), size = 3) +  # 🧭 North arrow
  
  tm_layout(main.title = "Elevation Map: Portage & Summit Counties",
            main.title.size = 1.4,
            legend.outside = TRUE,
            frame = FALSE)
elevation_map

#3
library(grid)  # For viewport()

# === Add park and stream layers to elevation map ===
elevation_map <- elevation_map +
  tm_shape(parkdat) +
  tm_polygons(fill = "FEATTYPE", palette = "matplotlib.greens",title = "Park Type", fill_alpha = 0.2) +
  tm_borders() +
  
  tm_shape(streamdat) +
  tm_lines(col = "darkblue", lwd = 6)

# === Build the Ohio population map for inset ===
oh_map <- tm_shape(counties_joined) +
  tm_fill(col = "poptotal",
          palette = "brewer.Blues",
          style = "kmeans",
          title = "Total Population") +
  tm_borders(col = "black", lwd = 2, lty = "dashed") +
  tm_layout(frame = TRUE, bg.color = "white")

# === Plot main map ===
elevation_map

# === Add Ohio map as inset in top-left corner ===
print(oh_map, vp = grid::viewport(x = .9, y = 0.2, width = 0.3, height = 0.4))
