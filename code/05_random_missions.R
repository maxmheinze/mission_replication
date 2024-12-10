
# Header ------------------------------------------------------------------

pacman::p_load(
  dplyr,
  magrittr,
  haven,
  fixest,
  BMS,
  BAS,
  sf,
  ggplot2,
  spatstat,
  spatstat.geom
)

rm(list=ls())

data_path <- "./data/source/Tables"

shapefile_path <- "./data/shapefiles"



# Read in Data ------------------------------------------------------------

literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"), encoding = "ISO-8859-1")

literacy_data <- literacy_data %>%
  mutate(mesorregi = as.factor(mesorregi))

adm0 <- st_read(file.path(shapefile_path, "adm0.shp"))
adm1 <- st_read(file.path(shapefile_path, "adm1.shp"))
adm2 <- st_read(file.path(shapefile_path, "adm2.shp"))


# Reproject ---------------------------------------------------------------

fvc_extent <- st_union(adm2)

# South America Albers Equal Area Conic projection
sa_albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32.5 +lon_0=-60 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs"
fvc_extent_proj <- st_transform(fvc_extent, crs = sa_albers)

extent_window <- as.owin(fvc_extent_proj)


# Parameters --------------------------------------------------------------

polygon_area <- as.numeric(st_area(fvc_extent_proj))

kappa <- 1 / polygon_area  # Intensity to expect one cluster center
mu <- 30  # Mean number of points per cluster
sigma <- 75000  # Adjust as needed for desired cluster looseness


# Simulate ----------------------------------------------------------------

set.seed(1000)

# Simulate the point pattern
pp <- rThomas(kappa = kappa, sigma = sigma, mu = mu, win = extent_window)

num_clusters <- length(pp$parent$x)
num_points <- pp$n

cat("Number of clusters:", num_clusters, "\n")
cat("Number of points:", num_points, "\n")


# Extract point coordinates
points_coords <- data.frame(x = pp$x, y = pp$y)

# Convert to sf object
points_sf <- st_as_sf(points_coords, coords = c("x", "y"), crs = st_crs(fvc_extent_proj))


ggplot() +
  geom_sf(data = fvc_extent_proj, fill = NA, color = 'black') +
  geom_sf(data = points_sf, color = 'red', size = 2) +
  theme_minimal() +
  labs(title = "Randomly Placed Jesuit Mission Cluster",
       subtitle = paste0("Thomas Process with κ = 1/", round(polygon_area), ", σ = ", sigma, ", µ = ", mu))


# Calculate distance, add to dataset --------------------------------------

# Transform points_sf to the CRS of adm2
points_sf_transformed <- st_transform(points_sf, crs = st_crs(adm2))

# Calculate centroids (may lie outside the polygon for irregular shapes)
adm2_centroids <- st_centroid(adm2)

# Find the index of the nearest point in points_sf_transformed for each centroid
nearest_indices <- st_nearest_feature(adm2_centroids, points_sf_transformed)

# Calculate the distance to the nearest point
nearest_distances <- st_distance(adm2_centroids, points_sf_transformed[nearest_indices, ], by_element = TRUE)

# Ensure distances are numeric
adm2$distsim <- as.numeric(nearest_distances)

plot(adm2[15])

literacy_data_tidied <- literacy_data %>%
  mutate(country1 = ifelse(country == "BRA", "Brazil", country)) %>%
  mutate(state1 = ifelse(state == "RS", "Rio Grande do Sul", state))

literacy_data_inclsim <- left_join(literacy_data_tidied, st_drop_geometry(dplyr::select(adm2, NAME_2, NAME_1, COUNTRY, distsim)), 
                                   by = join_by(muni == NAME_2, state1 == NAME_1, country1 == COUNTRY)) %>%
  mutate(distsim = distsim/1000)


# Regression --------------------------------------------------------------

model_full_1 <- feols(illiteracy ~ distmiss + lati + longi + corr + ita + mis + mis1, data = literacy_data_inclsim)
model_full_2 <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis + mis1, data = literacy_data_inclsim)
model_full_1s <- feols(illiteracy ~ distsim + lati + longi + corr + ita + mis + mis1, data = literacy_data_inclsim)
model_full_2s <- feols(illiteracy ~ distsim + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis + mis1, data = literacy_data_inclsim)

etable(model_full_1, model_full_2, model_full_1s, model_full_2s, se = "hetero")


