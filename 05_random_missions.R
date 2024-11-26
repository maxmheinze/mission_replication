
# Header ------------------------------------------------------------------

pacman::p_load(
  dplyr,
  magrittr,
  haven,
  fixest,
  BMS,
  BAS,
  sf,
  ggplot2
)

rm(list=ls())

data_path <- "./data/source/Tables"

shapefile_path <- "./data/shapefiles"



# Read in Data ------------------------------------------------------------

literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))

literacy_data <- literacy_data %>%
  mutate(mesorregi = as.factor(mesorregi))

literacy_data$distmiss






adm0 <- st_read(file.path(shapefile_path, "adm0.shp"))
adm1 <- st_read(file.path(shapefile_path, "adm1.shp"))
adm2 <- st_read(file.path(shapefile_path, "adm2.shp"))


fvc_extent <- st_union(adm2)
plot(fvc_extent)

# Define the South America Albers Equal Area Conic projection
sa_albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32.5 +lon_0=-60 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs"

# Transform your spatial object to the projected CRS
fvc_extent_proj <- st_transform(fvc_extent, crs = sa_albers)


random_points <- st_sample(fvc_extent_proj, type = "Thomas", scale=30, mu = 30, kappa = 5)


ggplot() + 
  geom_sf(aes(), data=fvc_extent_proj) + 
  geom_sf(aes(), data=random_points)
