# Subsetting and preparing the dataset to use with INLA/inlabru

## Load the dataset
data_path <- ''
df <- readRDS(data_path, "me4_dohc.2005.rds")

## Define the ocean mask
ocean_sf <-
  st_read("C:/Users/aabel/fdmr/tutorial_data/indian_ocean/indian_ocean.shp")
ocean_crs <- fm_crs(ocean_sf) 
fm_crs(ocean_sf) <- NULL
ocean_sf3 <- st_union(st_buffer(ocean_sf, 0.5))

ggplot() + geom_sf(data=ocean_sf) + geom_sf(data = ocean_sf3)

## Convert the dataset to an sf object
loc <- sf::st_as_sf(df, coords = c("lon", "lat"))

## Remove the data outside the region of interest
in_list <- base::unlist(sf::st_intersects(ocean_sf3, loc, sparse = TRUE))
df <- df[in_list,]

## Reindex (change the numbering of profiles)
df$number_of_profile <- base::seq(1, base::length(df$number_of_profile))

## Set the dataset as a sf object
df_sf <- sf::st_as_sf(df, coords = c('lon', 'lat'))

saveRDS(df, file.path(out_dir, "me4_dohc.2005.indian_ocean.rds"))