library(sf)
library(ggplot2)
library(INLA)
library(inlabru)

fdmr::retrieve_tutorial_data(dataset = "indian_ocean")
fdmr::load_tutorial_data(dataset = "indian_ocean", 
                         filename = "indian_ocean.rds")

ocean_sf <-
  st_read("~/fdmr/tutorial_data/indian_ocean/indian_ocean.shp")
ocean_crs <- fm_crs(ocean_sf) 
fm_crs(ocean_sf) <- NULL
ocean_sf2 <- st_union(st_buffer(ocean_sf, 1))



ggplot() + geom_sf(data=ocean_sf2)

mesh <- fm_mesh_2d(boundary = oceans.sf)


ante_meridian <- -20 ## change as needed
offset <- ante_meridian+180
jitter <- 0
lat <- c(seq(from=-90,to=90,by=0.25))
lon <- c(rep(offset-jitter,length(lat)),rep(offset+jitter,length(lat)))

oceans.sf <- cbind(lon, c(lat, rev(lat))) %>% st_linestring() %>% st_cast("POLYGON") %>% 
  st_sfc(crs=4326)


ggplot() + geom_sf(data=oceans.sf)


ggplot() + gg(data=oceans.sf) + gg(mesh)
