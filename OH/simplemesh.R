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

ocean_bnd <- 

mesh <- fm_mesh_2d(boundary = ocean_sf2, max_edge)

ggplot() + geom_sf(data=oceans.sf)

ggplot() + geom_sf(data=ocean_sf2) + gg(mesh)
