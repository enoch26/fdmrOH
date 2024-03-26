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
ocean_bnd <- st_cast(
  st_sf(geometry = fm_nonconvex_hull(ocean_sf2, 1)),
  "POLYGON"
)

ggplot() + geom_sf(data=ocean_sf2) + geom_sf(ocean_bnd)

plot(ocean_sf2)
plot(ocean_bnd)  
mesh <- fm_mesh_2d_inla(boundary = ocean_sf2,
                        max.edge = 2,
                        offset = c(pi, pi))

ggplot() + geom_sf(data=ocean_sf2) + gg(mesh)
ggplot() + geom_sf(data=oceans.sf)

