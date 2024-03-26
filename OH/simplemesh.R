library(sf)
library(ggplot2)
library(INLA)
library(inlabru)

fdmr::retrieve_tutorial_data(dataset = "indian_ocean")
fdmr::load_tutorial_data(dataset = "indian_ocean", 
                         filename = "indian_ocean.rds")

ocean_sf <-
  st_read("~/fdmr/tutorial_data/indian_ocean/indian_ocean.shp")
# ocean_sf <- ocean_sf[as.numeric(st_area(ocean_sf)) > 50000]
ocean_crs <- fm_crs(ocean_sf) 
fm_crs(ocean_sf) <- NULL
ocean_sf2 <- st_union(st_buffer(ocean_sf, 2))
fm_crs(ocean_sf) <- ocean_crs
ocean_sp <- as_Spatial(ocean_sf)
# ocean_sp <- sp::as(st_geometry(ocean_sf), "Spatial")

sp::coordinates(ocean_sp) <- c("LONG", "LAT")
fdmr::mesh_builder(spatial_data = ocean_sf,
                   x_coord = "LONG",
                   y_coord = "lat")

ocean_bnd <- st_cast(
  st_sf(geometry = fm_nonconvex_hull(ocean_sf2, 5)),
  "POLYGON"
)

ggplot() + geom_sf(data=ocean_sf2) + geom_sf(ocean_bnd)

boundary <- fm_as_segm_list(list(ocean_sf2, ocean_bnd))



mesh <- fm_mesh_2d_inla(boundary = boundary,
                        max.edge = c(2, 3),
                        offset = c(pi, pi),
                        cutoff = 1)

ggplot() + geom_sf(data = ocean_sf2) + gg(mesh)
ggplot() + geom_sf(data = oceans.sf)

