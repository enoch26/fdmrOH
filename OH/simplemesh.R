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
ocean_sf2 <- st_union(st_buffer(ocean_sf, 1.05))
ggplot() + geom_sf(data=ocean_sf2)

fm_crs(ocean_sf) <- ocean_crs

ocean_sp <- as_Spatial(ocean_sf)
# ocean_sp <- sp::as(st_geometry(ocean_sf), "Spatial")

# let say we have random points to get a feeling of the data
ocean_pts <- st_sample(ocean_sf, size = 20*20,
                       type = "regular")

ocean_pts_sp <- as_Spatial(ocean_pts)

colnames(ocean_pts_sp@coords) <- c("LONG", "LAT")
  
fdmr::mesh_builder(spatial_data = as.data.frame(ocean_pts_sp),
                   max_edge = c(50,100),
                   offset = c(50,100),
                   # crs = fm_crs(ocean_pts_sp),
                   cutoff = 1)

ocean_bnd <- st_cast(
  st_sf(geometry = fm_nonconvex_hull(ocean_sf2, 5)),
  "POLYGON"
)

# ggplot() + geom_sf(data=ocean_sf2) + geom_sf(ocean_bnd)

boundary <- fm_as_segm_list(list(ocean_sf2, ocean_bnd))

mesh <- fm_mesh_2d_inla(boundary = boundary,
                        max.edge = c(5, 7),
                        offset = c(3, 3),
                        cutoff = 1)

ggplot() + geom_sf(data = ocean_sf2) + gg(mesh)

