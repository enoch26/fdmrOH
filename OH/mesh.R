library(sf)
library(INLA)
library(inlabru)
library(ggplot2)

globe(
  R = 1,
  R.grid = 1.05,
  specular = "black",
  axes = FALSE,
  box = FALSE,
  xlab = "",
  ylab = "",
  zlab = ""
)

mesh <- fm_rcdt_2d_inla(globe = 2, crs = fm_crs("sphere"))
# glplot(mesh)
plot_rgl(mesh)
