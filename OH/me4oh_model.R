library(sp)
library(INLA)
library(inlabru)

# Load the final dataset
data_path <- ""
file_name <- "me4_dohc.2005.indian_ocean.rds"
df_f <- readRDS(base::file.path(data_path, file_name))
mesh <- readRDS(base::file.path(data_path, "mesh_5_7_1.rds"))

# Convert to sp
df_f$lat <- base::sapply(df_f$lat, as.numeric)
df_f$lon <- base::sapply(df_f$lon, as.numeric)
sp::coordinates(df_f) <- c('lon', 'lat')

# Priors
prior_range <- 25
prior_Pgt <- 0.2

# SPDE
ohc_spde <- INLA::inla.spde2.pcmatern(mesh, prior.range = c(prior_range, prior_Pgt),
                                      prior.sigma = c(1, 0.1))

# Time dependency prior
rhoprior <- base::list(theta = base::list(prior = 'pccor1', param = c(0, 0.9)))

# Group
n_time <- base::as.integer(base::length(base::unique(df_f@data$time)))

# Formula
model_formula <- dohc_L1 ~ -1 + f(
  main = coordinates,
  model = ohc_spde,
  group = time,
  ngroup = n_time,
  control.group = list(
    model = 'ar1',
    hyper = rhoprior
  )
)

# Run
bru_model <- inlabru::bru(model_formula,
                          data = df_f,
                          family = "gaussian",
                          options = list(
                            verbose = TRUE,
                            bru_verbose = 4
                          )
)


