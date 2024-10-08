source("R/init.R")
source("R/refy.R")
source("R/refy_mult_factor.R")
source("R/refy_distributions.R")
source("R/save_ref_dist.R")

# 1) CREATE `rm` OBJECT
#----------------------------------------
dt_ref <- refy(gls = gls,
               dsm = dsm,
               pinv = pinv,
               dl_aux = dl_aux) |>
  refy_mult_factor()

# 2) COMPARE POPULATION SIZES
#-----------------------------------------
# Get population sizes rm
dt_ref |>
  fsubset((survey_year %% 1) == 0)

# get refY_distribution populations
refy_pops <-
  refy_population_size(path         = output_dir_refy,
                       country_code = NULL) # to estimate all countries
