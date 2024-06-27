source("R/init.R")
source("R/refy.R")
source("R/refy_mult_factor.R")
source("R/refy_distributions.R")
source("R/save_ref_dist.R")
#source("R/full_refy_estimate_save.R")

# 1) CREATE `rm` OBJECT
#----------------------------------------
dt_ref <- refy(gls = gls,
               dsm = dsm,
               pinv = pinv,
               dl_aux = dl_aux) |>
  refy_mult_factor()

# 2) COMPARE REFY MEANS
#-----------------------------------------
# Estimate all refy means
full_means <-
  means_from_refy_dist(path         = output_dir_refy,
                       country_code = NULL) # to estimate all countries
# means from `rm` table
means_refy <- dt_ref |>
  fgroup_by(country_code, reporting_year, reporting_level, welfare_type) |>
  fmutate(w = reporting_pop/sum(reporting_pop),
          w = w*relative_distance) |>
  fsummarise(mean_dt_ref = fmean(x = predicted_mean_ppp,
                                 w = w))
# Compare refy distribution means to `rm` means
dt_mean_compare <-
  full_means |>
  joyn::joyn(y  = means_refy,
             by = c("country_code",
                    "ref_year = reporting_year",
                    "reporting_level",
                    "welfare_type"),
             keep = "left",
             match_type = "1:1")|>
  fmutate(diff = 100*(mean_refy - mean_dt_ref)/mean_dt_ref)
# These means could be slightly different
#   because in `rm`/dt_ref it comes from the
#   survey mean in `dsm`
dt_mean_compare$diff |> summary()

# 3) COMPARE NAC ADJUSTED MEANS AT SVY YEARS
#-----------------------------------------
# means by survey year
full_means_svy <-
  means_from_refy_dist(path = fs::path(output_dir, "lineups-with-survey_year"),
                       svy  = TRUE,
                       country_code = NULL)
dt_mean_compare_svy <-
  full_means_svy |>
  joyn::joyn(y = dt_ref |>
               fselect(country_code,
                       reporting_year,
                       reporting_level,
                       welfare_type,
                       predicted_mean_ppp,
                       survey_year),
             by = c("country_code",
                    "ref_year = reporting_year",
                    "reporting_level",
                    "welfare_type",
                    "survey_year"),
             keep = "left",
             match_type = "1:1") |>
  fmutate(diff = 100*(mean_refy - predicted_mean_ppp)/predicted_mean_ppp)

dt_mean_compare_svy$diff |>
  summary()
