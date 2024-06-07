source("R/init.R")
source("R/refy.R")
source("R/refy_mult_factor.R")
source("R/refy_distributions.R")

# Create rm object
dt_ref <- refy(gls = gls,
               dsm = dsm,
               pinv = pinv,
               dl_aux = dl_aux) |>
  refy_mult_factor()

# get ref year distributions
ref2016_col <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "COL", # random country ;)
                                  ref_year   = 2016,
                                  gls        = gls)
ref2012_zaf <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "ZAF", # another very random country
                                  ref_year   = 2012,
                                  gls        = gls)

ref2012_nga <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "NGA", # imputation
                                  ref_year   = 2012,
                                  gls        = gls)
ref2012_zaf |> head()

# quick look at weights
# col
ref2016_col |>
  fgroup_by(survey_year) |>
  fsummarise(sum_weight      = fsum(weight),
             sum_weight_refy = fsum(weight_refy),
             sum__adj        = fsum(weight_refy_adj))
dl_aux$pop |>
  fsubset(year == 2016 & country_code == "COL")
# zaf
ref2012_zaf |>
  fgroup_by(survey_year) |>
  fsummarise(sum_weight      = fsum(weight),
             sum_weight_refy = fsum(weight_refy),
             sum__adj        = fsum(weight_refy_adj))
dl_aux$pop |>
  fsubset(year == 2012 & country_code == "ZAF")
# nga
ref2012_nga |>
  fgroup_by(survey_year) |>
  fsummarise(sum_weight      = fsum(weight),
             sum_weight_refy = fsum(weight_refy),
             sum__adj        = fsum(weight_refy_adj))
dl_aux$pop |>
  fsubset(year == 2012 & country_code == "NGA")

# quick look at welfare
# col
ref2016_col |>
  fgroup_by(survey_year) |>
  fsummarise(sum_welfare      = fsum(welfare_ppp),
             sum_welfare_refy = fsum(welfare_refy),
             mean_welfare_ref = fmean(welfare_refy, w = weight_refy))
dt_ref |>
  fsubset(reporting_year == 2016 & country_code == "COL", predicted_mean_ppp)
# zaf
ref2012_zaf |>
  fgroup_by(survey_year) |>
  fsummarise(sum_welfare      = fsum(welfare_ppp),
             sum_welfare_refy = fsum(welfare_refy, w = weight_refy),
             mean_welfare_ref = fmean(welfare_refy, w = weight_refy))
dt_ref |>
  fsubset(reporting_year == 2012 & country_code == "ZAF", predicted_mean_ppp)
# NGA
ref2012_nga |>
  fgroup_by(survey_year) |>
  fsummarise(sum_welfare      = fsum(welfare_ppp),
             sum_welfare_refy = fsum(welfare_refy),
             mean_welfare_ref = fmean(welfare_refy, w = weight_refy))
dt_ref |>
  fsubset(reporting_year == 2012 & country_code == "NGA", predicted_mean_ppp)
