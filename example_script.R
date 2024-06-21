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

ref2000_zaf <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "ZAF", # another very random country
                                  ref_year   = 2000,
                                  gls        = gls)

ref2001_zaf <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "ZAF", # another very random country
                                  ref_year   = 2001,
                                  gls        = gls)

ref2012_nga <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "NGA", # imputation
                                  ref_year   = 2012,
                                  gls        = gls)

ref2011_nga <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "NGA", # imputation
                                  ref_year   = 2011,
                                  gls        = gls)
ref2010_nga <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "NGA", # imputation
                                  ref_year   = 2010,
                                  gls        = gls)
ref2016_nga <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "NGA", # imputation
                                  ref_year   = 2016,
                                  gls        = gls)
ref1999_alb <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "ALB", # imputation
                                  ref_year   = 1999,
                                  gls        = gls)
ref2007_syr <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "SYR", # imputation
                                  ref_year   = 2007,
                                  gls        = gls)
ref2012_zaf |> head()

ref1981_chn <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "CHN", # imputation
                                  ref_year   = 1981,
                                  gls        = gls)

ref1982_chn <- refy_distributions(rm         = dt_ref,
                                  cntry_code = "CHN", # imputation
                                  ref_year   = 1982,
                                  gls        = gls)


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




# save ref dist
save_ref_dist(df_refy = ref2001_zaf,
              path    = output_dir_refy)


# Full refy estimate and save
full_refy_estimate_save(df_refy = dt_ref,
                        cntry_refy = list(list(country_code = "ZAF",
                                               year         = 2000:2005),
                                          list(country_code = "COL",
                                               year         = 2000:2005)),
                        path = output_dir_refy)


list(list(country_code = "ZAF",
          year         = 1980:2023),
     list(country_code = "COL",
          year         = 1980:2023))

full_refy_estimate_save(df_refy = dt_ref,
                        cntry_refy = list(list(country_code = "COL",
                                               year         = 2001:2005)),
                        path = output_dir_refy)

full_refy_estimate_save(df_refy = dt_ref,
                        cntry_refy = list(list(country_code = "NGA",
                                               year         = 2010:2015)),
                        path = output_dir_refy,
                        gls = gls)



