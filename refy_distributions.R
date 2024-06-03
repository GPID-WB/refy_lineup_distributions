refy_distributions <- function(rm, cntry_code, refy, gls) {
  # for now
  country_code <- "USA"
  refy <- 2017

  # survey years
  svys <- pipload::pip_find_cache(country_code,
                                  version = gls$vintage_dir)
  yrs  <- extract_year(svys)
  keep <- which(abs(yrs - refy) <= minN(x = abs(yrs - refy),
                                        N = 2))
  yrs  <- yrs[keep]

  # Interpolation vs extrapolation
  if ( all(yrs <= refy)) {
    yrs <- yrs[which.min(abs(yrs - refy))]
  }

  # Append (possibly two) surveys on other side of refy
  df_svy <- collapse::rowbind(lapply(as.list(yrs),
                                      FUN = function(x){
                                        pipload::pip_load_cache(cntry_code,
                                                                x,
                                                                version = gls$vintage_dir) |>
                                          fselect(country_code, surveyid_year, survey_acronym,
                                                  survey_year, welfare, weight, reporting_level, welfare_type)
                                      }))
  # adjust weights - there are two separate surveys now with incomparable weights
  df_svy <- df_svy |>
    fgroup_by(survey_year) |>
    fmutate(weight = weight / fsum(weight,
                                   g = survey_year))

  df_refy <- rm |>
    fsubset(country_code == cntry_code) |>
    joyn::joyn(y          = df_svy,
               by         = c("country_code", "reporting_level", "welfare_type", "survey_year"),
               keep       = "left",
               match_type = "m:m",
               verbose = T) |>
    fmutate(welfare_lineup = welfare * mult_factor)

  df_refy

}
