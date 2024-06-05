
refy_distributions <- function(rm, cntry_code, ref_year, gls) {

  # Filter rm
  rm <-
    rm |>
    fsubset(country_code == cntry_code &
              reporting_year == ref_year)

  # Load surveys
  cache_id <- rm$cache_id
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                      FUN = function(x){
                                        pipload::pip_load_cache(cache_id = x,
                                                                version = gls$vintage_dir) |>
                                          fselect(country_code, surveyid_year, survey_acronym,
                                                  survey_year, welfare_ppp, weight,
                                                  reporting_level, welfare_type, imputation_id)
                                      }))

  # Join welfare & weights vectors from surveys to rm
  df_refy <- rm |>
    joyn(y          = df_svy,
         by         = c("country_code",
                        "reporting_level",
                        "welfare_type",
                        "survey_year"),
         keep       = "left",
         match_type = "1:m",
         verbose    = TRUE,
         reportvar  = FALSE) |>
    # Group by survey year
    fgroup_by(survey_year) |>
    # number of imputations per survey year (if micro data then n_imp = 1)
    fmutate(n_imp      = data.table::uniqueN(imputation_id),
            # population at survey (decimal) year found by summing survey weights
            svy_pop    = fsum(weight),
            # weights scaled by ratio of ref year and svy year pop
            #         to make relative to reference year
            # weights adj by rel dist to get weighted average of population at ref year
            frq_weight = weight * (reporting_pop / svy_pop) * # "adjust to WDI population" --> Andres, your comment
              relative_distance,
            # ref year weights divided by number of imputations
            #      this should sum to poplution amount
            weight_refy = frq_weight / n_imp) |>
    fungroup() |>
    fmutate(welfare = welfare * mult_factor)

  df_refy

}






