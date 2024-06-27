
refy_distributions <- function(rm, cntry_code, ref_year, gls) {

  # Filter rm
  rm <-
    rm |>
    fsubset(country_code == cntry_code &
              reporting_year == ref_year) |>
    fselect("country_code",
            "reporting_level",
            "welfare_type",
            "income_group_code",
            "survey_year",
            "reporting_year",
            #"nac",
            #"nac_sy",
            "relative_distance",
            #"estimation_type",
            #"lineup_case",
            #"interpolation_id",
            #"predicted_mean_ppp",
            #"reporting_gdp",
            #"reporting_pce",
            "reporting_pop",
            #"monotonic",
            #"same_direction",
            #"svy_mean",
            "survey_id",
            "cache_id",
            "wb_region_code",
            "pcn_region_code",
            "survey_acronym",
            #"survey_coverage",
            #"survey_comparability",
            #"comparable_spell",
            #"surveyid_year",
            #"survey_time",
            #"survey_mean_lcu",
            #"survey_mean_ppp",
            #"ppp",
            #"cpi",
            #"pop_data_level",
            #"gdp_data_level",
            #"pce_data_level",
            #"cpi_data_level",
            #"ppp_data_level",
            "distribution_type",
            #"gd_type",
            "is_interpolated",
            #"is_used_for_line_up",
            #"is_used_for_aggregation",
            #"display_cp",
            "lineup_approach",
            "mult_factor")


  # reduce rm
  rm <-
    rm |>
    vars_to_attr(vars = c(#"country_code",
                          #"reporting_level",
                          #"welfare_type",
                          "income_group_code",
                          #"survey_year",
                          "reporting_year",
                          "survey_id",
                          #"cache_id",
                          "wb_region_code",
                          "pcn_region_code",
                          "survey_acronym",
                          "distribution_type",
                          "is_interpolated",
                          "lineup_approach"#,
                          #"mult_factor"
                          ))

  # Load surveys
  cache_id <- rm$cache_id |>
    funique()
  gv(rm,
     "cache_id") <- NULL
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                      FUN = function(x){
                                        pipload::pip_load_cache(cache_id = x,
                                                                version = gls$vintage_dir) |>
                                          fselect(country_code, surveyid_year, survey_acronym,
                                                  survey_year, welfare_ppp, weight,
                                                  reporting_level, welfare_type, imputation_id)
                                      }))

  # Join welfare & weights vectors from surveys to rm
  df_refy <-
    rm |>
    joyn(y          = df_svy,
         by         = c("country_code",
                        "reporting_level",
                        "welfare_type",
                        "survey_year"),
         keep       = "left",
         match_type = "1:m",
         verbose    = FALSE,
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
            weight_refy = weight * (reporting_pop / svy_pop) * # "adjust to WDI population" --> Andres, your comment
              relative_distance#,
            # ref year weights divided by number of imputations
            #      this should sum to poplution amount
            #weight_refy_adj = weight_refy / n_imp
            ) |>
    fungroup() |>
    fmutate(welfare_refy = welfare_ppp * mult_factor)

  # temp
  setkey(df_refy, NULL)

  # keep attributes of rm
  attributes(df_refy) <- c(attributes(df_refy),
                           attributes(rm)[-which(names(attributes(rm)) %in%
                                                   c("dim",
                                                     "row.names",
                                                     "names",
                                                     "class",
                                                     ".internal.selfref",
                                                     names(attributes(df_refy))))])
  df_refy <- vars_to_attr(df_refy, "n_imp")
  gv(df_refy,
     c("svy_pop",
       "relative_distance",
       "reporting_pop",
       "surveyid_year",
       "mult_factor",
       "welfare_ppp",
       "weight")) <- NULL

  df_refy

}






