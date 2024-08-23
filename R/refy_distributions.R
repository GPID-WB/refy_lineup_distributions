
refy_distributions <- function(rm, cntry_code, ref_year, gls) {

  # ensure no factors
  lapply(rm,
         FUN = function(x) {
           if (is.factor(x)) {
             as.character(x)
           } else {
             x
           }
         }) |>
    qDT()
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
  survey_year_rows <- list()
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                      FUN = function(x){
                                        pipload::pip_load_cache(cache_id = x,
                                                                version  = gls$vintage_dir) |>
                                          fselect(country_code, surveyid_year, survey_acronym,
                                                  survey_year, welfare_ppp, weight,
                                                  reporting_level, welfare_type, imputation_id)

                                      }))

  # Make survey year rows an attribute
  survey_years_rows <- df_svy |>
    fselect(survey_year) |>
    fmutate(rows = 1:fnrow(df_svy)) |>
    fgroup_by(survey_year) |>
    fsummarise(rows = fmax(rows))

  survey_years_rows <-
    list(survey_years = survey_years_rows$survey_year,
         rows         = survey_years_rows$rows)

  # Make reporting level rows an attribute
  reporting_level_rows <- df_svy |>
    fselect(reporting_level) |>
    fmutate(rows = 1:fnrow(df_svy)) |>
    fgroup_by(reporting_level) |>
    fsummarise(rows = fmax(rows))

  reporting_level_rows <-
    list(reporting_level = as.character(reporting_level_rows$reporting_level),
         rows            = reporting_level_rows$rows)

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
            #      this should sum to population amount
            #weight_refy_adj = weight_refy / n_imp
            ) |>
    fungroup() |>
    fmutate(welfare_refy = welfare_ppp * mult_factor)

  # temp
  setkey(df_refy, NULL)

  # Make welfare type an attribute
  df_refy <-
    df_refy |>
    vars_to_attr(vars = c("welfare_type"))

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
  attr(df_refy,
       "survey_years_rows")    <- survey_years_rows

  df_refy <- df_refy |>
    vars_to_attr(var = c("country_code",
                         "survey_acronym",
                         "survey_year"))

  dist_stats <- dist_stats(df = df_refy)
  attr(df_refy,
       "dist_stats") <- dist_stats

  attr(df_refy,
       "reporting_level_rows") <- reporting_level_rows

  gv(df_refy,
     c("svy_pop",
       "relative_distance",
       "reporting_pop",
       "surveyid_year",
       "mult_factor",
       "welfare_ppp",
       "weight",
       "reporting_level")) <- NULL

  df_refy

}




dist_stats <- function(df) {

  # min
  min <- fmin(df$welfare_refy,
              g = df$reporting_level) |>
    as.list()

  # max
  max <- fmax(df$welfare_refy,
              g = df$reporting_level) |>
    as.list()

  # mean
  mean <- fmean(x = df$welfare_refy,
                w = df$weight_refy,
                g = df$reporting_level) |>
    as.list()

  # median
  median <- fmedian(x = df$welfare_refy,
                    w = df$weight_refy,
                    g = df$reporting_level) |>
    as.list()

  # gini
  gini <- sapply(df$reporting_level |> funique(),
                 FUN = \(x) {
                   wbpip::md_compute_gini(welfare = df$welfare_refy[df$reporting_level == x],
                                          weight  = df$weight_refy[df$reporting_level == x])
                 }) |>
    as.list()

  # mld
  mld <- sapply(df$reporting_level |> funique(),
                FUN = \(x) {
                  wbpip::md_compute_mld(welfare = df$welfare_refy[df$reporting_level == x],
                                        weight  = df$weight_refy[df$reporting_level == x],
                                        mean    = mean$x)
                }) |>
    as.list()

  # polarization
  pol <- sapply(df$reporting_level |> funique(),
                FUN = \(x) {
                  wbpip::md_compute_polarization(welfare = df$welfare_refy[df$reporting_level == x],
                                                 weight  = df$weight_refy[df$reporting_level == x],
                                                 gini    = gini[[x]],
                                                 mean    = mean[[x]],
                                                 median  = median[[x]])
                }) |>
    as.list()

  # results
  dist_stats <- list(min          = min,
                     max          = max,
                     mean         = mean,
                     median       = median,
                     gini         = gini,
                     mld          = mld,
                     polarization = pol)

  dist_stats

}



