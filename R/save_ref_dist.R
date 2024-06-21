#' Function to minimize and save reference year distribution for one
#' country and one reference year
#'
#' @param df_refy output from [refy_distributions]
#' @param path path to save the output - "P:\03.pip\lineup_distributions\output-lineup-ref-years"
#'
#' @return invisible logical - purpose is to save file
#' @keywords internal
save_ref_dist <- function(df_refy,
                          path) {

  #is_interpolated <- attributes(df_refy)$is_interpolated
  length_uniq_var <- length(attributes(df_refy)$interpolation_id)
  n_imp           <- attributes(df_refy)$n_imp
  # print(list(l = length_uniq_var,
  #            n_imp = n_imp))
  if (length(n_imp) > 1) {
    any_imp <- any(df_refy$distribution_type == "imputed")
    n_imp   <- 2 # multiple imputation (e.g. NGA) doesn't have unique numvars
  }

  #print(attributes(df_refy))
  #print(n_imp)
  if (length_uniq_var == 1 &
      n_imp == 1
      #(!is_interpolated & length_uniq_var == 1)
      ) {

    print("one")
    # some specific countries (e.g. CHN) produce
    #  "Error in `vars_to_list()`:
    #    ! The unique values in `num_var` and `name_var` column
    #       are not equal"
    # For CHN: because rural and urban have different means (svy_mean)
    #   etc. for the same survey year
    numvars <- c("nac_sy",
                 "svy_mean",
                 "survey_id",
                 "cache_id",
                 "surveyid_year",
                 #"survey_time", # SYR 2007 doesn't have `survey_time`
                 "survey_mean_lcu",
                 "survey_mean_ppp",
                 "cpi",
                 "mult_factor",
                 "svy_pop")

    # vars as attributes
    df_refy <- df_refy |>
      vars_to_attr(var = c("country_code",
                           "reporting_level",
                           "welfare_type",
                           "reporting_pop",
                           "distribution_type",
                           "gd_type",
                           "relative_distance"#,
                           #"n_imp"
                           )) |>
      # per survey year
      num_vars_to_attr(num_var = numvars,
                       name_var = rep("survey_year",
                                      length(numvars)))

  } else {
    print("two")
    # if error above, then reduce number of `numvars`
    #  that can be made attributes
    numvars <- c("nac_sy",
                 #"svy_mean",
                 "survey_id",
                 "cache_id",
                 "surveyid_year",
                 "survey_time",
                 #"survey_mean_lcu",
                 #"survey_mean_ppp",
                 #"cpi",
                 #"mult_factor",
                 "svy_pop")

    # vars as attributes
    df_refy <- df_refy |>
      vars_to_attr(var = c("country_code",
                           "reporting_level",
                           "welfare_type",
                           "reporting_pop",
                           "distribution_type",
                           "gd_type",
                           "relative_distance"
                           #"n_imp"
                           )) |>
      # per survey year
      num_vars_to_attr(num_var  = numvars,
                       name_var = rep("survey_year",
                                      length(numvars)))

  }

  cntry_code <- attributes(df_refy)$country_code
  ref_year   <- attributes(df_refy)$reporting_year

  # save
  qs::qsave(x = df_refy,
            file = fs::path(path, paste0(cntry_code,
                                         "_",
                                         ref_year,
                                         ".qs")))

  invisible(TRUE)
}




#' Save and minimize all reference year distributions individually in a list
#'
#' A wrapper over [save_ref_dist] applied on a list of data frames
#'
#' @param list_refy list of data frames output from [refy_distributions]
#' @param path path to save the output - "P:\03.pip\lineup_distributions\output-lineup-ref-years"
#'
#' @return invisible logical - purpose is to save files
#' @keywords internal
apply_save_ref_dist <- function(list_refy,
                                path) {

  lapply(list_refy,
         FUN = function(x) {
           save_ref_dist(df_refy = x,
                         path    = path)
         })

  invisible(TRUE)
}










