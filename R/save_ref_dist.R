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

  # vars as attributes
  df_refy <- df_refy |>
    vars_to_attr(var = c("country_code",
                         "reporting_level",
                         "welfare_type",
                        # "income_group_code",
                         "reporting_pop",
                         "distribution_type",
                         "gd_type",
                         "n_imp")) |>
    # per survey year
    num_vars_to_attr(num_var = c("nac_sy",
                             "relative_distance",
                             "predicted_mean_ppp",
                             "svy_mean",
                             "survey_id",
                             "cache_id",
                             "survey_acronym",
                             "surveyid_year",
                             "survey_time",
                             "survey_mean_lcu",
                             "survey_mean_ppp",
                             "cpi",
                             "mult_factor",
                             "svy_pop"),
                     name_var = rep("survey_year", 14))

  #if (is.null(cntry_code)) {
    cntry_code <- attributes(df_refy)$country_code
  #}
  #if (is.null(ref_year)) {
    ref_year <- attributes(df_refy)$reporting_year
  #}

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










