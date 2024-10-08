#' Estimate and save the reference year distributions for all countries-ref-years
#'
#' A wrapper over [refy_distributions]  and [save_ref_dist]
#' applied on a list of data frames
#'
#' @param df_refy Output from [refy_distributions]
#' @param cntry_refy list: each element is another list containing
#'                   country_code scalar (e.g. "ZAF") and year vector
#'                   (e.g.`c(2001, 2002, 2003)`). If there are four countries,
#'                   the `length(cntry_refy) = 4`, one for each country with its
#'                   year vector.
#' @param path path to save the output - "P:\03.pip\lineup_distributions\output-lineup-ref-years"
#'
#' @return
#' @export
#'
#' @examples
#' full_refy_estimate_save(df_refy = dt_ref,
#'                         cntry_refy = list(list(country_code = "ZAF",
#'                                                year         = 2000:2005),
#'                                           list(country_code = "COL",
#'                                                year         = 2000:2005)),
#'                         path = output_dir_refy)
full_refy_estimate_save <- function(df_refy, cntry_refy, path, gls) {

  lapply(cntry_refy,
         FUN = \(x) {

           lapply(x$year,
                  FUN = \(year = x$year,
                          country_code = x$country_code){

                    refy_distributions(rm         = df_refy,
                                       cntry_code = country_code,
                                       ref_year   = year,
                                       gls        = gls) |>
                      save_ref_dist(path = path)}
                  )
         })
  invisible(TRUE)
}

