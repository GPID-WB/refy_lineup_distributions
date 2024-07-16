means_from_refy_dist <- function(path, svy = FALSE, country_code = NULL) {

  fl <- list.files(path)

  if (!is.null(country_code)) {
    country_code <- paste(country_code,
                          collapse = "|")
    fl <- fl[grepl(pattern = country_code, x = fl)]
  }

  dl <- lapply(cli::cli_progress_along(fl,
                                       format = "working on {fl[{cli::pb_current}]} | {cli::pb_bar} {cli::pb_percent}"),
               function(i, p = path) {
                 x <- fl[i]
                 d <- qs::qread(fs::path(p, x))

                 if (isFALSE(svy)) {
                   d <- d |>
                     fgroup_by(reporting_level, welfare_type) |>
                     fsummarise(mean_refy = fmean(x = welfare_refy,
                                                  w = weight_refy)) |>
                     fmutate(country_code = attributes(d)$country_code,
                             ref_year     = attributes(d)$reporting_year)
                 } else {
                   d <- d |>
                     fgroup_by(reporting_level, welfare_type, survey_year) |>
                     fsummarise(mean_refy = fmean(x = welfare_refy,
                                                  w = weight_refy)) |>
                     fmutate(country_code = attributes(d)$country_code,
                             ref_year     = attributes(d)$reporting_year)
                 }

                 # print(x)
                 d

               }) |>
    rbindlist()

  dl

}



