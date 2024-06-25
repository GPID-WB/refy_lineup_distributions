means_from_refy_dist <- function(path, country_code = NULL) {

  fl <- list.files(path)

  if (!is.null(country_code)) {
    country_code <- paste(country_code,
                          collapse = "|")
    fl <- fl[grepl(pattern = country_code, x = fl)]
  }

  dl <- lapply(fl,
               function(x, p = path) {
                 d <- qs::qread(fs::path(p, x))

                 d <- d |>
                   fsummarise(mean_refy = fmean(x = welfare_refy,
                                                w = weight_refy),
                              predicted_mean_ppp = funique(predicted_mean_ppp)) |>
                   fmutate(country_code = attributes(d)$country_code,
                           ref_year     = attributes(d)$reporting_year)

               }) |>
    rbindlist()

  dl

}


fl <- means_from_refy_dist(path = output_dir_refy,
                           country_code = NULL)
z <- fl |>
  joyn::joyn(y = pipr::get_stats(fill_gaps = TRUE) |>
               fselect(country_code, year, mean),
             by = c("ref_year = year"),
             match_type = "m:1")
z |>
  fmutate(diff = 100*(mean_refy - mean)/mean)
