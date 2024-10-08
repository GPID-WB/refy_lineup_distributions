refy_population_size <- function(path, country_code = NULL) {

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
                   fsummarise(pop = fsum(weight_refy)) |>
                   fmutate(country_code = attributes(d)$country_code,
                           ref_year     = attributes(d)$reporting_year)

                 d

               }) |>
    rbindlist()

  dl

}
