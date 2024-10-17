# This script is tocompare all the means and ginis for lineups to those in the



get_refy_stats <- function(df) {

  # headcounts
  hc215 <-
    wbpip::md_compute_headcount(welfare = df$welfare_refy,
                                weight  = df$weight_refy,
                                povline = 2.15)
  hc365 <-
    wbpip::md_compute_headcount(welfare = df$welfare_refy,
                                weight  = df$weight_refy,
                                povline = 3.65)
  hc685 <-
    wbpip::md_compute_headcount(welfare = df$welfare_refy,
                                weight  = df$weight_refy,
                                povline = 6.85)
  hc10 <-
    wbpip::md_compute_headcount(welfare = df$welfare_refy,
                                weight  = df$weight_refy,
                                povline = 10)
  hc50 <-
    wbpip::md_compute_headcount(welfare = df$welfare_refy,
                                weight  = df$weight_refy,
                                povline = 50)

  # gini
  g <- wbpip::md_compute_gini(welfare = df$welfare_refy,
                              weight  = df$weight_refy)

  # mean
  m <- pipload:::extract_attr(df         = df,
                              dist_stats = TRUE,
                              aux_data   = FALSE,
                              attr       = "mean")
  rl <- names(m)
  m <- unlist(m) |>
    unname()

  # percentiles
  p1 <- wbpip::md_compute_lorenz(welfare = df$welfare_refy,
                                  weight = df$weight_refy,
                                  nbins = 100) |>
    fsubset(round(lorenz_weight, 2) == 0.01,
            welfare) |>
    as.numeric()
  p99 <-
    wbpip::md_compute_lorenz(welfare = df$welfare_refy,
                              weight = df$weight_refy,
                              nbins = 100) |>
    fsubset(round(lorenz_weight, 2) == 0.99,
            welfare) |>
    as.numeric()

  # results as list
  rl <-
    data.table::data.table(country_code = pipload:::extract_attr(df,
                                                     attr = "country_code"),
               year = pipload:::extract_attr(df,
                                             attr = "reporting_year"),
               estimate = "refy_lineups",
               reporting_level = rl,
               hc215 = hc215,
               hc365 = hc365,
               hc685 = hc685,
               hc10  = hc10,
               hc50  = hc50,
               gini  = g,
               mean  = m,
               p1    = p1,
               p99   = p99)

  rl

}

get_pipr_stats <- function(code, yr, full_pipr){

  # headcounts
  hc215 <-
    full_pipr |>
    fsubset(pipr_type    == "povline_215") |>
    fsubset(country_code == code) |>
    fsubset(year         == yr)

  g     <- hc215$gini
  m     <- hc215$mean
  rl    <- hc215$reporting_level
  hc215 <- hc215$headcount


  hc365 <-
    full_pipr |>
    fsubset(pipr_type   == "povline_365" &
            country_code == code &
            year         == yr)

  hc365 <- hc365$headcount
  hc685 <-    full_pipr |>
    fsubset(pipr_type   == "povline_685" &
            country_code == code &
            year         == yr)

  hc685 <- hc685$headcount
  hc10 <-
    full_pipr |>
    fsubset(pipr_type   == "povline_10" &
            country_code == code &
            year         == yr)

  hc10 <- hc10$headcount
  hc50 <-
    full_pipr |>
    fsubset(pipr_type   == "povline_50" &
            country_code == code &
            year         == yr)

  hc50 <- hc50$headcount

  # mean

  # percentiles
  p1 <-
    full_pipr |>
    fsubset(pipr_type   == "popshare_01" &
            country_code == code &
            year         == yr)
  p1 <- p1$poverty_line

  p99 <-
    full_pipr |>
    fsubset(pipr_type   == "popshare_99" &
            country_code == code &
            year         == yr)
  p99 <- p99$poverty_line

  # results as list
  rl <-
    data.table::data.table(
               country_code = code,
               year         = yr,
               estimate     = "pipr",
               reporting_level = rl,
               hc215        = hc215,
               hc365        = hc365,
               hc685        = hc685,
               hc10         = hc10,
               hc50         = hc50,
               gini         = g,
               mean         = m,
               p1           = p1,
               p99          = p99)
  rl

}


get_refy_comparison_df <- function(compare_list,
                                   path,
                                   #dsm,
                                   full_pipr_df) {

  compare_list <- pipload::transform_input(compare_list)

  lapply(X   = compare_list,
         FUN = \(x) {

           df <- load_refy(country_code = x$country_code,
                           year         = x$year,
                           path         = path)

           rowbind(get_refy_stats(df = df),
                   get_pipr_stats(code = x$country_code,
                                  yr = x$year,
                                  full_pipr = full_pipr_df))

         }) |>
    rowbind()



}


