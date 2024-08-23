# zaf2000 |> attributes()
# add_aux_data_attr
# pip_load_aux(measure = "ppp") |>
#   fsubset(country_code == country_code &
#             year == year)

aux_data <- function(cde,
                     yr,
                     reporting_level,
                     dl_aux,
                     df_refy,
                     all = TRUE,
                     filter_aux_data = FALSE,
                     py = 2017) {

  stopifnot(py %in% c(2011, 2017))
  output <- list()
#print("17")
  if (filter_aux_data) {
    vars     <- c("ppp_year", "release_version", "adaptation_version")
    ppp_v    <- unique(dl_aux$ppp[, ..vars], by = vars)
    data.table::setnames(x = ppp_v,
                         old = c("release_version", "adaptation_version"),
                         new = c("ppp_rv", "ppp_av"))

    # max release version
    m_rv <- ppp_v[ppp_year == py, max(ppp_rv)]

    # max adaptation year
    m_av <- ppp_v[ppp_year == py & ppp_rv == m_rv,
                  max(ppp_av)]


    ppp <- dl_aux$ppp[ppp_year == py
                             & release_version    == m_rv
                             & adaptation_version == m_av
    ][,
      ppp_default := TRUE] |>
      copy()
  } else {
    ppp <- dl_aux$ppp
  }
#print("42")
  # CPI
  reporting_level <- as.character(reporting_level)
  # cpi <-
  #   dl_aux$cpi |>
  #   fsubset(country_code == cde &
  #             cpi_year == yr,
  #           get(paste0("cpi", py))) |>
  #   as.numeric()
  #cpi <- fifelse(is.na(cpi), NA_real_, cpi)
  #output[[paste0("cpi", py)]] <- cpi


  cpi <-
    sapply(reporting_level,
           \(x){
             p <- dl_aux$cpi |>
               fsubset(country_code == cde &
                         cpi_year   == yr &
                         cpi_data_level %in% c(x),
                       get(paste0("cpi", py))) |>
               as.numeric()
           },
           simplify  = FALSE,
           USE.NAMES = TRUE)
  #cpi <- fifelse(is.na(cpi), NA_real_, cpi)
  output[[paste0("cpi", py)]] <- cpi

  #print("70")
  # PPP
  #reporting_level <- as.character(reporting_level)
  output[[paste0("ppp", py)]] <-
    sapply(reporting_level,
           \(x){
             p <- ppp |>
               fsubset(country_code == cde &
                         ppp_data_level %in% c(x),
                       ppp) |>
               as.numeric()
             },
           simplify  = FALSE,
           USE.NAMES = TRUE)
  #print("84")
  # GDP
  if (isTRUE(all)) {
    output[["gdp"]] <-
      dl_aux$gdp |>
      fsubset(country_code     == cde &
                year           == yr &
                gdp_data_level %in% reporting_level,
              gdp) |>
      funique() |>
      as.numeric()
    #print("95")

  # GDM - group data

  # CP

  # NPL

  # PCE
    output[["pce"]] <-
      dl_aux$pce |>
      fsubset(country_code == cde &
                year       == yr,
              pce) |>
      funique() |>
    as.numeric()

  # POP
    # output[["pop"]] <-
    #   dl_aux$pop |>
    #   fsubset(country_code == cde &
    #             year       == yr &
    #             pop_data_level %in% reporting_level,
    #           pop) |>
    #   as.numeric()

    output[["pop"]] <-
      sapply(reporting_level,
             \(x){
               p <- dl_aux$pop  |>
                 fsubset(country_code == cde &
                           year       == yr &
                           pop_data_level %in% c(x),
                         pop) |>
                 as.numeric()
               p
             },
             simplify  = FALSE,
             USE.NAMES = TRUE)
  }

  # return
  output

}


add_aux_data_attr <- function(df, dl_aux, df_refy, filter_aux_data) {

  code <- attr(x = df,
               which = "country_code")
  year <- attr(x = df,
               which = "reporting_year")
  reporting_level <- attr(x = df,
                          which = "reporting_level_rows")$reporting_level




  aux_data_list <- aux_data(cde             = code,
                            yr              = year,
                            reporting_level = reporting_level,
                            dl_aux          = dl_aux,
                            all             = TRUE,
                            filter_aux_data = filter_aux_data,
                            py              = 2017)

  aux_data_list <-
    c(aux_data_list,
      aux_data(cde             = code,
               yr              = year,
               reporting_level = reporting_level,
               dl_aux          = dl_aux,
               all             = FALSE,
               filter_aux_data = filter_aux_data,
               py              = 2011))

  attr(df,
       "aux_data") <- aux_data_list

  df
}

