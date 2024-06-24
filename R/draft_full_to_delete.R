
full_list <-
  lapply(as.list(dt_ref$country_code |> funique()),
         FUN = \(x) {
           l        <- c(as.list(x),
                         list(dt_ref$reporting_year |>
                                funique()))
           names(l) <- c("country_code", "year")
           l
         })

as.list(as.list(dt_ref$country_code |>
  funique()))
dt_ref$reporting_year |>
  funique()


full_refy_estimate_save(df_refy    = dt_ref,
                        cntry_refy = list(list(country_code = "ZAF",
                                               year         = 2000:2005),
                                          list(country_code = "COL",
                                               year         = 2000:2005)),
                        path       = output_dir_refy)



