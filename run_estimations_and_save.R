source("R/init.R")
source("R/refy.R")
source("R/refy_mult_factor.R")
source("R/refy_distributions.R")
source("R/save_ref_dist.R")
source("R/full_refy_estimate_save.R")

# 1) Create rm object
dt_ref <- refy(gls = gls,
               dsm = dsm,
               pinv = pinv,
               dl_aux = dl_aux) |>
  refy_mult_factor()

# 2) create full input list
full_list <-
  lapply(as.list(dt_ref$country_code |> funique()),
         FUN = \(x) {
           l        <- c(as.list(x),
                         list(dt_ref$reporting_year |>
                                funique()))
           names(l) <- c("country_code", "year")
           l
         })





# 3) Test with specific, small choices:
full_refy_estimate_save(df_refy    = dt_ref,
                        cntry_refy = list(list(country_code = "CHN",
                                               year         = 2012)),#,
                                          #list(country_code = "COL",
                                               #year         = 2000:2005)),
                        path       = output_dir_refy,
                        gls = gls)

# 4) Run entire saving process
t1 <- Sys.time()
full_refy_estimate_save(df_refy    = dt_ref,
                        cntry_refy = full_list,
                        path       = output_dir_refy,
                        gls = gls)
t2 <- Sys.time()
print(t2 - t1)


# Optional below


# 5) list from specific country: NGA
full_list_from_fsm <-
  lapply(as.list(funique(dt_ref$country_code)[
    which(funique(dt_ref$country_code) == "FSM"):
      length(funique(dt_ref$country_code))]),
    FUN = \(x) {
      l        <- c(as.list(x),
                    list(dt_ref$reporting_year |>
                           funique()))
      names(l) <- c("country_code", "year")
      l
    })
# 6) save from chosen country
t1 <- Sys.time()
full_refy_estimate_save(df_refy    = dt_ref,
                        cntry_refy = full_list_from_nga,
                        path       = output_dir_refy,
                        gls = gls)
t2 <- Sys.time()
print(t2 - t1)


# with survey years in data frame (i.e. not attribute)
t1 <- Sys.time()
full_refy_estimate_save(df_refy    = dt_ref,
                        cntry_refy = full_list,
                        path       = fs::path(output_dir, "lineups-with-survey_year"),
                        gls = gls,
                        inc_svy_year = TRUE)
t2 <- Sys.time()
print(t2 - t1)

