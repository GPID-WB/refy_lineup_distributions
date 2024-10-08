# run specific countries

#devtools::install_github(repo = "PIP-Technical-Team/pipload@lineups")

library(pipload)
# Source functions
source("R/init.R")
source("R/refy.R")
source("R/refy_mult_factor.R")
library(joyn)

# create refy table
df_refy <- refy(gls = gls,
                dsm = dsm,
                pinv = pinv,
                dl_aux = dl_aux) |>
  refy_mult_factor()

# Not now, but eventually yes (unless you have them ready, then it will be great
#   to place them in the P drive somewhere). But, we could start sending them key cases,
#   pure micro data, imputed data, sync data (with and without data-level breakdown,
#   like CHN and ARE, respectively).
dir <- "P:/03.pip/lineup_distributions/test-files"

input_list <- list(list(country_code = "CHN",
                        year         = 1981:2022),
                   list(country_code = "ARE",
                        year         = 1981:2022),
                   list(country_code = "IND",
                        year         = 1981:2022),
                   list(country_code = "COL",
                        year         = 1981:2022))

pipload:::write_multiple_refy_dist(df_refy     = df_refy,
                         cntry_refy  = input_list,
                         path        = fs::path(dir, "qs"),
                         gls         = gls,
                         dl_aux      = dl_aux)

# convert to dta


# load, write as dta
save_as_dta <- function(load_path, write_path) {
  fl <- list.files(path = load_path) |>
    as.list()

  lapply(fl,
         FUN = \(x) {
           nm <- sub("\\.qs$", "", x)
           cn <- sub("^(.{3}).*", "\\1", nm)
           yr <- sub(".*(.{4})$", "\\1", nm)

           # load
           dt <- pipload:::load_refy(country_code = cn,
                           year         = yr,
                           path         = load_path)
           print(nm)
           haven::write_dta(data = dt,
                            path = fs::path(write_path,
                                            nm,
                                            ext = "dta"))
           invisible(TRUE)

         })
  return(invisible(TRUE))
}

save_as_dta(load_path  = fs::path(dir, "qs"),
            write_path = fs::path(dir, "dta"))



save_as_dta_per_country <- function(load_path, write_path) {

  fl  <- list.files(path = load_path)
  cns <- sub("^(.{3}).*", "\\1", fl) |>
    funique() |>
    as.list()

  # fl  <- fl |>
  #   as.list()

  lapply(cns,
         FUN = \(x) {
           nm <- sub("\\.qs$", "", x)
           cn <- sub("^(.{3}).*", "\\1", nm)
           yr <- sub(".*(.{4})$", "\\1", nm)
           input_list <- list(country_code = cn,
                              year         = list(1981:2022))

           # load
           dt <- pipload:::load_list_refy(input_list,
                                          path = load_path) |>
             pipload:::append_refy_dt(add_columns = c("reporting_level",
                                                      "welfare_type"))
           print(cn)
           haven::write_dta(data = dt,
                            path = fs::path(write_path,
                                            cn,
                                            ext = "dta"))
           invisible(TRUE)

         })

  return(invisible(TRUE))

}
save_as_dta_per_country(load_path = fs::path(dir, "qs"),
                        write_path = fs::path(dir, "dta-per-country"))

