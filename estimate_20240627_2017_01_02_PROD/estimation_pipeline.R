# install newest version of the pipload
# devtools::install_github(repo = "PIP-Technical-Team/pipload@lineups")
source("./R/init.R")
source("./R/get_refy_comparison_df.R")
devtools::install_github(repo = "PIP-Technical-Team/pipfun@DEV")
devtools::install_github(repo = "PIP-Technical-Team/pipload@lineups")
library(data.table)
library(collapse)
library(joyn)
library(pipload)
# Step 1: get objects----
## 1.1 - gls ----
# gls2 <- pipfun::pip_create_globals(vintage = "20240627_2017_01_02_PROD")
gls2 <- qs::qread(file = fs::path("data_inputs",
                                  "gls3",
                                  ext = "qs"))
## 1.2 - dsm ----
dsm2 <- fst::read_fst(path = fs::path("//wbgmsddg001/PIP/pipapi_data/20240627_2017_01_02_PROD/estimations",
                                      "survey_means.fst"))

## 1.3 - dl_aux ----
# note this prep_aux_data function is from:
# https://github.com/PIP-Technical-Team/pip_ingestion_pipeline/blob/aaa4a161eb42bfa26294d27e334ac46004c4b009/R/utils.R#L538
prep_aux_data <- function(maindir = PIP_DATA_DIR) {

  auxdir <- fs::path(maindir, "_aux/")

  aux_dirs <- fs::dir_ls(auxdir,
                         recurse = FALSE,
                         type = "directory")

  aux_indicators <- stringr::str_extract(aux_dirs, "[^/]+$")
  aux_indicators   <-  tolower(unique(aux_indicators))

  # keep only those that exist
  dd <-
    purrr::map2_lgl(.x = aux_dirs,
                    .y = aux_indicators,
                    .f = ~{
                      ffst <- fs::path(.x, .y, ext = "fst")
                      frds <- fs::path(.x, .y, ext = "rds")

                      f_exists <- purrr::map_lgl(c(ffst, frds), fs::file_exists)
                      any(f_exists)

                    })
  names(dd) <- aux_indicators

  aux_indicators <- aux_indicators[dd]
  aux_dirs       <- aux_dirs[dd]

  names(aux_dirs) <- aux_indicators

  aux_tb <- data.table::data.table(
    auxname  = aux_indicators,
    auxfiles = aux_dirs
  )

  return(aux_tb)
}

aux_tb <- prep_aux_data(maindir = gls2$PIP_DATA_DIR)#,
                        #branch  = branch)
# filter
aux_tb <- aux_tb[!(auxname %chin% c("maddison"))]

aux_ver <- rep("00", length(aux_tb$auxname))

# aux_ver[which(aux_tb$auxname == "cpi")] <- -1 # remove for march update

dl_aux2 <- purrr::map2(.x = aux_tb$auxname,
                      .y =  aux_ver,
                      .f = ~ {
                        pipload::pip_load_aux(
                          measure     = .x,
                          apply_label = FALSE,
                          maindir     = gls2$PIP_DATA_DIR,
                          verbose     = FALSE,
                          version     = .y)#,
                          #branch      = branch)
                      }
)

names(dl_aux2) <- aux_tb$auxname

aux_versions2 <- purrr::map_df(aux_tb$auxname, ~{
  y <- attr(dl_aux2[[.x]], "version")
  w <- data.table(aux = .x,
                  version = y)
  w
})


# temporal change.
dl_aux2$pop$year <- as.numeric(dl_aux2$pop$year)


### Select PPP year

vars     <- c("ppp_year", "release_version", "adaptation_version")
ppp_v    <- unique(dl_aux2$ppp[, ..vars], by = vars)
data.table::setnames(x = ppp_v,
                     old = c("release_version", "adaptation_version"),
                     new = c("ppp_rv", "ppp_av"))
py <- 2017
# max release version
m_rv <- ppp_v[ppp_year == py, max(ppp_rv)]

# max adaptation year
m_av <- ppp_v[ppp_year == py & ppp_rv == m_rv,
              max(ppp_av)]


dl_aux2$ppp <- dl_aux2$ppp[ppp_year == py
                         & release_version    == m_rv
                         & adaptation_version == m_av
][,
  ppp_default := TRUE]


### Select the right CPI

cpivar <- paste0("cpi", py)

dl_aux2$cpi[, cpi := get(cpivar)]

### Select right Poverty lines table

dl_aux2$pl <- dl_aux2$pl[ppp_year == py
][,
  ppp_year := NULL]

### Select right Country Profile
dl_aux2$cp <-
  lapply(dl_aux2$cp,
         \(.) { # for each list *key indicators and charts
           lapply(.,
                  \(x) { # for each table inside each list
                    if ("ppp_year" %in% names(x)) {
                      x <-
                        x[ppp_year == py
                        ][,
                          ppp_year := NULL]
                    }
                    x
                  })
         })

## 1.4 - pinv ----
pip_inventory <-
  pipload::pip_find_data(
    inv_file     = fs::path(gls2$PIP_DATA_DIR,
                            '_inventory/inventory.fst'),
    filter_to_pc = TRUE,
    maindir      = gls2$PIP_DATA_DIR)


if (".joyn" %in% names(pip_inventory)) {
  pip_inventory[,
                .joyn := NULL]
}

# note this `db_filter_inventory` function is from
# https://github.com/PIP-Technical-Team/pip_ingestion_pipeline/blob/aaa4a161eb42bfa26294d27e334ac46004c4b009/R/pipdm/R/db_filter_inventory.R#L10
db_filter_inventory <- function(dt, pfw_table) {

  #--------- Prepare data for merge ---------

  # fix datatype for merge
  pfw_table[,
            surveyid_year := as.numeric(surveyid_year)
  ]

  dt[
    ,
    surveyid_year := as.numeric(surveyid_year)
  ]


  # Get original names + "new_filename_
  orig_names <- c(names(dt), "cache_id")

  dcols <- c(
    "cpi_domain",
    "ppp_domain",
    "gdp_domain",
    "pce_domain",
    "pop_domain"
  )

  #--------- filter and merge data ---------
  dt <-
    pfw_table[
      # filter inpovcal data
      inpovcal == 1
    ][dt, # Merge with inventory data
      on = c(
        "country_code",
        "surveyid_year",
        "survey_acronym"
      )
    ][,
      # Find MAX domain per obs
      reporting_level := apply(.SD, MARGIN = 1, max),
      .SDcols = dcols
    ]

  #--------- Create extra rows for alternative welfare ---------

  # Get obs with alternative welfare
  alt <- dt[oth_welfare1_type != ""]

  # all data
  dt[
    ,
    oth_welfare1_type := NULL # remove variable
  ][
    ,
    is_alt_welf := FALSE
  ]

  if (nrow(alt) != 0) {
    alt[
      ,
      welfare_type := fcase(
        grepl("^([Cc])", oth_welfare1_type), "consumption",
        grepl("^([Ii])", oth_welfare1_type), "income",
        default = ""
      )
    ][
      ,
      oth_welfare1_type := NULL # remove variable
    ][
      ,
      is_alt_welf := TRUE
    ]

    dt <- rbindlist(list(dt, alt),
                    use.names = TRUE,
                    fill = TRUE
    )
  } # end of confition alt != 0

  #--------- Create new name ---------

  # correspondence data
  crr <- dt[
    ,
    wt := fcase(
      welfare_type == "income", "INC",
      welfare_type == "consumption", "CON",
      default = ""
    )
  ][
    ,
    cache_id := paste(country_code,
                      surveyid_year,
                      survey_acronym,
                      paste0("D", reporting_level),
                      wt,
                      source,
                      sep = "_"
    )
  ][
    ,
    ..orig_names
  ]


  return(crr)
}
pipeline_inventory <-
  db_filter_inventory(dt        = pip_inventory,
                      pfw_table = dl_aux2$pfw)
pinv2 <- pipeline_inventory

# Step 2: estimate refy table ----
## 2.1 - df_refy
dsm2 <- qDT(dsm2)

source("./R/refy.R")

source("R/refy_mult_factor.R")
df_refy <- refy(gls = gls2,
                dsm = dsm2,
                pinv = pinv2,
                dl_aux = dl_aux2) |>
  refy_mult_factor()
# Step 3 ----
# 3.1 ---- get full list
full_list <-
  lapply(as.list(df_refy$country_code |> funique()),
         FUN = \(x) {
           l        <- c(as.list(x),
                         list(df_refy$reporting_year |>
                                funique()))
           names(l) <- c("country_code", "year")
           l
         })
ZAF_list <-
  lapply(as.list("ZAF"),
         FUN = \(x) {
           l        <- c(as.list(x),
                         list(df_refy$reporting_year |>
                                funique()))
           names(l) <- c("country_code", "year")
           l
         })
# 3.2 ----
dir <- "P:/03.pip/lineup_distributions/20240627_2017_01_02_PROD"
pipload:::write_multiple_refy_dist(df_refy     = df_refy,
                         cntry_refy  = full_list,
                         path        = fs::path(dir),
                         gls         = gls2,
                         dl_aux      = dl_aux2)

# Step 4: Compare----
## 4.1 check if `full_pipr_df` exists
if(!exists("full_pipr_df")) {

  full_pipr_df <-
    rowbind(pipr::get_stats(country   = "all",
                            year      = "all",
                            povline   = 2.15,
                            fill_gaps = TRUE) |>
              fmutate(pipr_type = "povline_215"),
            pipr::get_stats(country   = "all",
                            year      = "all",
                            povline   = 3.65,
                            fill_gaps = TRUE) |>
              fmutate(pipr_type = "povline_365"),
            pipr::get_stats(country   = "all",
                            year      = "all",
                            povline   = 6.85,
                            fill_gaps = TRUE) |>
              fmutate(pipr_type = "povline_685"),
            pipr::get_stats(country   = "all",
                            year      = "all",
                            povline   = 10,
                            fill_gaps = TRUE) |>
              fmutate(pipr_type = "povline_10"),
            pipr::get_stats(country   = "all",
                            year      = "all",
                            povline   = 50,
                            fill_gaps = TRUE) |>
              fmutate(pipr_type = "povline_50"),
            pipr::get_stats(country   = "all",
                            year      = "all",
                            popshare  = 0.01,
                            fill_gaps = TRUE) |>
              fmutate(pipr_type = "popshare_01"),
            pipr::get_stats(country   = "all",
                            year      = "all",
                            popshare  = 0.99,
                            fill_gaps = TRUE) |>
              fmutate(pipr_type = "popshare_99")) |>
    qDT()

}

## 4.2 get full comparison df----
full_list <-
  list(country_code = df_refy$country_code |> funique(),
       year = lapply(df_refy$country_code |> funique(),
                     FUN = \(x) {
                       df_refy$reporting_year |>
                         funique()
                     }))

full_comparison <-
  get_refy_comparison_df(compare_list = full_list,
                         path = dir,
                         full_pipr = qDT(full_pipr_df))


## 4.3 save full comparison df as fst
fst::write_fst(x = full_comparison,
               path = fs::path(dir,
                               "comparison_checks",
                               "full_comparison_df",
                               ext = "fst"))
