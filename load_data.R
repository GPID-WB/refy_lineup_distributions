#------------------------------------------
# refy distribution script
#------------------------------------------

#------------------------------------------
# 1. Imports
#------------------------------------------
#remotes::install_github("PIP-Technical-Team/pipload@dev")

# remotes::install_github("PIP-Technical-Team/wbpip@vectorize_spl",
#                        dependencies = FALSE)

# ZP: i reinstall wbpip to get the latest version
# remotes::install_github("PIP-Technical-Team/wbpip",
#                        dependencies = FALSE)
# remotes::install_github("PIP-Technical-Team/pipfun@DEV",
#                         dependencies = FALSE)
# remotes::install_github(repo = "PIP-Technical-Team/pipr",
#                         dependencies = FALSE)
## save data
force_create_cache_file         <- FALSE
save_pip_update_cache_inventory <- FALSE
force_gd_2_synth                <- FALSE
save_mp_cache                   <- FALSE

base_dir <- fs::path("C:/Users/wb612474/OneDrive - WBG/pip_technical_work/pip_ingestion_pipeline")

# Load packages
withr::with_dir(new = base_dir,
                code = {
                  # source("./_packages.R")

                  # Load R files
                  purrr::walk(fs::dir_ls(path = "./R",
                                         regexp = "\\.R$"), source)

                  # Read pipdm functions
                  purrr::walk(fs::dir_ls(path = "./R/pipdm/R",
                                         regexp = "\\.R$"), source)
                })
library(joyn)
# set defaults
py                 <- 2017  # PPP year
branch             <- "main"
branch             <- "DEV"
release            <- "20240326"
release            <- "20240429"
identity           <- "PROD"
identity           <- "INT"
max_year_country   <- 2022
max_year_aggregate <- 2022

## filter creation of synth data
cts <- yrs <- NULL

## save data
# force_create_cache_file         <- FALSE
# save_pip_update_cache_inventory <- FALSE
# force_gd_2_synth                <- TRUE
# save_mp_cache                   <- TRUE



#------------------------------------------
# 2. Data objects
#------------------------------------------


# The following are done below:
#      gls    -> global list
#      dl_aux -> auxiliary data
#      dsm    -> deflated survey means
#      pinv   -> pipeline inventory


#----
# gls
#----
gls <- pipfun::pip_create_globals(
  root_dir   = Sys.getenv("PIP_ROOT_DIR"),
  out_dir    = fs::path("y:/pip_ingestion_pipeline/temp/"),
  vintage    = list(release = release,
                    ppp_year = py,
                    identity = identity),
  create_dir = TRUE,
  max_year_country   = max_year_country,
  max_year_aggregate = max_year_aggregate
)



#----
# df_aux
#----
aux_tb  <- prep_aux_data(maindir = gls$PIP_DATA_DIR)
aux_tb  <- aux_tb[!(auxname %chin% c("maddison"))]
aux_ver <- rep("00", length(aux_tb$auxname))
# aux_ver[which(aux_tb$auxname == "cpi")] <- -1 # remove for march update
dl_aux <- purrr::map2(.x = aux_tb$auxname,
                      .y =  aux_ver,
                      .f = ~ {
                        pipload::pip_load_aux(
                          measure     = .x,
                          apply_label = FALSE,
                          maindir     = gls$PIP_DATA_DIR,
                          verbose     = FALSE,
                          version     = .y,
                          branch      = branch)
                      }
)
names(dl_aux) <- aux_tb$auxname
aux_versions <- purrr::map_df(aux_tb$auxname, ~{
  y <- attr(dl_aux[[.x]], "version")
  w <- data.table(aux = .x,
                  version = y)
  w
})

# temporal change.
dl_aux$pop$year <- as.numeric(dl_aux$pop$year)


#----
# dsm
#----
tar_load(svy_mean_ppp_table)
dsm <- svy_mean_ppp_table

#----
# pinv
#----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  PIP inventory ----
pip_inventory <-
  pipload::pip_find_data(
    inv_file = fs::path(gls$PIP_DATA_DIR, '_inventory/inventory.fst'),
    filter_to_pc = TRUE,
    maindir = gls$PIP_DATA_DIR)


if (".joyn" %in% names(pip_inventory)) {
  pip_inventory[, .joyn := NULL]
}

pipeline_inventory <-
  db_filter_inventory(dt        = pip_inventory,
                      pfw_table = dl_aux$pfw)
pinv <- pipeline_inventory


