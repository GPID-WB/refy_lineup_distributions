# Load packages
base_dir <- fs::path("P:/02.personal/wb612474/pip-technical/pip_ingestion_pipeline")
#config   <-  config::get(config = Sys.info()['user'])
#base_dir <- config$base_dir
withr::with_dir(new = base_dir,
                code = {
                   #source("./_packages.R")

                  # Load R files
                  purrr::walk(fs::dir_ls(path = "./R",
                                         regexp = "\\.R$"), source)

                  # Read pipdm functions
                  purrr::walk(fs::dir_ls(path = "./R/pipdm/R",
                                         regexp = "\\.R$"), source)


                })


output_dir <- "P:/03.pip/lineup_distributions/"
output_dir_refy <- fs::path(output_dir,
                            "output-lineup-ref-years")

# Load utils functions
# source("R/utils.R")
#devtools::install_github(repo = "PIP-Technical-Team/pipdata@DEV")

#devtools::install_github(repo = "PIP-Technical-Team/pipload@DEV")

all_scripts <- list.files(fs::path("R"),
                          pattern="*.R$", full.names=TRUE,
                          ignore.case=TRUE)


sapply(all_scripts[-which(all_scripts == "R/init.R" |
                            all_scripts == "R/draft_full_to_delete.R")],
       source,
       .GlobalEnv)

library(collapse)
library(data.table)
library(joyn)
library(qs)
library(pipdata)
# Load objects
gls    <- qs::qread(file = fs::path("data_inputs/gls.qs"))
dsm    <- qs::qread(file = fs::path("data_inputs/dsm.qs"))
pinv   <- qs::qread(file = fs::path("data_inputs/pinv.qs"))
dl_aux <- qs::qread(file = fs::path("data_inputs/dl_aux.qs"))

