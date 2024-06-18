# Load packages
config   <-  config::get(config = Sys.info()['user'])
base_dir <- config$base_dir
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

# Load utils functions
source("R/utils.R")
library(collapse)
library(data.table)
library(joyn)
library(qs)
# Load objects
# gls    <- qs::qread(file = fs::path("data_inputs/gls.qs"))
# dsm    <- qs::qread(file = fs::path("data_inputs/dsm.qs"))
# pinv   <- qs::qread(file = fs::path("data_inputs/pinv.qs"))
# dl_aux <- qs::qread(file = fs::path("data_inputs/dl_aux.qs"))
