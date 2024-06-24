# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(fastverse)
library(joyn)
library(qs)

# Only uncomment when eneded:
devtools::install_github(repo = "PIP-Technical-Team/pipdata@DEV")

library(pipdata)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble",
               "fastverse",
               "joyn") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Define objects
#------------------------------------------

output_dir <- "P:/03.pip/lineup_distributions/"
output_dir_refy <- fs::path(output_dir,
                            "output-lineup-ref-years")


# Replace the target list below with your own:
list(

  # Load initial objects
  #--------------------------------------------------------------
  tar_target(
    name     = gls,
    command  = qs::qread(file = fs::path("data_inputs/gls.qs"))
    # format = "qs" # Efficient storage for general data objects.
  ),
  tar_target(
    name     = dsm,
    command  = qs::qread(file = fs::path("data_inputs/dsm.qs"))
  ),
  tar_target(
    name     = dsm,
    command  = qs::qread(file = fs::path("data_inputs/pinv.qs"))
  ),
  tar_target(
    name     = dsm,
    command  = qs::qread(file = fs::path("data_inputs/dl_aux.qs"))
  ),

  # Survey data
  #--------------------------------------------------------------
  tar_target(
    name     = svy_cache_ids,
    command  = get_svy_cache_ids(pinv)
  ),
  tar_target(
    name     = svy_list,
    command  = get_svy_list(svy_cache_ids)
  ),
  tar_target(
    name = svy_list,

  )

  # Estimate the lineup distributions


)
