# load objects & packages
source("R/init.R")
pinv$cache_id[1]
path <- "P:/03.pip/lineup_distributions"
#path <- "//wbntpcifs/povcalnet/03.pip/lineup_distributions"


load_and_save_svy <- function(cache_id, gls, path, code, year) {
  # Load data
  df_svy <- pipload::pip_load_cache(cache_id = cache_id,
                                    version  = gls$vintage_dir)

  # Save data
  write_fst(x   = df_svy,
            path = fs::path(path, "input-survey-data", paste(code, year, sep = "_"), ".fst"))
}

load_and_save_svy(pinv$cache_id[1],
                  gls = gls, path = path)

dd <- read_fst(path = fs::path("P:/03.pip/estimates/temp-Prosperity_Gap/20240429_2017_01_02_INT/svy_years/PG_svy_year.fst"))


cache_id <- pinv$cache_id
df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                   FUN = function(x){
                                     pipload::pip_load_cache(cache_id = x,
                                                             version = gls$vintage_dir) |>
                                       fselect(country_code, surveyid_year, survey_acronym,
                                               survey_year, welfare_ppp, weight,
                                               reporting_level, welfare_type, imputation_id)
                                   }))
