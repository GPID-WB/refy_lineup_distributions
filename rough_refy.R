#source(here::here("load_data.R"))
gls    <- qs::qread(file = fs::path("data_inputs/gls.qs"))
dsm    <- qs::qread(file = fs::path("data_inputs/dsm.qs"))
pinv   <- qs::qread(file = fs::path("data_inputs/pinv.qs"))
dl_aux <- qs::qread(file = fs::path("data_inputs/dl_aux.qs"))

# load packages
library(joyn)

gdp <- dl_aux$gdp
pce <- dl_aux$pce
ig  <- dl_aux$income_groups
pop <-   pop <- dl_aux$pop |>
  # THis naming is different because it is merged way down in the process
  frename(reporting_level = pop_data_level,
          reference_year  = year) |>
  fselect(-pop_domain)
cpi <- dl_aux$cpi
ppp <- dl_aux$ppp
pfw <- dl_aux$pfw

## Get Cumulative Growth and pass through ------------
passthrough <- .7
nac <- joyn(x          = gdp |> frename(gdp_data_level = data_level),
            y          = pce |> frename(pce_data_level = data_level),
            by         = c("country_code", "data_level", "year"),
            match_type = "1:1",
            reportvar  = FALSE)  |>
  joyn(y          = ig,
       by         = c("country_code", "year"),
       match_type = "m:1",
       reportvar  = FALSE,
       keep       = "left") |>
  fgroup_by(country_code, data_level) |>
  fmutate(income_group_code =
            income_group_code |>
            na_locf() |>
            na_focb()) |>
  fungroup() |>
  roworderv(c("country_code", "data_level", "year")) |>
  findex_by(country_code, data_level, year) |>
  ftransform(gdp_gr =  (G(gdp, scale = 1, shift = "row") + 1),
             pce_gr =  (G(pce, scale = 1, shift = "row") + 1)) |>
  unindex() |>
  fgroup_by(country_code, data_level) |>
  fmutate(inc_growth = select_cum_growth(gdp_gr,
                                         pce_gr,
                                         inc_group = income_group_code),
          con_growth = select_cum_growth(gdp_gr,
                                         pce_gr,
                                         inc_group   = income_group_code,
                                         passthrough = passthrough)) |>
  fungroup()

# reduced Price framework data
rpfw <- pfw |>
  fselect(country_code, survey_acronym, surveyid_year, year = reporting_year) |>
  collapse::funique()

# welfare type
w2k <- pinv |>
  ftransform(welfare_type = fifelse(grepl("_INC_", cache_id), "income", "consumption")) |>
  fselect(country_code, welfare_type, survey_acronym, surveyid_year) |>
  joyn::joyn(rpfw,
             by = c("country_code", "survey_acronym", "surveyid_year"),
             match_type = "m:1",
             keep = "inner",
             reportvar = FALSE
  ) |>
  fselect(country_code, welfare_type, year) |>
  roworder(country_code, welfare_type, year)

w2k <- pinv |>
  ftransform(welfare_type = fifelse(grepl("_INC_", cache_id), "income", "consumption")) |>
  fselect(country_code, welfare_type, survey_acronym, surveyid_year) |>
  joyn::joyn(rpfw,
             by = c("country_code", "survey_acronym", "surveyid_year"),
             match_type = "m:1",
             keep = "inner",
             reportvar = FALSE
  ) |>
  fselect(country_code, welfare_type, year) |>
  roworder(country_code, welfare_type, year)


#temporal solution
setkey(w2k, NULL)

w2k <-
  w2k[
    , ny := seq_len(.N)
    , by = .(country_code, year)
  ][
    ### remove alternative welfare
    ny == 1
  ][
    , ny := NULL
  ]


## Load survey mean ---------
mnt <- dsm |>
  fselect(country_code,
          year = reporting_year ,
          reporting_level,
          survey_year,
          welfare_type,
          mean = survey_mean_ppp) |>
  joyn::joyn(w2k,
             by = c("country_code", "welfare_type", "year"),
             match_type = "m:1",
             keep = "inner",
             reportvar = FALSE) |>
  # convert to national those with only one obs per year
  # number of data level
  _[, ndl := .N,
    by = c("country_code", "welfare_type", "year")
  ][, data_level := fifelse(ndl == 1, "national", reporting_level)
  ][,
    ndl := NULL]
#temporal solution
setkey(mnt, NULL)





## remove national if urb/rur data level is available for the same and remve
# urb/rur if national available is subsequent years
# we need to sort to create the correct id... very inefficient
setorder(mnt, country_code, reporting_level, year)

mn <- mnt |>
  # number of reporting levels within each country
  fgroup_by(country_code) |>
  fmutate(rlid = groupid(reporting_level),
          maxrlid  = fmax(rlid)) |> # how many reporting levels does country have
  fungroup() |>
  copy() |>
  _[,
    # number of rows per year
    nry := .N,
    by = .(country_code, year)
  ][,
    # identify obs to drop
    # we drop:
    tokeep := fcase(
      # 1. urban or rural when there is only one obs per year and there is at
      # least one National reporting level in the series (e.g., URY, BOL).
      maxrlid > 1 & nry == 1 & reporting_level != "national", FALSE,
      # 2. National if there are more than one obs per year (e.g., IND).
      maxrlid > 1 & nry > 1 & reporting_level == "national", FALSE,
      # 3. All the others should not be dropped
      default = TRUE
    )
  ]  |>
  fsubset(tokeep == TRUE) |>
  fselect(-c(rlid, maxrlid, nry, tokeep))

## expand those with decimal years ----
sy <- mn |>
  fmutate(freq = fifelse(survey_year %% 1 > 0, 2, 1),
          freq = fifelse(is.na(freq), 1, freq)) |>
  # expand for decimals
  _[rep(seq_len(.N), freq), !"freq"] |>
  # use as reference year the floor of survey_year
  ftransform(year_floor   = floor(survey_year),
             year_orginal = year) |>
  fgroup_by(country_code, year, data_level, welfare_type) |>
  # Decimals years are in between two regular years, so `yid`
  # identifies them
  fmutate(yid  = seqid(survey_year),
          year = fifelse(yid == 2, year_floor + 1, year_floor)) |>
  fungroup() |>
  # fsubset(country_code == "ALB")
  fselect(-c(year_floor))

## get growth at decimal years ----------
dynac <- joyn::joyn(sy, nac,
                    by = c("country_code", "data_level", "year"),
                    match_type = "m:1",
                    reportvar = FALSE,
                    keep = "left") |>
  ftransform(nac = fifelse(welfare_type == "consumption",
                           con_growth, inc_growth)) |>
  ftransform(dist_weight = fifelse(yid == 1,
                                   1 - (survey_year %% 1),
                                   survey_year %% 1)) |>
  fgroup_by(country_code, reporting_level, data_level, welfare_type, survey_year, mean) |>
  fsummarise(nac_sy = fmean(nac,dist_weight)) |>
  fungroup()

#temporal solution
setkey(dynac, NULL)


## expand to reference years --------------
rynac <- tidyr::expand_grid(dynac,
                            reference_year = gls$PIP_REF_YEARS) |>
  qDT() |>
  joyn::joyn(nac,
             by = c("country_code", "data_level", "reference_year = year"),
             match_type = "m:1",
             reportvar = FALSE,
             keep = "left") |>
  ftransform(nac = fifelse(welfare_type == "consumption",
                           con_growth, inc_growth)) |>
  roworderv(c("country_code", "data_level", "reference_year", "survey_year")) |>
  fgroup_by(country_code, data_level) |>
  fmutate(income_group_code =
            income_group_code |>
            na_locf() |>
            na_focb()) |>
  fungroup()
#temporal solution
setkey(rynac, NULL)



# 3. Growth in Reference year ----------

## svy years per ref year --------

byvars <-
  c("country_code",
    "data_level",
    "reference_year"
  )
rr <-
  rynac |>
  # Get differences between reference year and svy year
  ftransform(diff_year = reference_year - survey_year) |>
  fgroup_by(c(byvars)) |>
  fmutate(survey_select = fifelse(any(diff_year == 0), TRUE, FALSE)) |>
  fgroup_by(c(byvars, "welfare_type")) |>
  fmutate(lineup_case = fcase(all(diff_year < 0), "below",
                              all(diff_year > 0), "above",
                              default = "mixed")) |>
  fungroup() |>
  ftransform(lineup_case = fifelse(survey_select == TRUE & lineup_case == "mixed",
                                   "svy_year", lineup_case)) |>
  ftransform(sign = fcase(
    lineup_case != "svy_year" & diff_year < 0, -1, # 37 760
    lineup_case != "svy_year" & diff_year > 0, 1,  # 14 029
    default = 0)) |>                               # 47 919

  # get all min differences between years
  fgroup_by(c(byvars,"sign")) |>  #
  fmutate(min_diff_sign = fmin(abs(diff_year))) |>

  fgroup_by(c(byvars,"welfare_type")) |>  #
  fmutate(min_diff_welfare = fmin(abs(diff_year))) |>

  fgroup_by(c(byvars,"lineup_case")) |>  #
  fmutate(min_diff_case = fmin(abs(diff_year))) |>

  fgroup_by(c(byvars, "welfare_type", "lineup_case")) |>  #
  fmutate(min_diff_welfare_case = fmin(abs(diff_year))) |>

  fgroup_by(c(byvars, "welfare_type", "sign")) |>  #
  fmutate(min_diff_welfare_sign = fmin(abs(diff_year))) |>

  fgroup_by(c(byvars)) |>  #
  fmutate(min_diff = fmin(abs(diff_year))) |>
  fungroup()

# Obs to keep
cvy <- copy(rr) |>
  ftransform(keep = fcase(
    #min diff by welfare and sign if mixed (to filter more later)
    lineup_case == "mixed" & min_diff_welfare_sign*sign == diff_year, TRUE,
    # min diff if above or below (independent of welfare type)
    lineup_case %in% c("below","above") & min_diff*sign == diff_year, TRUE,
    # survey year
    lineup_case == "svy_year" & diff_year == 0, TRUE,
    default = FALSE)) |>
  # Duplicate welfare type in mixed cases
  # fgroup_by(c(byvars, "keep")) |>
  fgroup_by(c(byvars, "keep")) |>
  fmutate(dup_wt = NROW(lineup_case)) |>
  fgroup_by(c(byvars, "keep", "lineup_case")) |>
  fmutate(ulc = NROW(lineup_case)) |>
  fungroup() |>
  # drop income cases where there ref year has both income and consumption
  # surveys below and above
  ftransform(keep = fifelse(lineup_case == "mixed"
                            & keep == TRUE
                            & dup_wt == 4
                            & welfare_type != "consumption",
                            FALSE, keep)) |>
  # drop cases where only one welfare type is close to the ref year but the
  # other welfare type is both above and below (mixed)
  ftransform(keep = fifelse(lineup_case != "mixed"
                            & keep == TRUE
                            & dup_wt == 3
                            & ulc == 1,
                            FALSE, keep)) |>
  # select consumption over income when both inc and consumption are equally
  # appart from the ref year
  ftransform(keep = fifelse(keep == TRUE
                            & dup_wt == 2
                            & ulc == 1
                            & welfare_type != "consumption",
                            FALSE, keep)) |>
  fsubset(keep == TRUE) |>
  ## Add population
  joyn::joyn(pop,
             by = c("country_code", "reporting_level", "reference_year"),
             match_type = "m:1",
             keep       = "inner",
             reportvar = FALSE)
#temporal solution
setkey(cvy, NULL)



# Delte temporary vars
vars_to_del <-
  c(
    "dup_wt",
    "ulc",
    "keep",
    "survey_select",
    "diff_year",
    "sign",
    grep("^min_diff", names(cvy), value = TRUE)
  )

get_vars(cvy, vars_to_del) <- NULL


# 4. Mean at the reference year --------

rm <- cvy |>
  fgroup_by(country_code, data_level, welfare_type, reference_year) |>
  ## is growth monotonic ? -------------
## # This function is not in `wbpip`
fmutate(monotonic =
          is_monotonic(svy = nac_sy, ref = nac) |>
          false_if_not_true(),
        # nac and mean grow in the same direction
        same_direction =
          wbpip:::is_same_direction(nac_sy, mean) |>
          false_if_not_true()) |>

  ### interpolates mean when Same direction happens --------
fmutate(ref_mean =
          fifelse(monotonic       == TRUE &
                    same_direction  == TRUE &
                    lineup_case     == "mixed",
                  yes = same_dir_mean(svy_mean = mean,
                                      svy_nac  = nac_sy,
                                      ref_nac  = nac),
                  no  = NA)
) |>
  fungroup() |>

  # Extrapolated and interpolated mean for diverging case ------------
ftransform(ref_mean = fifelse(is.na(ref_mean),
                              mean*nac/nac_sy,
                              ref_mean)) |>
  # get relative distance for weighted mean
  fgroup_by(country_code,
            data_level,
            welfare_type,
            income_group_code,
            reference_year) |>
  fmutate(relative_distance = relative_distance(ref_year = reference_year,
                                                svy_year = survey_year)) |>
  fungroup() |>
  ftransform(estimation_type  = fcase(lineup_case == "mixed", "interpolation",
                                      lineup_case == "svy_year" , "survey",
                                      default = "extrapolation")) |>
  ftransform(interpolation_id = paste(country_code, reference_year, reporting_level,
                                      sep =  "_")) |>
  # New reference mean
  fselect(
    country_code      ,
    reporting_level,
    welfare_type      ,
    income_group_code ,
    survey_year       ,
    reporting_year = reference_year,
    nac               ,
    nac_sy,
    relative_distance,
    estimation_type,
    lineup_case,
    interpolation_id,
    predicted_mean_ppp = ref_mean,
    reporting_gdp = gdp,
    reporting_pce = pce,
    reporting_pop = pop,
    monotonic,
    same_direction,
    svy_mean = mean
  )

setorder(rm, country_code, reporting_level, welfare_type, survey_year, reporting_year)

out <- joyn::joyn(
  x = rm,
  y = dsm |> # remove survey year population to merge reference year pop
    fselect(-reporting_pop),
  by = c(
    "country_code",
    "reporting_level",
    "welfare_type",
    "survey_year"),
  match_type = "m:1",
  reportvar = FALSE,
  keep = "left"
) |>
  ftransform(is_interpolated = fifelse(estimation_type  != "survey", TRUE, FALSE)) |>
  _[,
    reporting_pop := fmean(reporting_pop, w = relative_distance),
    by = c("country_code", "reporting_level", "reporting_year", "welfare_type")]

#temporal solution
setkey(out, NULL)

print(out)



# Mean for welfare distribution lineups
#rm |> View()



################################################################################
#-------------------------- Ref Distributions ----------------------------------
################################################################################
cntry <- "ZAF"




# TEST METHODS OUT

# 1. Load the data
dt_list <- rowbind(lapply(as.list(pipload::pip_find_cache(c("CHN", "PRY"), # nac$country_code |> funique(),
                                                            version = gls$vintage_dir)),
                            FUN = function(x){
                              ct_code   <- substring(x, 1, 3)
                              yr        <- extract_year(x)
                              pipload::pip_load_cache(ct_code,
                                                      yr,
                                                      version = gls$vintage_dir) |>
                                fselect(country_code, surveyid_year, survey_acronym,
                                        survey_year, welfare, weight, reporting_level, welfare_type)
                            }))


# break approaches up first to investigate

### extrapolations (below and above)
zaf <- rm |>
  fsubset(country_code == cntry) |>
  fsubset(estimation_type == "extrapolation") |> # REMOVES SOME REPORTING YEARS!!
  fmutate(mult_factor = fcase(estimation_type == "extrapolation" , nac/nac_sy,
                              default = 1)) |>
  joyn::left_join(y            = dt_list |> fsubset(country_code == cntry),
                  by           = c("country_code", "reporting_level", "welfare_type", "survey_year"),
                  relationship = "many-to-many") |>
  fmutate(welfare_extrap = welfare * mult_factor)

# check with plots
zaf |>
  fsubset(reporting_year %in% c(1985, 2019, 2020, 2010)) |>
  #fgroup_by(reporting_year) |>
  #fsummarise(avwelfare = fmean(welfare_extrap, w = weight))
  fmutate(l_welfare = log(welfare_extrap),
          reporting_year = as.factor(reporting_year)) |>
  ggplot() +
  geom_density(aes(x = l_welfare, group = reporting_year, color = reporting_year, fill = reporting_year), alpha = .5) +
  theme_classic()

### interpolations (same)

zafinterp <- rm |>
  fsubset(country_code == cntry) |>
  fsubset(estimation_type == "interpolation" & monotonic == TRUE & same_direction == TRUE) |>
  fmutate(survey_1_2 = fifelse(survey_year < reporting_year, yes = 1, no = 2)) |>
  joyn::joyn(y = dt_list |> fsubset(country_code == cntry),
             by = c("country_code", "reporting_level", "welfare_type", "survey_year"),
             match_type = "m:m",
             keep = "left") |>
  fmutate(welfare_interp = welfare*predicted_mean_ppp/svy_mean)

zafinterp |>
  fsubset(reporting_year == c(2006)) |>
  fmutate(l_welfare = log(welfare_interp),
          reporting_year = as.factor(reporting_year),
          survey_1_2 = as.factor(survey_1_2)) |>
  ggplot() +
  geom_density(aes(x = l_welfare, group = survey_1_2, color = survey_1_2, fill = survey_1_2), alpha = .5) +
  theme_classic()

# problem: means are not equal for two groups
zafinterp |>
  fgroup_by(reporting_year, survey_year) |>
  fsummarise(m = fmean(welfare_interp,
                       w = weight))


### interpolations (divergent)





rm |>
  fsubset(country_code == cntry) |>
  fsubset(estimation_type == "interpolation" & !(monotonic == TRUE & same_direction == TRUE)) |>
  fmutate(survey_1_2 = fifelse(survey_year < reporting_year, yes = 1, no = 2)) |>
  joyn::joyn(y          = dt_list |> fsubset(country_code == cntry),
             by         = c("country_code",
                            "reporting_level",
                            "welfare_type",
                            "survey_year"),
             match_type = "m:m",
             keep       = "left") |>
  fmutate(welfare_extrap = welfare * nac/nac_sy)



#----- All together
gl <- rm |>
  fmutate(lineup_approach = fcase(estimation_type == "extrapolation" , "extrapolation",
                                  estimation_type == "interpolation" & monotonic == TRUE & same_direction == TRUE, "interpolation_same",
                                  estimation_type == "interpolation" & !(monotonic == TRUE & same_direction == TRUE), "interpolation_diverge",
                                  default = NA),
          mult_factor = fcase(lineup_approach == "extrapolation" | lineup_approach == "interpolation_diverge", nac/nac_sy,
                              lineup_approach == "interpolation_same", predicted_mean_ppp/svy_mean,
                              default = NA)) |>
  joyn::joyn(y          = dt_list,# |> fsubset(country_code == cntry),
             by         = c("country_code", "reporting_level", "welfare_type", "survey_year"),
             keep       = "left",
             match_type = "m:m",
             verbose = T) |>
  fmutate(welfare_lineup = welfare * mult_factor)






refy_distribution <- function(country_codes = c("CHN", "PRY")){ # nac$country_code |> funique()

  dt_list <- rbindlist(lapply(as.list(pipload::pip_find_cache(country_codes,
                                                              version = gls$vintage_dir)),
                              FUN = function(x){
                                ct_code   <- substring(x, 1, 3)
                                yr        <- extract_year(x)
                                pipload::pip_load_cache(ct_code,
                                                        yr,
                                                        version = gls$vintage_dir) |>
                                  fselect(country_code, surveyid_year, survey_acronym,
                                          survey_year, welfare, weight, reporting_level, welfare_type)
                              }))

  rm |>
    fmutate(lineup_approach = fcase(estimation_type == "extrapolation" , "extrapolation",
                                    estimation_type == "interpolation" & monotonic == TRUE & same_direction == TRUE, "interpolation_same",
                                    estimation_type == "interpolation" & !(monotonic == TRUE & same_direction == TRUE), "interpolation_diverge",
                                    default = NA),
            mult_factor = fcase(lineup_approach == "extrapolation" | lineup_approach == "interpolation_diverge", nac/nac_sy,
                                lineup_approach == "interpolation_same", predicted_mean_ppp/svy_mean,
                                default = NA)) |>
    joyn::joyn(y          = dt_list,
               by         = c("country_code", "reporting_level", "welfare_type", "survey_year"),
               keep       = "left",
               match_type = "m:m",
               verbose = T) |>
    fmutate(welfare_lineup = welfare * mult_factor)

}


#
#
#
# #----------------------------
# # Extrapolations
# min_year <- 1975
# max_year <- 2023
# cntry    <- "ZAF"
#
# # lower
# svy_years <- pipload::pip_find_cache(cntry,
#                                      version = gls$vintage_dir)
# svy_year_min <- sapply(svy_years, extract_year) |>
#   min()
# svy_welfare_min <- pipload::pip_load_cache(cntry,
#                                            svy_year_min,
#                                            version = gls$vintage_dir)
# fact_adj_down <- nac |> fsubset(country_code == cntry &
#                                   year == svy_year_min, c("inc_growth", "con_growth"))
# dt_extrap_down <- svy_welfare_min |>
#   fselect(country_code, survey_year, welfare_type, welfare, weight, reporting_level) |>
#   joyn(y          = nac |> fsubset(country_code == cntry &
#                                      year >= min_year & year <= svy_year_min),
#        keep       = "full",
#        by         = c("country_code", "reporting_level = data_level"),
#        match_type = "m:m",
#        y_vars_to_keep = c("year", "inc_growth", "con_growth")) |>
#   roworder(country_code, welfare) |>
#   fmutate(welfare = fifelse(year == svy_year_min, welfare,
#                             fifelse(welfare_type == "consumption",
#                                     welfare / (con_growth/fact_adj_down$con_growth),
#                                     welfare / (inc_growth/fact_adj_down$inc_growth))))
#
# dt_extrap_down |> fsubset(year %in% c(2023, 2020)) |>
#   fmutate(year    = as.factor(year),
#           welfare = fifelse(welfare < 0.25, 0.25, welfare),
#           welfare = log(welfare)) |>
#   ggplot() +
#   geom_density(aes(x = welfare, group = year, color = year, fill = year), alpha = .5) +
#   theme_classic()
#
#
# # upper
# svy_year_max <- sapply(svy_years, extract_year) |>
#   max()
# svy_welfare_max <- pipload::pip_load_cache(cntry,
#                                            svy_year_max,
#                                            version = gls$vintage_dir)
# fact_adj_up <- nac |> fsubset(country_code == cntry &
#                                 year == svy_year_max, c("inc_growth", "con_growth"))
# dt_extrap_up <- svy_welfare_max |>
#   fselect(country_code, survey_year, welfare_type, welfare, weight, reporting_level) |>
#   joyn(y          = nac |> fsubset(country_code == cntry &
#                                      year <= max_year & year >= svy_year_max),
#        keep       = "full",
#        by         = c("country_code", "reporting_level = data_level"),
#        match_type = "m:m",
#        y_vars_to_keep = c("year", "inc_growth", "con_growth")) |>
#   roworder(country_code, welfare) |>
#   fmutate(welfare = fifelse(year == svy_year_max, welfare,
#                             fifelse(welfare_type == "consumption",
#                                     welfare * (con_growth/fact_adj_up$con_growth),
#                                     welfare * (inc_growth/fact_adj_up$inc_growth))))
# dt_extrap_up
#
#
# dt_extrap_up |> fsubset(year %in% c(2023, 2020)) |>
#   fmutate(year = as.factor(year),
#           welfare = fifelse(welfare < 0.25, 0.25, welfare),
#           welfare = log(welfare)) |>
#   ggplot() +
#   geom_density(aes(x = welfare, group = year, color = year, fill = year), alpha = .5) +
#   theme_classic()
#
#
#
# #----------------------------
# # Interpolations
#
# # growth in survey means
# mn |>
#   findex_by(country_code, reporting_level, welfare_type) |>
#   ftransform(mean_gr = G(mean, scale = 1, shift = "row")) |>
#   fsubset(country_code == cntry) |>
#   unindex()
#
# rr |>
#   fsubset(lineup_case == "mixed") |>
#   #fsubset(country_code == cntry) |>
#   fgroup_by(country_code, reporting_level, welfare_type) |>
#   fmutate() |>


















