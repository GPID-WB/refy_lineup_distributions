dt_ref <- refy(gls = gls,
               dsm = dsm,
               pinv = pinv,
               dl_aux = dl_aux) |>
  refy_mult_factor()


dt_ref |> dim()

rm <- dt_ref


refy_distributions <- function(rm, cntry_code, ref_year, gls) {

  # Filter rm
  rm <-
    rm |>
    fsubset(country_code == cntry_code &
              reporting_year == ref_year)

  # cache ids
  cache_id <- rm$cache_id

  # # survey years
  # svys <- pipload::pip_find_cache(cntry_code,
  #                                 version = gls$vintage_dir)
  # yrs  <- extract_year(svys)
  # keep <- which(abs(yrs - refy) <= minN(x = abs(yrs - refy),
  #                                       N = 2))
  # yrs  <- yrs[keep]

  # reporting level change
  # welfare level change?


  # Interpolation vs extrapolation
  # if ( all(yrs <= refy)) {
  #   yrs <- yrs[which.min(abs(yrs - refy))]
  # }

  # Append (possibly two) surveys on other side of refy
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                      FUN = function(x){
                                        pipload::pip_load_cache(cache_id = x,
                                                                version = gls$vintage_dir) |>
                                          fselect(country_code, surveyid_year, survey_acronym,
                                                  survey_year, welfare, weight, reporting_level, welfare_type)
                                      }))
  # adjust weights - there are two separate surveys now with incomparable weights
  df_svy <- df_svy |>
    fgroup_by(survey_year, reporting_level, welfare_type) |>
    fmutate(weight = weight / fsum(weight)) |>
    fungroup()

  df_refy <- rm |>
    fsubset(country_code == cntry_code) |>
    joyn::joyn(y          = df_svy,
               by         = c("country_code", "reporting_level", "welfare_type", "survey_year"),
               keep       = "left",
               match_type = "1:m",
               verbose = T) |>
    fmutate(welfare_lineup = welfare * mult_factor)

  df_refy

}

