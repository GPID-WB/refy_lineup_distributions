refy_mult_factor <- function(rm, cntry_code, refy, gls) {

  rm |>
    fmutate(lineup_approach = fcase(estimation_type == "extrapolation" , "extrapolation",
                                    estimation_type == "interpolation" & monotonic == TRUE & same_direction == TRUE, "interpolation_same",
                                    estimation_type == "interpolation" & !(monotonic == TRUE & same_direction == TRUE), "interpolation_diverge",
                                    default = NA),
            mult_factor = fcase(lineup_approach == "extrapolation" | lineup_approach == "interpolation_diverge", nac/nac_sy,
                                lineup_approach == "interpolation_same", predicted_mean_ppp/svy_mean,
                                default = NA))

}




