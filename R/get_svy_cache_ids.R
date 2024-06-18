get_svy_cache_ids <- function(pinv) {

  pinv$cache_id |>
    funique() |>
    as.list()

}
