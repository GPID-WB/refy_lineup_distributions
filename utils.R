# Extract year from cached survey name
# example:
# cntry <- "ZAF"
# svy_names <- pipload::pip_find_cache(cntry,
#                                      version = gls$vintage_dir)
# years <- sapply(svy_names, extract_year)
extract_year <- function(string) {
  match_positions <- gregexpr("\\d{4}", string)
  year <- regmatches(string, match_positions)
  as.double(year)
}


minN <- function(x, N = 2){
  len <- length(x)
  if (N > len) {
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial = N)[N]
}

minN(1:10)
