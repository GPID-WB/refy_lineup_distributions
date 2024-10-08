# Extract year from cached survey name
# example:
# cntry <- "ZAF"
# svy_names <- pipload::pip_find_cache(cntry,
#                                      version = gls$vintage_dir)
# years <- sapply(svy_names, extract_year)




#' Extract year from string
#'
#' This function extracts a four-digit year from a given string.
#'
#' @param string A character string that potentially contains a four-digit year (e.g., "XYZ_2023_abc").
#'
#' @return A numeric value representing the four-digit year extracted from the string. If multiple years are found, all are returned as a numeric vector.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with a single year in the string
#' year <- extract_year("Report_2023_version")
#' # Returns: 2023
#'
#' # Example with multiple years in the string
#' years <- extract_year("Data_2019_2021_final")
#' # Returns: c(2019, 2021)
#' }
extract_year <- function(string) {
  match_positions <- gregexpr("\\d{4}", string)
  year <- regmatches(string, match_positions)
  as.double(year)
}



# Nth smallest number in a vector
minN <- function(x, N = 2){
  len <- length(x)
  if (N > len) {
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x, partial = N)[N]
}

minN(4:10)
