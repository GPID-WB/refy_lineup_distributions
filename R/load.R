
load_refy <- function(country_code,
                      year,
                      path = output_dir_refy) {

  qs::qread(file =
              fs::path(path,
                       paste0(country_code,
                              "_",
                              year,
                              ".qs")))

}
zaf1999 <- load_refy(country_code = "ZAF",
                     year         = 1999)

inpt <- list(list(country_code = "ZAF",
          year         = 2000:2005),
     list(country_code = "COL",
          year         = 2000:2005))




load_append_refy <- function(input_list,
                             path = output_dir_refy) {

  # transform input list
  input_list <- transform_input(input_list)

  # envir for attributes
  e <- rlang::new_environment()

  # appended data
  dt <- lapply(input_list,
               FUN = \(x) {
                 d <-
                   load_refy(country_code = x$country_code,
                             year         = x$year,
                             path         = path)
                 dattr <- attributes(d)
                 assign(x     = paste0(x$country_code,
                                       x$year,
                                       "_attr"),
                        value = dattr,
                        envir = e)
                 d
               })

  # rowbind
  dt <- collapse::rowbind(dt)

  # lsit of attributes
  dattr <- as.list(e)
  attributes(dt) <- c(attributes(dt),
                      as.list(e))

  dt

}
d <- load_append_refy(
  list(
    country_code = c("COL", "ZAF", "ARG"),
    year         = list(2010:2011,
                        2006:2007,
                        2010)
  )
)


load_attr <- function(country_code,
                      year,
                      path = output_dir_refy) {

  qs::qattributes(fs::path(path,
                           paste0(country_code,
                                  "_",
                                  year,
                                  ".qs")))

}

load_dist_stats <- function(country_code,
                            year,
                            path = output_dir_refy) {

  all_attr <- load_attr(country_code,
                        year,
                        path)

  all_attr$dist_stats

}
load_aux_data <- function(country_code,
                           year,
                           path = output_dir_refy) {

  all_attr <- load_attr(country_code,
                        year,
                        path)

  all_attr$aux_data

}





# helper functions

transform_input <- function(input_list) {

  country_codes <- input_list$country_code
  years         <- input_list$year

  # years as list
  if (!is.list(years)) {
    years <- lapply(country_codes,
                    function(x) years)
  } else {
    # Check if the length of the year list matches the length of the country_codes
    if (length(years) != length(country_codes)) {
      stop("The length of the 'year' list must match the length of the 'country_code' vector.")
    }
  }

  # each element one country-year
  output_list <- lapply(seq_along(country_codes), function(i) {
    lapply(years[[i]], function(y) {
      list(country_code = country_codes[i], year = y)
    })
  })

  # flatten
  output_list <- unlist(output_list, recursive = FALSE)

  return(output_list)
}












# rough work below

list(country_code  = c("COL", "ZAF", "ARG"),
     year = 2010:2020)

# transform into this
list(
  list(country_code  = c("COL"),
       year = 2015:2020),
  list(country_code  = c("ZAF"),
       year = 2006:2020),
  list(country_code  = c("COL"),
       year = 2010:2020)
)



# This is error
list(country_code  = c("COL", "ZAF", "ARG"),
     year = list(2010:2020,
                 2006:2020))

# This is OK
list(country_code  = c("COL", "ZAF", "ARG"),
     year = list(2010:2020,
                 2006:2020,
                 2010))
