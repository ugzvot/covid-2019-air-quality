library(checkpoint)

checkpoint("2020-03-03")
library(tidyverse)

col_types <- cols(
  X1 = col_datetime(format = "%d.%m.%Y %H:%M"),
  X2 = col_double(),
  X3 = col_double(),
  X4 = col_double(),
  X5 = col_double(),
  X6 = col_double(),
  X7 = col_double(),
  X8 = col_double(),
  X9 = col_double(),
  X10 = col_double(),
  X11 = col_double(),
  X12 = col_double(),
  X13 = col_double(),
  X14 = col_double(),
  X15 = col_double(),
  X16 = col_double()
)
read_part <- function(skip, max, types=NULL, names=TRUE) {readr::read_delim(file = "http://ogd.zueriluft.ch/api/v1/h1.csv", 
                                            delim = ";",
                                            skip = skip,
                                            n_max = max,
                                            col_names = names,
                                            col_types = types,
                                            locale = readr::locale(encoding="latin1"))}

clean_header <- function(x){
  purrr::map(purrr::keep(as_vector(select_if(x, is.character)), ~!is.na(.x)), str_trim)
  
}

#Get locations
station_names <- read_part(1,1, names=FALSE) %>% clean_header
#Get measurement instrumen
quantity <- read_part(2,1, names=FALSE) %>% clean_header
#Get values

values <- read_part(6,Inf, names=FALSE) %>% readr::type_convert(col_types = col_types)


full_headers <- purrr::map2(station_names, quantity, ~ stringr::str_interp("${.x}&${.y}"))



untidy_air_data <- values %>% set_names(append("time",full_headers))

tidy_air_data <-
untidy_air_data %>% 
  pivot_longer(-time) %>% 
  separate(name, sep="&", into=c("station","measurement")) %>%
  pivot_wider(id_cols = c("time", "station", "measurement"), names_from = "measurement")