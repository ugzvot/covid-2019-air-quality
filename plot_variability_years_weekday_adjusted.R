# Stellt den Verlauf verschiedener NOx Messsationen im März der letzten 5 Jahre + 2020 dar.
# Die historischen Zeitreihen werden verschoben, so dass die Wochentage aufeinanderfallen. 
# Damit fallen die Wochenend Effekte aufeinander.
# 
# Written in 2020 by Thomas von Allmen
# 
# To the extent possible under law, the author(s) have dedicated all copyright and related
# and neighboring rights to this script to the public domain worldwide. This script is
# distributed without any warranty. See <http://creativecommons.org/publicdomain/zero/1.0/>
#

library(rOstluft)       # Package Quelle: https://github.com/Ostluft/rOstluft
library(purrr)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rlang)
library(scales)
library(glue)
library(tibble)
library(tidyr)



# aktuelle NOx Daten vom Webexport
nox_2020 <- rOstluft::read_airmo_webexport("http://ogd.zueriluft.ch/api/v1/ol_nox_covid19_2020.csv")
nox_2020 <- rOstluft::pluck_parameter(nox_2020, "NOx")

# h1 oder min30 Daten nutzen? gemeinsame basis für praktisch alle Daten wäre h1
nox_2020 <- rOstluft::resample(nox_2020, "mean", "h1")

# historische NOx Daten vom s3 aqmet store für 2015 bis 2019
sites <- levels(nox_2020$site)
historic_years <- 2015:2019

s <- rOstluft::store_aqmet_public()
data_historic <- s$get(site = sites, year = historic_years, interval = "h1")
# saveRDS(data_historic, "daten_2015-2019.rds")
# data_historic <- readRDS("daten_2015-2019.rds")

nox_historic <- rOstluft::pluck_parameter(data_historic, "NOx")

# fasse alle noxdaten zusammen
nox <- rOstluft::bind_rows_with_factor_columns(nox_historic, nox_2020)

# filtere die Daten für jedes Jahr. Starte mit dem Montag am nächsten zum 9.3
origin <- lubridate::ymd("2020-03-09", tz = "Etc/GMT-1")
end <- lubridate::floor_date(lubridate::now("Etc/GMT-1"), "day")
duration <- lubridate::as.period(end - origin)


# Hilfsunktion sucht das nächste Datum mit dem gleichen Wochentag wie der Ursprung im gesuchten Jahr
align_year_on_weekday_offset <- function(origin, target_year) {
  origin <- lubridate::as_date(origin)
  origin_wday <- lubridate::wday(origin)
  target <- lubridate::`year<-`(origin, target_year)
  target_wday <- lubridate::wday(target)
  offset <- ((origin_wday - target_wday + 3) %% 7) - 3
  offset
}

years <- c(historic_years, 2020)
all_years <- tibble::tibble( year = years)

all_years <- dplyr::mutate(all_years,
  start = lubridate::`year<-`(origin, .data$year),
  offset = align_year_on_weekday_offset(origin, .data$year),
  start_day = .data$start + lubridate::days(.data$offset),  
  end_day = .data$start_day + duration,
  
  # Kontrolle
  weekday = lubridate::wday(.data$start_day, week_start = 1, label = TRUE, abbr = FALSE)
)

# pull die Vektoren, dann können wir den alten Code verwenden
start_days <- rlang::set_names(dplyr::pull(all_years, "start_day"), years)
end_days <- rlang::set_names(dplyr::pull(all_years, "end_day"), years)

# NSE zur Erstellung einer Filter Expression für jeden Zeitraum für jedes jahr
create_filter_quo <- function(start_date, end_date) {
  rlang::quo(( .data$starttime >= {{start_date}} & .data$starttime < {{end_date}} ))
}

reduce_filter_or <- function(acc, nxt) {
  rlang::quo(!!acc | !!nxt)
}

filter_list_quo <- purrr::map2(start_days, end_days, create_filter_quo)
filter_arg <- purrr::reduce(filter_list_quo, reduce_filter_or)

nox_filtered <- dplyr::filter(nox, !!filter_arg)

# gruppiere bei jahr und fülle lücken auf in jedem Zeitraum
nox_filtered <- dplyr::group_by(nox_filtered, year = lubridate::year(.data$starttime), add = TRUE)

pad_group <- function(data, key) {
  start_date <- lubridate::as_datetime(start_days[as.character(key$year)], tz = "Etc/GMT-1")
  end_date <- lubridate::as_datetime(end_days[as.character(key$year)], tz = "Etc/GMT-1")
  rOstluft::pad(data, start_date, end_date, drop_last = TRUE)
}

nox_filtered <- dplyr::group_map(nox_filtered, pad_group, keep = TRUE)
nox_filtered <- rOstluft::bind_rows_with_factor_columns(!!!nox_filtered)

# join nox_filtered mit all_years um den offset hinzufügen
offset_years <- dplyr::select(all_years, "year", "offset")
offset_years <- dplyr::mutate(offset_years, offset = lubridate::days(.data$offset))
nox_filtered <- dplyr::left_join(nox_filtered, offset_years, by = "year")

# um verschiedene Jahre auf der gleichen Achse zu plotten müssen wir eine gemeinsame Zeitachse einführen
# factorize Jahr damit es diskret ist und nicht kontinuerlich (für ggplot scales)
nox_graph <- dplyr::mutate(nox_filtered,
  x = lubridate::`year<-`(.data$starttime, 2020) - .data$offset,
  size = dplyr::if_else(.data$year == 2020, 0.75, 0.5),
  year = as.factor(.data$year)
)

# Color scale basierend auf viridis aber mit 2020 schwarz
#color_scale <- viridisLite::cividis(length(all_years), begin = 0.2, end = 0.8)
color_scale <- scales::brewer_pal(type = "qual", palette = "Dark2")(length(historic_years))
color_scale <- c(color_scale, "black")
color_scale <- rlang::set_names(color_scale, years)

legend_labels <- glue::glue_data(all_years, '{year}: {format(start_day, "%d.%m")} - {format(end_day, "%d.%m")}')

plt <- ggplot(nox_graph, aes(x = .data$x, y = .data$value, color = .data$year, size = .data$size)) +
  labs(
    title = "Meteorologische Variabilität von NOx über verschiedene Jahre (Wochentag adustiert)",
    x = "Datum für Jahr 2020",
    y = "NOx [ppb]"
  ) +
  geom_line() +
  scale_size_identity() +
  scale_color_manual(
    labels = legend_labels,
    values = color_scale,
    guide = guide_legend(title = "Jahr", nrow = 1, override.aes = list(size = 2))
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    expand = expansion(mult = 0.01),
    date_labels = "%d %b"
  ) +
  facet_wrap(vars(site), ncol = 2, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "top"
  )

ggsave("figures/NOx_Varibilitaet_per_Jahr_Wochentag_adjustiert.png", plt, width = 1600/300, height = 900 / 300, scale = 2.5)

