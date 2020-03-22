# Stellt den Tagesgang an Wochentagen und Wochende der letzten 5 Jahre an verschiedenen NOx Messstation.
# In den Jahren 2018 und 2019 beinflusste eine Baustelle die Messtation Zch_Stampfenbachstrassse.
# 
# Written in 2020 by Thomas von Allmen
# 
# To the extent possible under law, the author(s) have dedicated all copyright and related
# and neighboring rights to this script to the public domain worldwide. This script is
# distributed without any warranty. See <http://creativecommons.org/publicdomain/zero/1.0/>
#

library(rOstluft)       # Package Quelle: https://github.com/Ostluft/rOstluft
library(rOstluft.plot)  # Package Quelle: https://github.com/Ostluft/rOstluft.plot
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

# historische NOx Daten vom s3 aqmet store für 2015 bis 2019
sites <- levels(nox_2020$site)
historic_years <- 2015:2019
# s <- rOstluft::store_aqmet()
# data_historic <- s$get(site = sites, year = historic_years, interval = "h1")
# saveRDS(data_historic, "daten_2015-2019.rds")
data_historic <- readRDS("daten_2015-2019.rds")

nox_historic <- rOstluft::pluck_parameter(data_historic, "NOx")

nox <- dplyr::mutate(nox_historic,
  starttime = lubridate::with_tz(.data$starttime, "CET"), # DST Zeitzone für Verhalten
  month = lubridate::month(.data$starttime),
  year = lubridate::year(.data$starttime)
)

nox <- dplyr::filter(nox, .data$month %in% c(3,4))

groupings <- rOstluft.plot::grp("starttime_of_day", "weekend", "year")
nox_periodic <- rOstluft.plot::summary_periodic(nox, groupings = groupings)
nox_periodic <-  tidyr::spread(nox_periodic, "stat", "value")
times_of_day <- levels(nox_periodic$starttime_of_day)
breaks <- seq(length.out = 8, by = length(times_of_day) / 8 )
labels <- times_of_day[breaks]

color_scale <- scales::brewer_pal(type = "qual", palette = "Dark2")(length(historic_years))

plt <- ggplot(nox_periodic, aes(x = as.numeric(.data$starttime_of_day), y = .data$mean, color = as.factor(.data$year))) +
  labs(
    title = "NOx Tagesgang der Monate März und April der letzten 5 Jahre",
    y = "NOx [ppb]",
    x = "Uhrzeit (Sommerzeit)"
  ) +
  geom_line( size = 0.75) +
  facet_grid(
    rows = vars(site),
    cols = vars(weekend),
    scales = "free_y"
  ) +
  scale_color_manual(
    values = color_scale,
    guide = guide_legend(title = "Jahr", override.aes = list(size = 2))
  ) +
  theme(
    strip.text.y = element_text(size = 5)
  )

ggsave("figures/NOx_Tagesgang_2015-2019.png", plt, width = 800/300, height = 1000 / 300, scale = 2.5)
