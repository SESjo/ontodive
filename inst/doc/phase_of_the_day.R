## ----setup, include=FALSE------------------------------------------------------------------
# command to build package without getting vignette error
# https://github.com/rstudio/renv/issues/833
# devtools::check(build_args=c("--no-build-vignettes"))

# global option relative to rmarkdown
knitr::opts_chunk$set(
  cache = FALSE,
  echo = TRUE,
  fig.align = "center",
  out.width = "100%",
  message = FALSE,
  warning = FALSE
)

# library
library(data.table)
library(magrittr)
library(ggplot2)
library(TwGeos)
library(fpc)
library(lubridate)

# remove some warnings
suppressWarnings(library(ggplot2))

## ----phase-of-the-day-1--------------------------------------------------------------------
# load library
library(ontodive)

# load data
data_nes <- get_data("nes")

# combine all individuals
data_2018 <- rbindlist(data_nes$year_2018, use.name = TRUE, idcol = TRUE)

# remove phase column for the purpose of this document
data_2018[, phase := NULL]

## ----phase-of-the-day-2, fig.cap="Visualization of light level at the surface along 2018-individuals' trip", fig.height=6----
# let's first average `lightatsurf` by individuals, day since departure and hour
dataPlot <- data_2018[, .(lightatsurf = median(lightatsurf, na.rm = T)),
  by = .(.id, day_departure, date = as.Date(date), hour = hour(date))
]

# display the result
ggplot(dataPlot, aes(x = day_departure, y = hour, fill = lightatsurf)) +
  geom_tile() +
  facet_grid(.id ~ .) +
  theme_jjo() +
  labs(x = "# of days since departure", y = "Hour", fill = "Light level at the surface") +
  theme(legend.position = c("bottom"))

## ----phase-of-the-day-3, fig.cap="Distribution of `lightatsurf` with a threshold at 110."----
# display the result
ggplot(dataPlot, aes(x = lightatsurf, fill = .id)) +
  geom_histogram(show.legend = FALSE) +
  geom_vline(xintercept = 110, linetype = "longdash") +
  facet_wrap(.id ~ .) +
  theme_jjo()

## ----phase-of-the-day-4, fig.cap="Visualization of light level at the surface along 2018-individuals' trip, with twilight detection points", fig.height=6----
# identification of sunset, sunrise pairs
res_twi <- data_2018[!is.na(lightatsurf),
  findTwilights(.(Date = date, Light = lightatsurf),
    threshold = 110, include = date
  ),
  by = .id
]

# add `day_departure` to res_twi using a rolling join
# https://www.r-bloggers.com/2016/06/understanding-data-table-rolling-joins/
res_twi <- data_2018[, .(.id, Twilight = date, day_departure)] %>%
  .[res_twi, roll = T, on = c(".id", "Twilight")] %>%
  # hour column
  .[, hour := hour(Twilight)]

# display the result
ggplot() +
  geom_tile(data = dataPlot, aes(x = day_departure, y = hour, fill = lightatsurf)) +
  geom_point(data = res_twi, aes(x = day_departure, y = hour, col = Rise)) +
  facet_grid(.id ~ .) +
  theme_jjo() +
  labs(
    x = "# of days since departure",
    y = "Hour",
    fill = "Light level at the surface",
    col = "Sunrise"
  ) +
  theme(legend.position = c("bottom"))

## ----phase-of-the-day-5--------------------------------------------------------------------
# calculate the period of time between a sunrise and a sunset (i.e. two consecutive rows)
res_twi[, period_time := c(0, as.numeric(diff(Twilight, units = "hours"),
  units = "mins"
)),
by = .(.id, as.Date(Twilight))
]

# keep only the longer period of time and the row just before
res_twi_inter <- res_twi[
  c(
    # index of row with the longer period of time
    res_twi[, .I[period_time == max(period_time)],
      by = .(.id, as.Date(Twilight))
    ]$V1,
    # index of the row previous the one with the longer period of time
    res_twi[, .I[period_time == max(period_time)],
      by = .(.id, as.Date(Twilight))
    ]$V1 - 1
  )
] %>%
  # reorder by date
  .[order(Twilight)]

## ----phase-of-the-day-6, fig.cap="Visualization of light level at the surface along 2018-individuals' trip, with twilight detection points corrected", fig.height=6----
# display the result
ggplot() +
  geom_tile(data = dataPlot, aes(x = day_departure, y = hour, fill = lightatsurf)) +
  geom_point(data = res_twi_inter, aes(x = day_departure, y = hour, col = Rise)) +
  facet_grid(.id ~ .) +
  theme_jjo() +
  labs(
    x = "# of days since departure",
    y = "Hour",
    fill = "Light level at the surface",
    col = "Sunrise"
  ) +
  theme(legend.position = c("bottom"))

## ----zoom_2010074, fig.cap="Visualization of light level at the surface for the first 100 days of `ind_2018074`, with twilight detection points corrected"----
# display the result
ggplot() +
  geom_tile(
    data = dataPlot[.id == "ind_2018074" & day_departure < 100, ],
    aes(x = day_departure, y = hour, fill = lightatsurf)
  ) +
  geom_point(
    data = res_twi_inter[.id == "ind_2018074" & day_departure < 100],
    aes(x = day_departure, y = hour, col = Rise)
  ) +
  theme_jjo() +
  labs(
    x = "# of days since departure",
    y = "Hour",
    fill = "Light level at the surface",
    col = "Sunrise"
  ) +
  theme(legend.position = c("top"))

## ----phase-of-the-day-7, fig.cap="Distributions of the time difference between two rows identified as sunrise and sunset"----
# display
ggplot(res_twi_inter, aes(x = period_time, fill = .id)) +
  geom_histogram() +
  facet_grid(.id ~ .) +
  theme_jjo()

## ----phase-of-the-day-8--------------------------------------------------------------------
# remove outlier (but keep the 0)
res_twi_out <- res_twi[period_time == 0 | period_time %between% c(300, 900)]

# keep only the longer period of time and the row just before
res_twi_out_inter <- res_twi_out[
  c(
    # index of row with the longer period of time
    res_twi_out[, .I[period_time == max(period_time)],
      by = .(.id, as.Date(Twilight))
    ]$V1,
    # index of the row previous the one with the longer period of time
    res_twi_out[, .I[period_time == max(period_time)],
      by = .(.id, as.Date(Twilight))
    ]$V1 - 1
  )
  # reorder by date
] %>%
  # reorder by date
  .[order(Twilight)]

## ----phase-of-the-day-9, fig.cap="Visualization of light level at the surface along 2018-individuals' trip, with twilight detection points corrected", fig.height=6----
# display the result
ggplot() +
  geom_tile(data = dataPlot, aes(x = day_departure, y = hour, fill = lightatsurf)) +
  geom_point(data = res_twi_out_inter, aes(x = day_departure, y = hour, col = Rise)) +
  facet_grid(.id ~ .) +
  theme_jjo() +
  labs(
    x = "# of days since departure",
    y = "Hour",
    fill = "Light level at the surface",
    col = "Sunrise"
  ) +
  theme(legend.position = c("bottom"))

## ----phase-of-the-day-10-------------------------------------------------------------------
# # let's first split our dataset by individual
# split_inter = split(data_2018, data_2018$.id)
#
# # create a times series of ligth level
# split_inter = lapply(split_inter, function(x) {
#   # summerize data at the min level
#   df = x[,.(lightatsurf = median(lightatsurf, na.rm=T)), by=.(date = floor_date(date, unit="min"))]
#   # creation of a time series
#   z = as.ts(zoo(df$lightatsurf, df$date))
#   # smoother creation
#   # https://boostedml.com/2020/05/an-introduction-to-time-series-smoothing-in-r.html
#   s = ksmooth(time(z),as.numeric(z),'normal',bandwidth=6)
#   # retrieve smooth value
#   x[, lightatsurf_smooth := s$y]
# })
#
# # unlist
# data_2018 = rbindlist(split_inter)

