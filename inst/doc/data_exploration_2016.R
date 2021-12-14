## ----setup, include=FALSE-----------------------------------------------------------------------------------------------
# command to build package without getting vignette error
# https://github.com/rstudio/renv/issues/833
# devtools::check(build_args=c("--no-build-vignettes"))

# reduce png size
knitr::knit_hooks$set(optipng = knitr::hook_optipng)
knitr::knit_hooks$set(pngquant = knitr::hook_pngquant)

# global option relative to rmarkdown
knitr::opts_chunk$set(
  echo = TRUE,
  fig.align = "center",
  out.width = "100%",
  message = FALSE,
  warning = FALSE,
  # tidy = TRUE,
  optipng = "-o7 -quiet",
  pngquant = "--speed=1"
)

# library
library(data.table)
library(ggplot2)
library(kableExtra)
library(leaflet)
library(gtsummary) # https://cran.r-project.org/web/packages/gtsummary/vignettes/tbl_summary.html
library(corrplot)
library(ggcorrplot)
library(ggnewscale)
library(magrittr)
library(DT)
library(plotly)
library(geosphere)

# remove some warnings
suppressWarnings(library(ggplot2))

# define my own table format: https://github.com/haozhu233/kableExtra/issues/374
sable <- function(x, escape = T, ...) {
  knitr::kable(x, escape = escape, ...) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "responsive"),
      full_width = F
    )
}

# theme ggplot
# based: https://benjaminlouis-stat.fr/en/blog/2020-05-21-astuces-ggplot-rmarkdown/
theme_jjo <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # the whole figure
      # plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # figure area
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # axes
      # axis.title = element_text(size = rel(0.85), face = "bold"),
      # axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.2, "lines"), type = "closed")),
      # legend
      # legend.title = element_text(size = rel(0.85), face = "bold"),
      # legend.text = element_text(size = rel(0.70), face = "bold"),
      # legend.key = element_rect(fill = "transparent", colour = NA),
      # legend.key.size = unit(1.5, "lines"),
      # legend.background = element_rect(fill = "transparent", colour = NA),
      # Les <U+00E9>tiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#888888", color = "#888888"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5, 0, 5, 0))
    )
}

## ----data-exploration-2016-1--------------------------------------------------------------------------------------------
# load library
library(weanlingNES)

# load data
data("data_nes", package = "weanlingNES")
# load("../data/data_nes.rda")

## ----data-exploration-2016-2--------------------------------------------------------------------------------------------
# list structure
str(data_nes$year_2016, max.level = 1, give.attr = F, no.list = T)

## ----data-exploration-2016-3, eval=FALSE--------------------------------------------------------------------------------
#  # combine all individuals
#  data_2016 <- rbindlist(data_nes$year_2016, use.name = TRUE, idcol = TRUE)
#  
#  # display
#  DT::datatable(data_2016[sample.int(.N, 100), ], options = list(scrollX = T))

## ----data-exploration-2016-4, echo=FALSE, results='asis'----------------------------------------------------------------
# combine all individuals
data_2016 <- rbindlist(data_nes$year_2016, use.name = TRUE, idcol = TRUE)

# title
cat("<table style='width: 50%'>", paste0("<caption>", "(#tab:myDThtmltools)", "Sample of 100 random rows from `data_2016`", "</caption>"), "</table>", sep = "\n")

# display
DT::datatable(data_2016[sample.int(.N, 100), ], options = list(scrollX = T))

## ----data-exploration-2016-5--------------------------------------------------------------------------------------------
# raw_data
data_2016[, .(
  nb_days_recorded = uniqueN(as.Date(date)),
  max_depth = max(maxpress_dbars),
  sst_mean = mean(sst2_c),
  sst_sd = sd(sst2_c)
), by = .id] %>%
  sable(
    caption = "Summary diving information relative to each 2016 individual",
    digits = 2
  )

## ----data-exploration-2016-6, fig.cap="Distribution of raw `sst2` for the four individuals in 2016"---------------------
ggplot(data_2016, aes(x = sst2_c, fill = .id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  theme_jjo()

## ----data-exploration-2016-7, fig.cap="Distribution of filtered `sst2` for the four individuals in 2016"----------------
data_2016_filter <- data_2016[sst2_c < 500, ]
ggplot(data_2016_filter, aes(x = sst2_c, fill = .id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  theme_jjo()

## ----data-exploration-2016-8--------------------------------------------------------------------------------------------
# nbrow removed
data_2016[sst2_c > 500, .(nb_row_removed = .N), by = .id] %>%
  sable(caption = "# of rows removed by 2016-individuals")

## ----data-exploration-2016-9, fig.cap="Where and when the `sst2` outliers occured", fig.width=9-------------------------
# max depth
ggplot(
  data_2016,
  aes(y = -maxpress_dbars, x = as.Date(date), col = .id)
) +
  geom_path(show.legend = FALSE) +
  geom_point(data = data_2016[sst2_c > 500, ], col = "black") +
  scale_x_date(date_labels = "%m/%Y") +
  labs(y = "Pressure (dbar)", x = "Date") +
  facet_wrap(.id ~ .) +
  theme_jjo() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ----data-exploration-2016-10, fig.cap="It is supposed to be the track of `ind_3449`... (red dots are the location of removed rows)", fig.width=8----
# interactive map
leaflet() %>%
  setView(lng = -122, lat = 38, zoom = 2) %>%
  addTiles() %>%
  addPolylines(
    lat = data_2016[.id == "ind_3449", latitude_degs],
    lng = data_2016[.id == "ind_3449", longitude_degs],
    weight = 2
  ) %>%
  addCircleMarkers(
    lat = data_2016[.id == "ind_3449" & sst2_c > 500, latitude_degs],
    lng = data_2016[.id == "ind_3449" & sst2_c > 500, longitude_degs],
    radius = 3,
    stroke = FALSE,
    color = "red",
    fillOpacity = 1
  )

## ----data-exploration-2016-11-------------------------------------------------------------------------------------------
# summary of the coordinates by individuals
data_2016[, .(.id, longitude_degs, latitude_degs)] %>%
  tbl_summary(by = .id) %>%
  modify_caption("Summary of `longitude_degree` and `latitude_degree`")

## ----data-exploration-2016-12, fig.width=9, fig.cap="Distribution of coordinates per seal"------------------------------
# distribution coordinates
ggplot(
  data = melt(data_2016[, .(
    Longitude = longitude_degs,
    Latitude = latitude_degs,
    .id
  )],
  id.vars =
    ".id", value.name = "Coordinate"
  ),
  aes(x = Coordinate, fill = .id)
) +
  geom_histogram(show.legend = F) +
  facet_grid(variable ~ .id) +
  theme_jjo()

## ----data-exploration-2016-13, fig.cap="An attempt to display the `ind_3449`'s track", fig.width=8----------------------
# interactive map
leaflet() %>%
  setView(lng = -122, lat = 50, zoom = 3) %>%
  addTiles() %>%
  addPolylines(
    lat = data_2016[.id == "ind_3449", abs(latitude_degs)],
    lng = data_2016[.id == "ind_3449", -abs(longitude_degs)],
    weight = 2
  )

## ----data-exploration-2016-14, fig.cap="Check for missing value in 2016-individuals", out.width="100%"------------------
# build dataset to check for missing values
dataPlot <- melt(data_2016_filter[, .(.id, is.na(.SD)), .SDcol = -c(
  ".id",
  "rec#",
  "date",
  "time"
)])
# add the id of rows
dataPlot[, id_row := c(1:.N), by = c("variable", ".id")]

# plot
ggplot(dataPlot, aes(x = variable, y = id_row, fill = value)) +
  geom_tile() +
  labs(x = "Attributes", y = "Rows") +
  scale_fill_manual(
    values = c("white", "black"),
    labels = c("Real", "Missing")
  ) +
  facet_wrap(.id ~ ., scales = "free_y") +
  theme_jjo() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_rect(colour = "black")
  )

