## ----setup, include=FALSE-----------------------------------------------------
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
  tidy = TRUE,
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

## -----------------------------------------------------------------------------
# load library
library(weanlingNES)

# load data
data("data_nes", package = "weanlingNES")
# load("../data/data_nes.rda")

## -----------------------------------------------------------------------------
# list structure
str(data_nes$year_2016, max.level = 1, give.attr = F, no.list = T)

## ---- eval=FALSE--------------------------------------------------------------
#  # combine all individuals
#  data_2016 <- rbindlist(data_nes$year_2016, use.name = TRUE, idcol = TRUE)
#  
#  # display
#  DT::datatable(data_2016[sample.int(.N, 100), ], options = list(scrollX = T))

## ---- echo=FALSE, results='asis'----------------------------------------------
# combine all individuals
data_2016 <- rbindlist(data_nes$year_2016, use.name = TRUE, idcol = TRUE)

# title
cat("<table style='width: 50%'>", paste0("<caption>", "(#tab:myDThtmltools)", "Sample of 100 random rows from `data_2016`", "</caption>"), "</table>", sep = "\n")

# display
DT::datatable(data_2016[sample.int(.N, 100), ], options = list(scrollX = T))

## -----------------------------------------------------------------------------
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

## ---- fig.cap="Distribution of raw `sst2` for the four individuals in 2016"----
ggplot(data_2016, aes(x = sst2_c, fill = .id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  theme_jjo()

## ---- fig.cap="Distribution of filtered `sst2` for the four individuals in 2016"----
data_2016_filter <- data_2016[sst2_c < 500, ]
ggplot(data_2016_filter, aes(x = sst2_c, fill = .id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  theme_jjo()

## -----------------------------------------------------------------------------
# nbrow removed
data_2016[sst2_c > 500, .(nb_row_removed = .N), by = .id] %>%
  sable(caption = "# of rows removed by 2016-individuals")

## ---- fig.cap="Where and when the `sst2` outliers occured", fig.width=9-------
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

## ---- fig.cap="It is supposed to be the track of `ind_3449`... (red dots are the location of removed rows)", fig.width=8----
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

## -----------------------------------------------------------------------------
# summary of the coordinates by individuals
data_2016[, .(.id, longitude_degs, latitude_degs)] %>%
  tbl_summary(by = .id) %>%
  modify_caption("Summary of `longitude_degree` and `latitude_degree`")

## ---- fig.width=9, fig.cap="Distribution of coordinates per seal"-------------
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

## ---- fig.cap="An attempt to display the `ind_3449`'s track", fig.width=8-----
# interactive map
leaflet() %>%
  setView(lng = -122, lat = 50, zoom = 3) %>%
  addTiles() %>%
  addPolylines(
    lat = data_2016[.id == "ind_3449", abs(latitude_degs)],
    lng = data_2016[.id == "ind_3449", -abs(longitude_degs)],
    weight = 2
  )

## ----fig.cap="Check for missing value in 2016-individuals", out.width="100%"----
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

## -----------------------------------------------------------------------------
# list structure
str(data_nes$year_2018, max.level = 1, give.attr = F, no.list = T)

## ---- eval=FALSE--------------------------------------------------------------
#  # combine all individuals
#  data_2018 <- rbindlist(data_nes$year_2018)
#  
#  # display
#  DT::datatable(data_2018[sample.int(.N, 100), ], options = list(scrollX = T))

## ---- echo=FALSE, results='asis'----------------------------------------------
# combine all individuals
data_2018 <- rbindlist(data_nes$year_2018)

# title
cat("<table style='width: 50%'>", paste0("<caption>", "(#tab:myDThtmltools)", "Sample of 100 random rows from `data_2018`", "</caption>"), "</table>", sep = "\n")

# display

DT::datatable(data_2018[sample.int(.N, 100), ], options = list(scrollX = T))

## -----------------------------------------------------------------------------
# raw_data
data_2018[, .(
  nb_days_recorded = uniqueN(as.Date(date)),
  nb_dives = .N,
  maxdepth_mean = mean(maxdepth),
  dduration_mean = mean(dduration),
  botttime_mean = mean(botttime),
  pdi_mean = mean(pdi, na.rm = T)
), by = .id] %>%
  sable(
    caption = "Summary diving information relative to each 2018 individual",
    digits = 2
  )

## ----fig.cap="Check for missing value in 2018-individuals", fig.width=9-------
# build dataset to check for missing values
dataPlot <- melt(data_2018[, .(.id, is.na(.SD)), .SDcol = -c(
  ".id",
  "divenumber",
  "year",
  "month",
  "day",
  "hour",
  "min",
  "sec",
  "juldate",
  "divetype",
  "date",
  "phase",
  "lat",
  "lon"
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

## -----------------------------------------------------------------------------
# table with percent
table_inter <- data_2018[, lapply(.SD, function(x) {
  round(length(x[is.na(x)]) * 100 / length(x), 1)
}), .SDcol = -c(
  ".id",
  "divenumber",
  "year",
  "month",
  "day",
  "hour",
  "min",
  "sec",
  "juldate",
  "divetype",
  "date",
  "phase",
  "lat",
  "lon"
)]

# find which are different from 0
cond_inter <- sapply(table_inter, function(x) {
  x == 0
})

# display the percentages that are over 0
table_inter[, which(cond_inter) := NULL] %>%
  sable(caption = "Percentage of missing values per columns having missing values!") %>%
  scroll_box(width = "100%")

## ---- fig.cap='Distribution of `dduration` for each seal. The dashed line highlight the "subjective" threshold used to remove outliers (3000 sec)', fig.height=3----
ggplot(
  data_2018[, .SD][, state := "Before"],
  aes(x = dduration, fill = .id)
) +
  geom_histogram(show.legend = FALSE) +
  geom_vline(xintercept = 3000, linetype = "longdash") +
  facet_grid(state ~ .id,
             scales = "free"
  ) +
  labs(y = "# of dives", x = "Dive duration (s)") +
  theme_jjo()

## ---- fig.cap='Same distribution of `dduration` for each seal but after removing any `dduration` > 3000 sec. The dashed line highlight the "subjective" threshold used to remove outliers', fig.height=3----
ggplot(
  data_2018[dduration < 3000, ][][, state := "After"],
  aes(x = dduration, fill = .id)
) +
  geom_histogram(show.legend = FALSE) +
  geom_vline(xintercept = 3000, linetype = "longdash") +
  facet_grid(state ~ .id,
             scales = "free"
  ) +
  labs(x = "# of dives", y = "Dive duration (s)") +
  theme_jjo()

## -----------------------------------------------------------------------------
# filter data
data_2018_filter <- data_2018[dduration < 3000, ]

# nbrow removed
data_2018[dduration >= 3000, .(nb_row_removed = .N), by = .id] %>%
  sable(caption = "# of rows removed by 2018-individuals")

## ---- fig.cap="Visualization of light level at the surface along 2018-individuals' trip", fig.height=6----
# let's first average `lightatsurf` by individuals, day since departure and hour
dataPlot <- data_2018[, .(lightatsurf = median(lightatsurf)),
                      by = .(.id, day_departure, date = as.Date(date), hour)
]

# display the result
ggplot(dataPlot, aes(x = day_departure, y = hour, fill = lightatsurf)) +
  geom_tile() +
  facet_grid(.id ~ .) +
  theme_jjo() +
  labs(x = "# of days since departure", y = "Hour", fill = "Light level at the surface") +
  theme(legend.position = c("bottom"))

## ---- fig.cap="Visualization of detected night time and day time along 2018-individuals' trip", fig.height=6----
# let's first average `lightatsurf` by individuals, day since departure and hour
dataPlot <- data_2018[, .(lightatsurf = median(lightatsurf)),
                      by = .(.id, day_departure, date = as.Date(date), hour, phase)
]

# display the result
ggplot(dataPlot, aes(x = day_departure, y = hour, fill = phase)) +
  geom_tile() +
  facet_grid(.id ~ .) +
  theme_jjo() +
  labs(x = "# of days since departure", y = "Hour", fill = "Day time and night time as detected by the `cal_phase_day` function") +
  theme(legend.position = c("bottom"))

## -----------------------------------------------------------------------------
names_display <- names(data_2018_filter[, -c(
  ".id",
  "date",
  "divenumber",
  "year",
  "month",
  "day",
  "hour",
  "min",
  "sec",
  "juldate",
  "divetype",
  "euphoticdepth",
  "thermoclinedepth",
  "day_departure",
  "phase",
  "lat",
  "lon"
)])

## ---- eval=FALSE, include=TRUE------------------------------------------------
#  for (i in names_display) {
#    cat("#####", i, "{.unlisted .unnumbered} \n")
#    if (i == "maxdepth") {
#      print(
#        ggplot() +
#          geom_point(
#            data = data_2018_filter[, .(
#              .id,
#              date,
#              thermoclinedepth
#            )],
#            aes(
#              x = as.Date(date),
#              y = -thermoclinedepth,
#              colour = "Thermocline (m)"
#            ),
#            alpha = .2,
#            size = .5
#          ) +
#          geom_point(
#            data = data_2018_filter[, .(
#              .id,
#              date,
#              euphoticdepth
#            )],
#            aes(
#              x = as.Date(date),
#              y = -euphoticdepth,
#              colour = "Euphotic (m)"
#            ),
#            alpha = .2,
#            size = .5
#          ) +
#          scale_colour_manual(
#            values = c(
#              "Thermocline (m)" = "red",
#              "Euphotic (m)" = "black"
#            ),
#            name = "Zone"
#          ) +
#          new_scale_color() +
#          geom_point(
#            data = melt(data_2018_filter[, .(.id, date, get(i))], id.vars = c(".id", "date")),
#            aes(
#              x = as.Date(date),
#              y = -value,
#              col = .id
#            ),
#            alpha = 1 / 10,
#            size = .5,
#            show.legend = FALSE
#          ) +
#          facet_wrap(. ~ .id, scales = "free") +
#          scale_x_date(date_labels = "%m/%Y") +
#          labs(x = "Date", y = "Maximum Depth (m)") +
#          theme_jjo() +
#          theme(
#            axis.text.x = element_text(angle = 45, hjust = 1),
#            legend.position = "bottom"
#          )
#      )
#      cat("<blockquote> Considering `ind_2018074` has slightly different values than other individuals for the thermocline depth, it would be interesting to see where the animal went. </blockquote>")
#    } else if (i == "driftrate") {
#      print(
#        ggplot(
#          data = melt(data_2018_filter[, .(.id, date, get(i), divetype)], id.vars = c(".id", "date", "divetype")),
#          aes(
#            x = as.Date(date),
#            y = value,
#            col = divetype
#          )
#        ) +
#          geom_point(
#            alpha = 1 / 10,
#            size = .5
#          ) +
#          facet_wrap(. ~ .id, scales = "free") +
#          scale_x_date(date_labels = "%m/%Y") +
#          labs(x = "Date", y = "Drift Rate 'm/s", col = "Dive Type") +
#          theme_jjo() +
#          theme(
#            axis.text.x = element_text(angle = 45, hjust = 1),
#            legend.position = "bottom"
#          ) +
#          guides(colour = guide_legend(override.aes = list(
#            size = 7,
#            alpha = 1
#          )))
#      )
#    } else {
#      print(
#        ggplot(
#          data = melt(data_2018_filter[, .(.id, date, get(i))], id.vars = c(".id", "date")),
#          aes(
#            x = as.Date(date),
#            y = value,
#            col = .id
#          )
#        ) +
#          geom_point(
#            show.legend = FALSE,
#            alpha = 1 / 10,
#            size = .5
#          ) +
#          facet_wrap(. ~ .id, scales = "free") +
#          scale_x_date(date_labels = "%m/%Y") +
#          labs(x = "Date", y = i) +
#          theme_jjo() +
#          theme(axis.text.x = element_text(angle = 45, hjust = 1))
#      )
#    }
#  
#    cat("\n \n")
#  }

## ---- results='asis', cache=TRUE, echo=FALSE----------------------------------
for (i in names_display) {
  cat("#####", i, "{.unlisted .unnumbered} \n")
  if (i == "maxdepth") {
    print(
      ggplot() +
        geom_point(
          data = data_2018_filter[, .(
            .id,
            date,
            thermoclinedepth
          )],
          aes(
            x = as.Date(date),
            y = -thermoclinedepth,
            colour = "Thermocline (m)"
          ),
          alpha = .2,
          size = .5
        ) +
        geom_point(
          data = data_2018_filter[, .(
            .id,
            date,
            euphoticdepth
          )],
          aes(
            x = as.Date(date),
            y = -euphoticdepth,
            colour = "Euphotic (m)"
          ),
          alpha = .2,
          size = .5
        ) +
        scale_colour_manual(
          values = c(
            "Thermocline (m)" = "red",
            "Euphotic (m)" = "black"
          ),
          name = "Zone"
        ) +
        new_scale_color() +
        geom_point(
          data = melt(data_2018_filter[, .(.id, date, get(i))], id.vars = c(".id", "date")),
          aes(
            x = as.Date(date),
            y = -value,
            col = .id
          ),
          alpha = 1 / 10,
          size = .5,
          show.legend = FALSE
        ) +
        facet_wrap(. ~ .id, scales = "free") +
        scale_x_date(date_labels = "%m/%Y") +
        labs(x = "Date", y = "Maximum Depth (m)") +
        theme_jjo() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        )
    )
    cat("<blockquote> Considering `ind_2018074` has slightly different values than other individuals for the thermocline depth, it would be interesting to see where the animal went. </blockquote>")
  } else if (i == "driftrate") {
    print(
      ggplot(
        data = melt(data_2018_filter[, .(.id, date, get(i), divetype)], id.vars = c(".id", "date", "divetype")),
        aes(
          x = as.Date(date),
          y = value,
          col = divetype
        )
      ) +
        geom_point(
          alpha = 1 / 10,
          size = .5
        ) +
        facet_wrap(. ~ .id, scales = "free") +
        scale_x_date(date_labels = "%m/%Y") +
        labs(x = "Date", y = "Drift Rate 'm/s", col = "Dive Type") +
        theme_jjo() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        ) +
        guides(colour = guide_legend(override.aes = list(
          size = 7,
          alpha = 1
        )))
    )
  } else {
    print(
      ggplot(
        data = melt(data_2018_filter[, .(.id, date, get(i))], id.vars = c(".id", "date")),
        aes(
          x = as.Date(date),
          y = value,
          col = .id
        )
      ) +
        geom_point(
          show.legend = FALSE,
          alpha = 1 / 10,
          size = .5
        ) +
        facet_wrap(. ~ .id, scales = "free") +
        scale_x_date(date_labels = "%m/%Y") +
        labs(x = "Date", y = i) +
        theme_jjo() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
  
  cat("\n \n")
}

## ---- eval=FALSE, include=TRUE------------------------------------------------
#  # same plot with a colored for the phase of the day
#  for (i in names_display) {
#    cat("#####", i, "{-} \n")
#    print(
#      ggplot(
#        data = melt(data_2018_filter[, .(.id, date, get(i), phase)],
#                    id.vars = c(
#                      ".id",
#                      "date",
#                      "phase"
#                    )
#        ),
#        aes(
#          x = as.Date(date),
#          y = value,
#          col = phase
#        )
#      ) +
#        geom_point(
#          alpha = 1 / 10,
#          size = .5
#        ) +
#        facet_wrap(. ~ .id, scales = "free") +
#        scale_x_date(date_labels = "%m/%Y") +
#        labs(x = "Date", y = i) +
#        theme_jjo() +
#        theme(
#          axis.text.x = element_text(angle = 45, hjust = 1),
#          legend.position = "bottom"
#        ) +
#        guides(colour = guide_legend(override.aes = list(
#          size = 7,
#          alpha = 1
#        )))
#    )
#    cat("\n \n")
#  }

## ---- results='asis', cache=TRUE, echo=FALSE----------------------------------
# same plot with a colored for the phase of the day
for (i in names_display) {
  cat("#####", i, "{-} \n")
  print(
    ggplot(
      data = melt(data_2018_filter[, .(.id, date, get(i), phase)],
                  id.vars = c(
                    ".id",
                    "date",
                    "phase"
                  )
      ),
      aes(
        x = as.Date(date),
        y = value,
        col = phase
      )
    ) +
      geom_point(
        alpha = 1 / 10,
        size = .5
      ) +
      facet_wrap(. ~ .id, scales = "free") +
      scale_x_date(date_labels = "%m/%Y") +
      labs(x = "Date", y = i) +
      theme_jjo() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      guides(colour = guide_legend(override.aes = list(
        size = 7,
        alpha = 1
      )))
  )
  cat("\n \n")
}

## ---- eval=FALSE, include=TRUE------------------------------------------------
#  for (i in names_display) {
#    cat("#####", i, "{.unlisted .unnumbered} \n")
#    if (i == "maxdepth") {
#      print(
#        ggplot() +
#          geom_point(
#            data = data_2018_filter[day_departure < 32, .(
#              .id,
#              day_departure,
#              thermoclinedepth
#            )],
#            aes(
#              x = day_departure,
#              y = -thermoclinedepth,
#              colour = "Thermocline (m)",
#              group = day_departure
#            ),
#            alpha = .2,
#            size = .5
#          ) +
#          geom_point(
#            data = data_2018_filter[day_departure < 32, .(
#              .id,
#              day_departure,
#              euphoticdepth
#            )],
#            aes(
#              x = day_departure,
#              y = -euphoticdepth,
#              colour = "Euphotic (m)",
#              group = day_departure
#            ),
#            alpha = .2,
#            size = .5
#          ) +
#          scale_colour_manual(
#            values = c(
#              "Thermocline (m)" = "red",
#              "Euphotic (m)" = "black"
#            ),
#            name = "Zone"
#          ) +
#          new_scale_color() +
#          geom_boxplot(
#            data = melt(data_2018_filter[day_departure < 32, .(.id, day_departure, get(i))], id.vars = c(".id", "day_departure")),
#            aes(
#              x = day_departure,
#              y = -value,
#              col = .id,
#              group = day_departure
#            ),
#            alpha = 1 / 10,
#            size = .5,
#            show.legend = FALSE
#          ) +
#          facet_wrap(. ~ .id, scales = "free") +
#          labs(x = "# days since departure", y = "Maximum Depth (m)") +
#          theme_jjo() +
#          theme(legend.position = "bottom")
#      )
#    } else {
#      print(
#        ggplot(
#          data = melt(data_2018_filter[day_departure < 32, .(.id, day_departure, get(i))], id.vars = c(".id", "day_departure")),
#          aes(
#            x = day_departure,
#            y = value,
#            color = .id,
#            group = day_departure
#          )
#        ) +
#          geom_boxplot(
#            show.legend = FALSE,
#            alpha = 1 / 10,
#            size = .5
#          ) +
#          facet_wrap(. ~ .id, scales = "free") +
#          labs(x = "# days since departure", y = i) +
#          theme_jjo()
#      )
#    }
#    cat("\n \n")
#  }

## ---- results='asis', cache=TRUE, echo=FALSE----------------------------------
for (i in names_display) {
  cat("#####", i, "{.unlisted .unnumbered} \n")
  if (i == "maxdepth") {
    print(
      ggplot() +
        geom_point(
          data = data_2018_filter[day_departure < 32, .(
            .id,
            day_departure,
            thermoclinedepth
          )],
          aes(
            x = day_departure,
            y = -thermoclinedepth,
            colour = "Thermocline (m)",
            group = day_departure
          ),
          alpha = .2,
          size = .5
        ) +
        geom_point(
          data = data_2018_filter[day_departure < 32, .(
            .id,
            day_departure,
            euphoticdepth
          )],
          aes(
            x = day_departure,
            y = -euphoticdepth,
            colour = "Euphotic (m)",
            group = day_departure
          ),
          alpha = .2,
          size = .5
        ) +
        scale_colour_manual(
          values = c(
            "Thermocline (m)" = "red",
            "Euphotic (m)" = "black"
          ),
          name = "Zone"
        ) +
        new_scale_color() +
        geom_boxplot(
          data = melt(data_2018_filter[day_departure < 32, .(.id, day_departure, get(i))], id.vars = c(".id", "day_departure")),
          aes(
            x = day_departure,
            y = -value,
            col = .id,
            group = day_departure
          ),
          alpha = 1 / 10,
          size = .5,
          show.legend = FALSE
        ) +
        facet_wrap(. ~ .id, scales = "free") +
        labs(x = "# days since departure", y = "Maximum Depth (m)") +
        theme_jjo() +
        theme(legend.position = "bottom")
    )
  } else {
    print(
      ggplot(
        data = melt(data_2018_filter[day_departure < 32, .(.id, day_departure, get(i))], id.vars = c(".id", "day_departure")),
        aes(
          x = day_departure,
          y = value,
          color = .id,
          group = day_departure
        )
      ) +
        geom_boxplot(
          show.legend = FALSE,
          alpha = 1 / 10,
          size = .5
        ) +
        facet_wrap(. ~ .id, scales = "free") +
        labs(x = "# days since departure", y = i) +
        theme_jjo()
    )
  }
  cat("\n \n")
}

## ---- eval=FALSE, include=TRUE------------------------------------------------
#  for (i in names_display) {
#    cat("#####", i, "{.unlisted .unnumbered} \n")
#    print(
#      ggplot(
#        data = melt(data_2018_filter[
#          day_departure < 32,
#          .(.id, day_departure, get(i), phase)
#        ],
#        id.vars = c(".id", "day_departure", "phase")
#        ),
#        aes(
#          x = day_departure,
#          y = value,
#          color = phase,
#          group = interaction(day_departure, phase),
#        )
#      ) +
#        geom_boxplot(
#          alpha = 1 / 10,
#          size = .5
#        ) +
#        facet_wrap(. ~ .id, scales = "free") +
#        labs(x = "# days since departure", y = i) +
#        theme_jjo() +
#        theme(legend.position = "bottom")
#    )
#    cat("\n \n")
#  }

## ---- results='asis', cache=TRUE, echo=FALSE----------------------------------
for (i in names_display) {
  cat("#####", i, "{.unlisted .unnumbered} \n")
  print(
    ggplot(
      data = melt(data_2018_filter[
        day_departure < 32,
        .(.id, day_departure, get(i), phase)
      ],
      id.vars = c(".id", "day_departure", "phase")
      ),
      aes(
        x = day_departure,
        y = value,
        color = phase,
        group = interaction(day_departure, phase),
      )
    ) +
      geom_boxplot(
        alpha = 1 / 10,
        size = .5
      ) +
      facet_wrap(. ~ .id, scales = "free") +
      labs(x = "# days since departure", y = i) +
      theme_jjo() +
      theme(legend.position = "bottom")
  )
  cat("\n \n")
}

## ---- fig.cap="Correlation matrix (crosses indicate non significant correlation)", fig.width=10, fig.height=10----
# compute correlation
corr_2018 <- round(cor(data_2018_filter[, names_display, with = F],
                       use = "pairwise.complete.obs"
), 1)

# replace NA value by 0
corr_2018[is.na(corr_2018)] <- 0

# compute p_values
corr_p_2018 <- cor_pmat(data_2018_filter[, names_display, with = F])

# replace NA value by 0
corr_p_2018[is.na(corr_p_2018)] <- 1

# display
ggcorrplot(
  corr_2018,
  p.mat = corr_p_2018,
  hc.order = TRUE,
  method = "circle",
  type = "lower",
  ggtheme = theme_jjo(),
  sig.level = 0.05,
  colors =  c("#00AFBB", "#E7B800", "#FC4E07")
)

## -----------------------------------------------------------------------------
# flatten correlation matrix
cor_result_2018 <- flat_cor_mat(corr_2018, corr_p_2018)

# keep only the one above .7
cor_result_2018[cor >= .7, ][order(-abs(cor))] %>%
  sable(caption = "Pairwise correlation above 0.75 and associated p-values")

## ---- fig.cap="Proportion dive types"-----------------------------------------
# dataset to plot proportional area plot
data_2018_filter[, sum_id := .N, by = .(.id, day_departure)][, sum_id_days := .N, by = .(.id, day_departure, divetype)][, prop := sum_id_days /
                                                                                                                          sum_id]
dataPlot <- unique(data_2018_filter[, .(prop, .id, divetype, day_departure)])

# area plot
ggplot(dataPlot, aes(
  x = as.numeric(day_departure),
  y = prop,
  fill = as.character(divetype)
)) +
  geom_area(alpha = 0.6, size = 1) +
  facet_wrap(.id ~ ., scales = "free") +
  theme_jjo() +
  theme(legend.position = "bottom") +
  labs(x = "# of days since departure", y = "Proportion of dives", fill = "Dive types")

## ---- fig.cap="Dive duration vs. Maximum Depth colored 2018-individuals"------
# plot
ggplot(data = data_2018_filter, aes(y = dduration, x = maxdepth, col = .id)) +
  geom_point(size = .5, alpha = .1, show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  labs(x = "Maximum depth (m)", y = "Dive duration (s)") +
  theme_jjo()

## ---- fig.cap="Dive duration vs. Maximum Depth colored by Dive Type"----------
# plot
ggplot(data = data_2018_filter, aes(y = dduration, x = maxdepth, col = divetype)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  labs(x = "Maximum depth (m)", y = "Dive duration (s)") +
  theme_jjo() +
  theme(legend.position = "bottom")

## ---- fig.cap="Dive duration vs. Maximum Depth colored by # days since departure"----
# plot
ggplot(data = data_2018_filter[, prop_track := (day_departure * 100) / max(day_departure), by = .id], aes(y = dduration, x = maxdepth, col = prop_track)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  labs(x = "Maximum depth (m)", y = "Dive duration (s)", col = "Proportion of completed track (%)") +
  scale_color_continuous(type = "viridis") +
  theme_jjo() +
  theme(legend.position = "bottom")

## ---- fig.cap="Dive duration vs. Maximum Depth colored by phases of the day"----
# plot
ggplot(data = data_2018_filter, aes(y = dduration, x = maxdepth, col = phase)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  labs(x = "Maximum depth (m)", y = "Dive duration (s)", col = "Phases of the day") +
  theme_jjo() +
  theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# build dataset
dataPlot <- data_2018_filter[divetype == "2: drift",
                             # median drift rate for drift dive
                             .(driftrate = median(driftrate, na.rm = T)),
                             by = .(.id, day_departure)
][data_2018_filter[,
                   .(
                     # median dive duration all dives considered
                     dduration = median(dduration, na.rm = T),
                     # median max depth all dives considered
                     maxdepth = median(maxdepth, na.rm = T),
                     # median bottom dives all dives considered
                     botttime = median(botttime, na.rm = T)
                   ),
                   by = .(.id, day_departure)
],
on = c(".id", "day_departure")
]

## ---- fig.cap="Drift rate vs. Bottom time"------------------------------------
# plot
ggplot(dataPlot, aes(x = botttime, y = driftrate, col = .id)) +
  geom_point(size = .5, alpha = .5) +
  geom_smooth(method = "lm") +
  guides(color = "none") +
  facet_wrap(.id ~ .) +
  scale_x_continuous(limits = c(0, 700)) +
  labs(x = "Daily median Bottom time (s)", y = "Daily median drift rate (m.s-1)") +
  theme_jjo()

## ---- fig.cap="Drift rate vs. Maximum depth"----------------------------------
# plot
ggplot(dataPlot, aes(x = maxdepth, y = driftrate, col = .id)) +
  geom_point(size = .5, alpha = .5) +
  geom_smooth(method = "lm") +
  guides(color = "none") +
  facet_wrap(.id ~ .) +
  labs(x = "Daily median Maximum depth (m)", y = "Daily median drift rate (m.s-1)") +
  theme_jjo()

## ---- fig.cap="Drift rate vs. Dive duration"----------------------------------
# plot
ggplot(dataPlot, aes(x = dduration, y = driftrate, col = .id)) +
  geom_point(size = .5, alpha = .5) +
  geom_smooth(method = "lm") +
  guides(color = "none") +
  facet_wrap(.id ~ .) +
  labs(x = "Daily median Dive duration (s)", y = "Daily median drift rate (m.s-1)") +
  theme_jjo()

## ---- fig.cap="Post-dive duration vs. dive duration"--------------------------
# dive duration vs pdi by days
ggplot(data = data_2018_filter[pdi < 300, ], aes(
  x = dduration,
  y = pdi,
  color = .id,
  group = dduration,
  fill = "none"
)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.05, alpha = 0) +
  labs(x = "Dive duration (s)", y = "Post-dive duration (s)") +
  facet_wrap(. ~ .id, scales = "free_x") +
  theme_jjo()

## ---- fig.cap="Post-dive duration vs. dive duration (raw data)"---------------
# dive duration vs pdi by days
ggplot(data = data_2018_filter[pdi < 300,], aes(x = dduration, 
                                                y = pdi, 
                                                color = .id)) +
  geom_point(show.legend = FALSE, alpha = 0.05) +
  geom_smooth(
    method = "gam",
    show.legend = FALSE,
    col = "black",
    linetype = "dashed"
  ) +
  labs(x = "Dive duration (s)", y = "Post-dive duration (s)") +
  facet_wrap(. ~ .id, scales = "free_x") +
  theme_jjo()

## ---- fig.cap="Post-dive duration / dive duration ratio vs. day since departure"----
# dive duration vs pdi by days
ggplot(data = data_2018_filter[pdi < 300, .(.id, pdi_ratio = pdi / dduration, day_departure)], aes(
  x = day_departure,
  y = pdi_ratio,
  color = .id,
  group = day_departure,
  fill = "none"
)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.05, alpha = 0) +
  labs(x = "# days since departure", y = "Post-dive / Dive duration ratio") +
  facet_wrap(. ~ .id, scales = "free_x") +
  # zoom
  coord_cartesian(ylim=c(0,0.4)) +
  theme_jjo()

## ---- fig.cap="Distribution of the number of dives each day. The threshold used to calculate bADL is fixed at 50 dives per day.", fig.height=3----
ggplot(data_2018_filter[,.(nb_dives = .N), 
                        by = .(.id, day_departure)], 
       aes(x=nb_dives, fill=.id)) +
  geom_histogram(show.legend = FALSE) + 
  facet_grid(.~.id) +
  labs(y="# of days", x = "# of dives per day") +
  theme_jjo()

## ---- fig.cap="Behavioral ADL vs. drift rate along animals' trip (Am I the only one seeing some kind of relationship?)"----
# select day that have at least 50 dives
days_to_keep = data_2018_filter[,
                                .(nb_dives = .N),
                                by = .(.id, day_departure)] %>%
  .[nb_dives >= 50,]

# keep only those days
data_2018_filter_complete_day = merge(data_2018_filter,
                                      days_to_keep,
                                      by = c(".id", "day_departure"))

# data plot
dataPlot = data_2018_filter_complete_day[,
                                         .(badl = quantile(dduration, 0.95)),
                                         by = .(.id, day_departure)]

# combine two datasets to be able to use a second axis
# https://stackoverflow.com/questions/49185583/two-y-axes-with-different-scales-for-two-datasets-in-ggplot2
dataMegaPlot = rbind(data_2018_filter_complete_day[divetype == "2: drift"] %>%
                       .[, .(w = .id,
                             y = driftrate,
                             x = day_departure,
                             z = "second_plot")],
                     dataPlot[, .(
                       w = .id,
                       # tricky one
                       y = (badl / 1000) - 1,
                       x = day_departure,
                       z = "first_plot"
                     )])

# plot
ggplot() +
  geom_point(
    data = dataMegaPlot[z == "second_plot", ],
    aes(x = x, y = y),
    alpha = 1 / 10,
    size = 0.5,
    color = "grey40",
    show.legend = FALSE
  ) +
  geom_path(data = dataMegaPlot[z == "first_plot", ],
            aes(x = x, y = y, color = w),
            show.legend = FALSE) +
  scale_y_continuous(
    # Features of the first axis
    name = "Drift rate (m/s)",
    # Add a second axis and specify its features
    sec.axis = sec_axis( ~ (. * 1000) + 1000, name = "Behavioral Aerobic Dive Limit (s)")
  ) +
  labs(x = "# days since departure") +
  facet_wrap(w ~ .) +
  theme_jjo()

## -----------------------------------------------------------------------------
# get badl
dataplot_1 = data_2018_filter_complete_day[,
                              .(badl = quantile(dduration, 0.95)),
                              by = .(.id, day_departure)]
# get driftrate
dataplot_2 = data_2018_filter_complete_day[divetype == "2: drift",
                              .(driftrate = median(driftrate)),
                              by = .(.id, day_departure)]

# merge
dataPlot = merge(dataplot_1,
                 dataplot_2,
                 by = c(".id", "day_departure"),
                 all = TRUE)

# plot
ggplot(data = dataPlot, aes(x = badl, y = driftrate, col = .id)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(.id~., scales = "free") +
  theme_jjo()

## -----------------------------------------------------------------------------
# ind_2018070
plot_ly(
  x = dataPlot[.id == "ind_2018070", badl],
  y = dataPlot[.id == "ind_2018070", day_departure],
  z = dataPlot[.id == "ind_2018070", driftrate],
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2),
  color = dataPlot[.id == "ind_2018070", day_departure]
) %>% 
  layout(scene = list(xaxis = list(title = 'Behavioral ADL'),
                      yaxis = list(title = '# days since departure'),
                      zaxis = list(title = 'Drift rate (m/s)')))

## -----------------------------------------------------------------------------
# ind_2018072
plot_ly(
  x = dataPlot[.id == "ind_2018072", badl],
  y = dataPlot[.id == "ind_2018072", day_departure],
  z = dataPlot[.id == "ind_2018072", driftrate],
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2),
  color = dataPlot[.id == "ind_2018072", day_departure]
) %>% 
  layout(scene = list(xaxis = list(title = 'Behavioral ADL'),
                      yaxis = list(title = '# days since departure'),
                      zaxis = list(title = 'Drift rate (m/s)')))

## -----------------------------------------------------------------------------
# ind_2018074
plot_ly(
  x = dataPlot[.id == "ind_2018074", badl],
  y = dataPlot[.id == "ind_2018074", day_departure],
  z = dataPlot[.id == "ind_2018074", driftrate],
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2),
  color = dataPlot[.id == "ind_2018074", day_departure]
) %>% 
  layout(scene = list(xaxis = list(title = 'Behavioral ADL'),
                      yaxis = list(title = '# days since departure'),
                      zaxis = list(title = 'Drift rate (m/s)')))

## -----------------------------------------------------------------------------
# ind_2018080
plot_ly(
  x = dataPlot[.id == "ind_2018080", badl],
  y = dataPlot[.id == "ind_2018080", day_departure],
  z = dataPlot[.id == "ind_2018080", driftrate],
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2),
  color = dataPlot[.id == "ind_2018080", day_departure]
) %>% 
  layout(scene = list(xaxis = list(title = 'Behavioral ADL'),
                      yaxis = list(title = '# days since departure'),
                      zaxis = list(title = 'Drift rate (m/s)')))

## ---- fig.cap="Map with polylines", fig.width=8, eval=FALSE-------------------
#  # This piece of code is only there to show how to draw a polylines with a gradient color using leaflet.
#  # We're not using it due to the size of the created map, and will continue using circle marker
#  
#  # datasets used to display map
#  df_driftrate = unique(data_2018_filter[.id == "ind_2018070" &
#                                           divetype == "2: drift",
#                                         .(.id, lat, lon, dduration)])
#  
#  # color palette
#  pal <- colorNumeric(
#    palette = "YlGnBu",
#    domain = df_driftrate$dduration
#  )
#  
#  # add
#  df_driftrate[, `:=`(nextLat = shift(lat),
#                      nextLon = shift(lon),
#                      color = pal(df_driftrate$dduration))]
#  
#  # interactive map
#  gradient_map <- leaflet() %>%
#    setView(lng = -122, lat = 38, zoom = 2) %>%
#    addTiles()
#  
#  # add lines
#  for (i in 1:nrow(df_driftrate)) {
#    gradient_map <- addPolylines(
#      map = gradient_map,
#      data = df_driftrate,
#      lat = as.numeric(df_driftrate[i, c('lat', 'nextLat')]),
#      lng = as.numeric(df_driftrate[i, c('lon', 'nextLon')]),
#      color = df_driftrate[i, color],
#      weight = 3,
#      group = "individual_1"
#    )
#  }
#  
#  # add layer control
#  gradient_map <- addLayersControl(
#    map = gradient_map,
#    overlayGroups = c("individual_1"),
#    options = layersControlOptions(collapsed = FALSE)
#  )
#  
#  # format(object.size(gradient_map), units = "Mb")

## -----------------------------------------------------------------------------
# interactive map
gradient_map <- leaflet() %>%
  setView(lng = -132, lat = 48, zoom = 4) %>%
  addTiles()

# loop by individuals and variable
grps = NULL
for (i in seq(data_2018_filter[!is.na(lat),unique(.id)])){
  for (k in c("dduration","maxdepth","efficiency", "driftrate")){
    if (k == "driftrate"){
      # set dataset used to plot
      dataPlot = unique(data_2018_filter %>% 
                          .[order(date),] %>% 
                          .[.id==data_2018_filter[!is.na(lat), unique(.id)][i] &
                              divetype == "2: drift" &
                              !is.na(get(k)), 
                            c("lat", "lon", k), with=FALSE] %>% 
                          .[!is_outlier(get(k)),])
      # color palette creation
      colPal <- colorNumeric(
        palette = "BrBG",
        domain = seq(-dataPlot[,max(abs(driftrate))],
                     dataPlot[,max(abs(driftrate))],
                     0.1)
      )
    } else {
      # set dataset used to plot
      dataPlot = unique(data_2018_filter %>% 
                          .[order(date),] %>% 
                          .[.id==data_2018_filter[!is.na(lat), unique(.id)][i] & 
                              divetype != "2: drift" &
                              !is.na(get(k)), 
                            c("lat", "lon", k), with=FALSE] %>% 
                          .[!is_outlier(get(k)),]) 
      # color palette creation
      colPal <- colorNumeric(
        palette = "YlGnBu",
        domain = dataPlot[,get(k)]
      )
    }
    
    # add color to dataset
    dataPlot[, color := colPal(dataPlot[,get(k)])]
    # add size column
    dataPlot[, radius := 3]
    # mark the beginning of the trip
    dataPlot[1, `:=` (color = "green",
                      radius = 4)]
    # mark the end of the trip
    dataPlot[.N, `:=` (color = "red",
                       radius = 4)]
    # reorder to make the end and the beginning in front
    dataPlot = rbind(dataPlot[-1,],dataPlot[1,])
    # add markers to map
    gradient_map <- addCircleMarkers(
      map = gradient_map,
      data = dataPlot,
      lat = ~lat,
      lng = ~lon,
      radius = ~radius,
      stroke = FALSE,
      color = ~color,
      fillOpacity = 1,
      group = paste(data_2018_filter[,unique(.id)][i], "-", k)
    ) %>% 
      addLegend("bottomleft", 
                data = dataPlot,
                group = paste(data_2018_filter[,unique(.id)][i], "-", k),
                pal = colPal, 
                values = ~get(k),
                title = k,
                opacity = 1
      )
    # retrieve groups
    grps = c(grps, paste(data_2018_filter[,unique(.id)][i], "-", k))
  }
}

# add layer control
gradient_map <- addLayersControl(
  map = gradient_map,
  overlayGroups = grps,
  options = layersControlOptions(collapsed = TRUE)
) %>% hideGroup(grps)

## ---- fig.cap="Tracking data 2018 individuals (green and red dot respectively indicate the beginning and the end of each trip)", fig.width=8----
# display
gradient_map

