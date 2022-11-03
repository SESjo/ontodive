## ----setup, include=FALSE---------------------------------------------------------------
# command to build package without getting vignette error
# https://github.com/rstudio/renv/issues/833
# devtools::check(build_args=c("--no-build-vignettes"))

# reduce png size
knitr::knit_hooks$set(optipng = knitr::hook_optipng)
knitr::knit_hooks$set(pngquant = knitr::hook_pngquant)

# global option relative to rmarkdown
knitr::opts_chunk$set(
  cache = TRUE,
  echo = TRUE,
  fig.align = "center",
  out.width = "100%",
  message = FALSE,
  warning = FALSE,
  # tidy = TRUE,
  cache.lazy = FALSE,
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
library(tidync)
library(gganimate)
library(transformr)
library(magick)
library(gifski)

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

## ----data-exploration-2018-13-----------------------------------------------------------
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
  "lon",
  "dist_dep",
  "sp"
)])

# calulate the median of driftrate for each day
median_driftrate <- data_2018[divetype == "2: drift",
  .(driftrate = quantile(driftrate, 0.5)),
  by = .(date = as.Date(date), .id)
]

# let's identity when the smooth changes sign
changes_driftrate <- median_driftrate %>%
  .[, .(
    y_smooth = predict(loess(driftrate ~ as.numeric(date), span = 0.25)),
    date
  ), by = .id] %>%
  .[c(FALSE, diff(sign(y_smooth)) != 0), ]

## ----data-exploration-2018-22, fig.cap="Correlation matrix (crosses indicate non significant correlation)", fig.width=10, fig.height=10----
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

## ----data-exploration-2018-23-----------------------------------------------------------
# flatten correlation matrix
cor_result_2018 <- flat_cor_mat(corr_2018, corr_p_2018)

# keep only the one above .7
cor_result_2018[cor >= .7, ][order(-abs(cor))] %>%
  sable(caption = "Pairwise correlation above 0.75 and associated p-values")

## ----data-exploration-2018-24, fig.cap="Proportion dive types"--------------------------
# dataset to plot proportional area plot
data_2018_filter[, sum_id := .N, by = .(.id, day_departure)] %>%
  .[, sum_id_days := .N, by = .(.id, day_departure, divetype)] %>%
  .[, prop := sum_id_days / sum_id]
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
  labs(x = "# of days since departure", 
       y = "Proportion of dives", 
       fill = "Dive types")

## ----data-exploration-2018-25, fig.cap="Dive duration vs. Maximum Depth colored 2018-individuals"----
# plot
ggplot(data = data_2018_filter, aes(y = dduration, x = maxdepth, col = .id)) +
  geom_point(size = .5, alpha = .1, show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  labs(x = "Maximum depth (m)", y = "Dive duration (s)") +
  theme_jjo()

## ----data-exploration-2018-26, fig.cap="Dive duration vs. Maximum Depth colored by Dive Type"----
# plot
ggplot(data = data_2018_filter, aes(y = dduration, 
                                    x = maxdepth, 
                                    col = divetype)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  labs(x = "Maximum depth (m)", y = "Dive duration (s)") +
  theme_jjo() +
  theme(legend.position = "bottom")

## ----data-exploration-2018-27, fig.cap="Dive duration vs. Maximum Depth colored by # days since departure"----
# plot
ggplot(data = data_2018_filter[, prop_track := (day_departure * 100) / max(day_departure), by = .id], 
       aes(y = dduration, x = maxdepth, col = prop_track)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  labs(x = "Maximum depth (m)", 
       y = "Dive duration (s)", 
       col = "Proportion of completed track (%)") +
  scale_color_continuous(type = "viridis") +
  theme_jjo() +
  theme(legend.position = "bottom")

## ----data-exploration-2018-28, fig.cap="Dive duration vs. Maximum Depth colored by phases of the day"----
# plot
ggplot(data = data_2018_filter, aes(y = dduration, x = maxdepth, col = phase)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  labs(x = "Maximum depth (m)", 
       y = "Dive duration (s)", 
       col = "Phases of the day") +
  theme_jjo() +
  theme(legend.position = "bottom")

## ----data-exploration-2018-29-----------------------------------------------------------
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

## ----data-exploration-2018-30, fig.cap="Drift rate vs. Bottom time"---------------------
# plot
ggplot(dataPlot, aes(x = botttime, y = driftrate, col = .id)) +
  geom_point(size = .5, alpha = .5) +
  geom_smooth(method = "lm") +
  guides(color = "none") +
  facet_wrap(.id ~ .) +
  scale_x_continuous(limits = c(0, 700)) +
  labs(x = "Daily median Bottom time (s)", 
       y = "Daily median drift rate (m.s-1)") +
  theme_jjo()

## ----data-exploration-2018-31, fig.cap="Drift rate vs. Maximum depth"-------------------
# plot
ggplot(dataPlot, aes(x = maxdepth, y = driftrate, col = .id)) +
  geom_point(size = .5, alpha = .5) +
  geom_smooth(method = "lm") +
  guides(color = "none") +
  facet_wrap(.id ~ .) +
  labs(x = "Daily median Maximum depth (m)", 
       y = "Daily median drift rate (m.s-1)") +
  theme_jjo()

## ----data-exploration-2018-32, fig.cap="Drift rate vs. Dive duration"-------------------
# plot
ggplot(dataPlot, aes(x = dduration, y = driftrate, col = .id)) +
  geom_point(size = .5, alpha = .5) +
  geom_smooth(method = "lm") +
  guides(color = "none") +
  facet_wrap(.id ~ .) +
  labs(x = "Daily median Dive duration (s)", 
       y = "Daily median drift rate (m.s-1)") +
  theme_jjo()

## ----data-exploration-2018-33, fig.cap="Post-dive duration vs. dive duration"-----------
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

## ----data-exploration-2018-34, fig.cap="Post-dive duration vs. dive duration (raw data)"----
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

## ----data-exploration-2018-35, fig.cap="Post-dive duration / dive duration ratio vs. day since departure"----
# dive duration vs pdi by days
ggplot(
  data = data_2018_filter[pdi < 300, .(.id, pdi_ratio = pdi / dduration, day_departure)],
  aes(
    x = day_departure,
    y = pdi_ratio,
    color = .id,
    group = day_departure,
    fill = "none"
  )
) +
  geom_boxplot(show.legend = FALSE,
               outlier.alpha = 0.05,
               alpha = 0) +
  labs(x = "# days since departure", y = "Post-dive / Dive duration ratio") +
  facet_wrap(. ~ .id, scales = "free_x") +
  # zoom
  coord_cartesian(ylim = c(0, 0.4)) +
  theme_jjo()

## ----data-exploration-2018-36, fig.cap="Distribution of the number of dives each day. The threshold used to calculate bADL is fixed at 50 dives per day.", fig.height=3----
ggplot(data_2018_filter[,.(nb_dives = .N), 
                        by = .(.id, day_departure)], 
       aes(x=nb_dives, fill=.id)) +
  geom_histogram(show.legend = FALSE) + 
  facet_grid(.~.id) +
  labs(y="# of days", x = "# of dives per day") +
  theme_jjo()

## ----data-exploration-2018-37, fig.cap="Behavioral ADL vs. drift rate along animals' trip (Am I the only one seeing some kind of relationship?)"----
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
dataPlot = data_2018_filter_complete_day[divetype=="1: foraging",
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
    sec.axis = sec_axis( ~ (. * 1000) + 1000, 
                         name = "Behavioral Aerobic Dive Limit (s)")
  ) +
  labs(x = "# days since departure") +
  facet_wrap(w ~ .) +
  theme_jjo()

## ----data-exploration-2018-38-----------------------------------------------------------
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

## ----data-exploration-2018-39-----------------------------------------------------------
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

## ----data-exploration-2018-40-----------------------------------------------------------
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

## ----data-exploration-2018-41-----------------------------------------------------------
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

## ----data-exploration-2018-42-----------------------------------------------------------
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

## ---------------------------------------------------------------------------------------
# saving the data_2018_filter dataset
saveRDS(data_2018_filter, file = "tmp/data_2018_filter.rds")

