## ----setup, include=FALSE-----------------------------------------------------------------------------
# command to build package without getting vignette error
# https://github.com/rstudio/renv/issues/833
# devtools::check(build_args=c("--no-build-vignettes"))

# # reduce png size
# knitr::knit_hooks$set(optipng = knitr::hook_optipng)
# knitr::knit_hooks$set(pngquant = knitr::hook_pngquant)

# global option relative to rmarkdown
knitr::opts_chunk$set(
  echo = TRUE,
  fig.align = "center",
  out.width = "100%",
  message = FALSE,
  warning = FALSE,
  cache.lazy = FALSE,
  optipng = "-o7 -quiet",
  pngquant = "--speed=1"
)

# library
library(data.table)
library(ggplot2)
library(kableExtra)
library(corrplot)
library(magrittr)
library(ggpubr)
library(plotly)

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

## -----------------------------------------------------------------------------------------------------
# load wealingNES package
library(weanlingNES)

# load dataset
data("data_nes")
data("data_ses")

# merge into one dataset
data_comp <- rbind(
  rbindlist(data_nes$year_2018) %>%
    .[, sp := "nes"],
  rbindlist(data_ses$year_2014) %>%
    .[, sp := "ses"],
  use.names = T,
  fill = T
)

## ----fig.cap="Estimated temporal changes in maximum depth (m)"----------------------------------------
p1 <- plot_comp(data_comp[phase == "day", ], "maxdepth", nb_days = 300) +
  labs(
    x = "# days since departure",
    y = "Maximum Depth (m)",
    title = "Day"
  ) +
  scale_y_continuous(limits = c(0, data_comp[, max(maxdepth)])) +
  theme_jjo()
p2 <- plot_comp(data_comp[phase == "night", ], "maxdepth", nb_days = 300) +
  labs(
    x = "# days since departure",
    y = "Maximum Depth (m)",
    title = "Night"
  ) +
  scale_y_continuous(limits = c(0, data_comp[, max(maxdepth)])) +
  theme_jjo() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )
ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")

## ----fig.cap="Estimated temporal changes in dive duration (s)"----------------------------------------
p1 <- plot_comp(data_comp[phase == "day", ], "dduration", nb_days = 300) +
  labs(
    x = "# days since departure",
    y = "Dive Duration (s)",
    title = "Day"
  ) +
  scale_y_continuous(limits = c(0, data_comp[, quantile(dduration,0.99)])) +
  theme_jjo()
p2 <- plot_comp(data_comp[phase == "night", ], "dduration", nb_days = 200) +
  labs(
    x = "# days since departure",
    y = "Dive Duration (s)",
    title = "Night"
  ) +
  scale_y_continuous(limits = c(0, data_comp[, quantile(dduration,0.99)])) +
  theme_jjo() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )
ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")

## ----fig.cap="Estimated temporal changes in bADL (s)"-------------------------------------------------
# data nes
days_to_keep_nes = data_comp[sp=="nes",
                                .(nb_dives = .N),
                                by = .(.id, day_departure)] %>%
  .[nb_dives >= 50,]
# keep only those days
data_nes_complete_day = merge(data_comp[sp == "nes",],
                                      days_to_keep_nes,
                                      by = c(".id", "day_departure"))
# data ses
days_to_keep_ses = data_comp[sp=="ses",
                                .(nb_dives = .N),
                                by = .(.id, day_departure)] %>%
  .[nb_dives >= 8,]
# keep only those days
data_ses_complete_day = merge(data_comp[sp == "ses",],
                                      days_to_keep_ses,
                                      by = c(".id", "day_departure"))
# data plot
dataPlot = rbind(data_nes_complete_day[divetype=="1: foraging",
                                         .(badl = quantile(dduration, 0.95)),
                                         by = .(.id, day_departure, sp)],
                 data_ses_complete_day[divetype=="1: foraging",
                                         .(badl = quantile(dduration, 0.95)),
                                         by = .(.id, day_departure, sp)])

# comparative plot
plot_comp(dataPlot, "badl", nb_days = 300, alpha_point = .1) +
  labs(
    x = "# days since departure",
    y = "bADL (s)",
    title = "Day"
  ) +
  scale_y_continuous(limits = c(0, dataPlot[, quantile(badl,0.99)])) +
  theme_jjo()

## ----fig.cap="Estimated temporal changes in drift rate (m/s)"-----------------------------------------
# calculate drift rate per day, id and sp
dataPlot = data_comp[divetype == "2: drift",
  .(driftrate = quantile(driftrate, 0.5)),
  by = .(day_departure, .id, sp)
]

# comparative plot
p1 = plot_comp(dataPlot, "driftrate", nb_days = 300, alpha_point = .1) +
  labs(
    x = "# days since departure",
    y = "Drift Rate (m/s)",
    title = "Day"
  ) +
  theme_jjo() +
  theme(legend.position = "bottom")
p2 = ggplot(dataPlot, aes(x = day_departure, y = driftrate, col = .id)) +
  geom_point(show.legend = FALSE) +
  facet_grid(sp~.) +
  theme_jjo() +
  theme(axis.title.y = element_blank())
ggarrange(p1, p2, ncol = 2)

## ----fig.cap="Evolution of the maximum depth reached across time for the individual 2018070"----------
# let's pick an individual
data_test <- data_comp[.id == "ind_2018070", ]

# first we average `lightatsurf` by individuals, day since departure and hour
dataPlot <- data_test[, .(lightatsurf = median(lightatsurf, na.rm = T),
                          phase = first(phase)),
                      by = .(.id,
                             day_departure,
                             date = as.Date(date),
                             hour)]

# then we choose the variable to represent
i <- "maxdepth"
ggplot(data = melt(data_test[, .(.id, date, get(i), phase)],
                   id.vars = c(".id",
                               "date",
                               "phase")),
       aes(x = as.Date(date),
           y = value,
           col = phase)) +
  geom_point(alpha = 1 / 10,
             size = .5) +
  facet_wrap(. ~ .id, scales = "free") +
  scale_x_date(date_labels = "%m/%Y") +
  labs(x = "Date", y = i) +
  theme_jjo() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size = 7,
                                                   alpha = 1)))

## ----fig.cap="Maximum depth in function of the number of days since departure and the hour of the day for ind_2018070 (nes). The 2-D plane allow to identify day and night based on light level"----
# this is art!
htmltools::tagList(list(
plot_ly() %>%
  add_trace(
    data = data_test,
    x = ~hour,
    y = ~day_departure,
    z = ~ -maxdepth,
    marker = list(
      size = 1,
      opacity = 0.5
    ),
    mode = "markers",
    type = "scatter3d",
    text = ~divenumber
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~ (lightatsurf / 200) * 20,
    intensity = ~ lightatsurf / 200,
    data = dataPlot,
    type = "mesh3d",
    showlegend = FALSE
  ) %>%
  layout(
    scene = list(
      zaxis = list(title = "Maximum depth (m)"),
      yaxis = list(title = "# days since departure"),
      xaxis = list(title = "Hour")
    ),
    legend = list(itemsizing = "constant")
  )))

## ----fig.cap="Same graph but with `maxdepth` colored by `divetype`"-----------------------------------
# this is art!
htmltools::tagList(list(
  plot_ly() %>%
  add_trace(
    data = data_test,
    x = ~hour,
    y = ~day_departure,
    z = ~ -maxdepth,
    color = ~divetype,
    marker = list(
      size = 1,
      opacity = 0.5
    ),
    mode = "markers",
    type = "scatter3d",
    text = ~divenumber
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~ (lightatsurf / 200) * 20,
    intensity = ~ lightatsurf / 200,
    data = dataPlot,
    type = "mesh3d",
    showlegend = FALSE
  ) %>%
  layout(
    scene = list(
      zaxis = list(title = "Maximum depth (m)"),
      yaxis = list(title = "# days since departure"),
      xaxis = list(title = "Hour")
    ),
    legend = list(itemsizing = "constant")
  )
))

## ----fig.cap="Kind of the same graph, but without looking at the hour of the day"---------------------
ggplot(
  data = melt(data_test[, .(.id, date, get(i), divetype)],
              id.vars = c(
                ".id",
                "date",
                "divetype"
              )
  ),
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

## ----fig.cap="This is art!"---------------------------------------------------------------------------
htmltools::tagList(list(
  plot_ly() %>%
  add_trace(
    data = data_test,
    x = ~hour,
    y = ~day_departure,
    z = ~ -maxdepth,
    color = ~divetype,
    marker = list(
      size = 1,
      opacity = 0.5
    ),
    mode = "markers",
    type = "scatter3d",
    text = ~divenumber
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~ (lightatsurf / 200) * 20,
    intensity = ~ lightatsurf / 200,
    data = dataPlot,
    type = "mesh3d",
    showlegend = FALSE
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~bathy,
    data = data_test,
    type = "mesh3d"
  ) %>%
  layout(
    scene = list(
      zaxis = list(title = "Maximum depth (m)"),
      yaxis = list(title = "# days since departure"),
      xaxis = list(title = "Hour")
    ),
    legend = list(itemsizing = "constant")
  )))

## ----fig.cap="Evolution of the maximum depth reached across time for the individual 140059"-----------
# let's pick an individual
data_test <- data_comp[.id == "ind_140059",]

# first we average `lightatsurf` by individuals, day since departure and hour
dataPlot <- data_test[, .(lightatsurf = median(lightatsurf, na.rm = T),
                          phase = first(phase)),
                      by = .(.id,
                             day_departure,
                             date = as.Date(date),
                             hour)]

# then we choose the variable to represent
i <- "maxdepth"
ggplot(data = melt(data_test[, .(.id, date, get(i), phase)],
                   id.vars = c(".id",
                               "date",
                               "phase")),
       aes(x = as.Date(date),
           y = value,
           col = phase)) +
  geom_point(alpha = 1 / 5,
             size = .5) +
  facet_wrap(. ~ .id, scales = "free") +
  scale_x_date(date_labels = "%m/%Y") +
  labs(x = "Date", y = i) +
  theme_jjo() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size = 7,
                                                   alpha = 1)))

## ----fig.cap="Maximum depth in function of the number of days since departure and the hour of the day for ind_140059 (ses). The 2-D plane allow to identify day and night based on time and location."----
# this is art!
htmltools::tagList(list(
  plot_ly() %>%
  add_trace(
    data = data_test,
    x = ~hour,
    y = ~day_departure,
    z = ~ -maxdepth,
    marker = list(
      size = 1,
      opacity = 0.8
    ),
    mode = "markers",
    type = "scatter3d",
    text = ~divenumber
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~ (hour * 0.1) + 20,
    intensity = ~phase_bool,
    data = dataPlot[][, phase_bool := fifelse(phase == "night", 0, 1)][],
    type = "mesh3d",
    showlegend = FALSE
  ) %>%
  layout(
    scene = list(
      zaxis = list(title = "Maximum depth (m)"),
      yaxis = list(title = "# days since departure"),
      xaxis = list(title = "Hour")
    ),
    legend = list(itemsizing = "constant")
  )))

## ----fig.cap="Same graph but with `maxdepth` colored by `divetype`"-----------------------------------
# this is art!
htmltools::tagList(list(
  plot_ly() %>%
  add_trace(
    data = data_test,
    x = ~hour,
    y = ~day_departure,
    z = ~ -maxdepth,
    color = ~divetype,
    marker = list(
      size = 1,
      opacity = 0.8
    ),
    mode = "markers",
    type = "scatter3d",
    text = ~divenumber
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~ (hour * 0.1) + 20,
    intensity = ~phase_bool,
    data = dataPlot[][, phase_bool := fifelse(phase == "night", 0, 1)][],
    type = "mesh3d",
    showlegend = FALSE
  ) %>%
  layout(
    scene = list(
      zaxis = list(title = "Maximum depth (m)"),
      yaxis = list(title = "# days since departure"),
      xaxis = list(title = "Hour")
    ),
    legend = list(itemsizing = "constant")
  )
))

## ----fig.cap="Kind of the same graph, but without looking at the hour of the day"---------------------
ggplot(
  data = melt(data_test[, .(.id, date, get(i), divetype)],
              id.vars = c(
                ".id",
                "date",
                "divetype"
              )
  ),
  aes(
    x = as.Date(date),
    y = value,
    col = divetype
  )
) +
  geom_point(
    alpha = 1 / 5,
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

## ----fig.cap="This is art!"---------------------------------------------------------------------------
htmltools::tagList(list(plot_ly() %>%
  add_trace(
    data = data_test,
    x = ~hour,
    y = ~day_departure,
    z = ~ -maxdepth,
    color = ~divetype,
    marker = list(
      size = 1,
      opacity = 0.8
    ),
    mode = "markers",
    type = "scatter3d",
    text = ~divenumber
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~ (lightatsurf / 200) * 20,
    intensity = ~ lightatsurf / 200,
    data = dataPlot,
    type = "mesh3d",
    showlegend = FALSE
  ) %>%
  add_trace(
    x = ~hour,
    y = ~day_departure,
    z = ~bathy,
    data = data_test,
    type = "mesh3d"
  ) %>%
  layout(
    scene = list(
      zaxis = list(title = "Maximum depth (m)"),
      yaxis = list(title = "# days since departure"),
      xaxis = list(title = "Hour")
    ),
    legend = list(itemsizing = "constant")
  )))

