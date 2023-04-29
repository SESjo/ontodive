## ----setup, include=FALSE-----------------------------------------------------------------
# command to build package without getting vignette error
# https://github.com/rstudio/renv/issues/833
# devtools::check(build_args=c("--no-build-vignettes"))

# # reduce png size
# knitr::knit_hooks$set(optipng = knitr::hook_optipng)
# knitr::knit_hooks$set(pngquant = knitr::hook_pngquant)

# global option relative to rmarkdown
knitr::opts_chunk$set(
  cache = FALSE,
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
library(knitr)
library(kableExtra)
library(dplyr)
library(magrittr)
library(gganimate)
library(ontodive)

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

## ----netcdf-read-1------------------------------------------------------------------------
# first load `tidync` package
library(tidync)

# then import your file
df_nc <- tidync("../inst/extdata/copernicus/global-reanalysis-phy-001-031-grepv2-daily_1639765258612.nc")

# flatten the multidimensional array
df <- df_nc %>%
  # extract NetCDF data as an expanded table
  hyper_tibble() %>%
  # not mandatory, but here is the code to convert to a data.table
  setDT()

## ----netcdf-read-2------------------------------------------------------------------------
# print the first 10 rows
df[sample(nrow(df), 10), ] %>%
  sable(
    caption = "First 10 rows of the flatten netCDF file",
    digits = 2
  )

## ----netcdf-read-3------------------------------------------------------------------------
# load ncmeta library
library(ncmeta)

# then we use the function nc_atts to access attributes of times
print(
  nc_atts(
    "../inst/extdata/copernicus/global-reanalysis-phy-001-031-grepv2-daily_1639765258612.nc",
    "time"
  ) %>%
    # we want information on the unit for time column
    dplyr::filter(name == "units") %>%
    pull(value)
)

## ----netcdf-read-4------------------------------------------------------------------------
# convert time into a readable format
df[, time := as.Date(time, origin = as.Date("1950-01-01"))]

# print
df[sample(nrow(df), 10), ] %>%
  sable(
    caption = "First 10 rows of the flatten netCDF file, with reformat time column",
    digits = 2
  )

## ----netcdf-read-5------------------------------------------------------------------------
# first let's calculate the velocity norm
df[, `:=`(
  vel_cglo = sqrt(uo_cglo^2 + vo_cglo^2),
  vel_oras = sqrt(uo_oras^2 + vo_oras^2),
  vel_foam = sqrt(uo_foam^2 + vo_foam^2),
  vel_glor = sqrt(uo_glor^2 + vo_glor^2),
  month = month(time),
  year = year(time)
)]

# average by month
df_summarize <- df[, .(
  vel_cglo = mean(vel_cglo, na.rm = T),
  vel_oras = mean(vel_oras, na.rm = T),
  vel_foam = mean(vel_foam, na.rm = T),
  vel_glor = mean(vel_glor, na.rm = T)
),
by = .(
  date = paste(year, "-", month),
  longitude,
  latitude
)
] %>%
  .[, time := .GRP, by = .(date)]

## ----netcdf-read-6, cache=FALSE-----------------------------------------------------------
# data wrangling
dataPlot <- melt(df_summarize,
  id.vars = c("date", "longitude", "latitude", "time"),
  variable.name = "model"
)

# let's comput the ouput of all models
res_anim <- ggplot() +
  geom_raster(
    data = dataPlot,
    aes(x = longitude, y = latitude, fill = value),
    interpolate = TRUE
  ) +
  geom_sf(
    data = spData::world,
    col = 1,
    fill = "ivory"
  ) +
  coord_sf(xlim = c(-165, -115), ylim = c(25, 55)) +
  facet_wrap(. ~ model, ncol = 2) +
  theme_jjo() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL) +
  scale_fill_gradientn(colours = oce::oceColors9A(120)) +
  labs(title = "Date: {df_summarize[,unique(date)][frame_time]}") +
  transition_time(time) +
  ease_aes("linear")

## ----netcdf-read-7, fig.cap="4 models of the current velocity in the North-East part of Pacific ocean"----
# let's print
animate(res_anim, height = 500, width = 500)

## ----netcdf-read-8, eval=FALSE, include=FALSE---------------------------------------------
#  # summarized by week
#  df_summarize <- df[, .(
#    longitude = mean(longitude),
#    latitude = mean(latitude),
#    vel_cglo = mean(vel_cglo)
#  ),
#  by = .(date = paste(year(time), "-", week(time)))
#  ] %>%
#    .[, time := .I]
#  
#  # let's print the output of CGLO
#  ggplot() +
#    geom_raster(
#      data = df_summarize,
#      aes(x = longitude, y = latitude, fill = vel_cglo),
#      interpolate = TRUE
#    ) +
#    geom_sf(
#      data = spData::world,
#      col = 1,
#      fill = "ivory"
#    ) +
#    coord_sf(xlim = c(-165, -115), ylim = c(25, 55)) +
#    theme_jjo() +
#    labs(x = NULL, y = NULL) +
#    scale_fill_gradientn(colours = oce::oceColors9A(120)) +
#    labs(title = "Date: {df_summarize$date[frame_time]}") +
#    gganimate::transition_time(time) +
#    gganimate::ease_aes("linear")

