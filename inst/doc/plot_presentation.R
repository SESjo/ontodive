## ---- include = FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
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
library(ggnewscale)
library(ggplot2)
library(magrittr)
library(ggOceanMaps)
library(ggOceanMapsData)
library(maps)
library(cowplot)
library(scales)
library(sf)
library(ontodive)
library(ggpubr)
library(gridExtra)

# remove some warnings
suppressWarnings(library(ggplot2))

## ----echo=FALSE---------------------------------------------------------------------------
# package
library(ontodive)

# load data
data_nes <- get_data("nes")
data_ses <- get_data("ses")

# rbind each dataset
data_2018_nes <- rbindlist(data_nes$year_2018)
data_2014_ses <- rbindlist(data_ses$year_2014)

## ----plot_ind_nes, results='asis', echo=FALSE, fig.asp=1.7--------------------------------
for (i in data_2018_nes[!is.na(lat), unique(.id)]) {
  cat("###", i, "{.unlisted .unnumbered} \n")
  print(plot_ind(data_2018_nes[.id == i, ]))
  cat("\n \n")
}

## ----plot_ind_ses, results='asis', echo=FALSE, fig.asp=1.7--------------------------------
for (i in data_2014_ses[!is.na(lat), unique(.id)]) {
  cat("###", i, "{.unlisted .unnumbered} \n")
  print(plot_ind(data_2014_ses[.id == i, ]))
  cat("\n \n")
}

