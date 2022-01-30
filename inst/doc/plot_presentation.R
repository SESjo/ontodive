## ---- include = FALSE------------------------
knitr::opts_chunk$set(
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
library(ggnewscale)
library(ggplot2)
library(magrittr)
library(ggOceanMaps)
library(ggOceanMapsData)
library(maps)
library(cowplot)
library(scales)
library(sf)
library(weanlingNES)
library(gridExtra)

# remove some warnings
suppressWarnings(library(ggplot2))

## ----echo=FALSE------------------------------
# read the processed data
data_2018_filter <- readRDS("tmp/data_2018_filter.rds")

## ----plot_ind, results='asis', cache=TRUE, echo=FALSE, fig.asp=1.5----
for (i in data_2018_filter[!is.na(lat),unique(.id)]) {
  cat("###", i, "{.unlisted .unnumbered} \n")
    print(
      plot_ind(data_2018_filter, ind = i)
    )
  cat("\n \n")
}

