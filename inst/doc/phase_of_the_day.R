## ----setup, include=FALSE---------------------------------------------------------------
# command to build package without getting vignette error
# https://github.com/rstudio/renv/issues/833
# devtools::check(build_args=c("--no-build-vignettes"))

# global option relative to rmarkdown
knitr::opts_chunk$set(
  cache = TRUE,
  echo = TRUE,
  fig.align = 'center',
  out.width = "100%",
  message = FALSE,
  warning=FALSE)

# library
library(data.table)
library(magrittr)
library(ggplot2)
library(TwGeos)
library(fpc)
library(lubridate)

# remove some warnings
suppressWarnings(library(ggplot2))

## ----phase-of-the-day-11, cahce=TRUE, fig.cap="Visualization of the moment where the light was measured at the surface colored with the associated cluster (HCPC)", fig.height=6----
# remove nan value
df_clust = dataPlot[!is.na(lightatsurf),.(hour,day_departure,lightatsurf)]
                    
# HCPC with onlys 2 groups
res_hcpc = FactoMineR::HCPC(df_clust, nb.clust = 2, graph = FALSE)

# display the result
ggplot() +
  geom_tile(data = dataPlot[!is.na(lightatsurf),
                            ][,cluster:=res_hcpc$data.clust$clust], 
            aes(x = day_departure, y = hour, fill = factor(cluster))) + 
  theme_jjo() +
  facet_grid(.id ~ .)+
  labs(x = "# of days since departure", 
       y = "Hour", 
       fill = "cluster", 
       col = "Sunrise") +
  theme(legend.position = c("bottom"))

## ----dbscan_first_test, fig.cap="Visualization of the moment where the light was measured at the surface colored with the associated cluster (DBSCAN, `eps=45`, `MinPts=nrow(dataPlot)*0.06`)", fig.height=6----
# determine the right values by testing several of them...
res_dbscan = dbscan(df_clust,
            eps = 21,
            MinPts = nrow(dataPlot) * 0.01,
            method = "raw")

# display the result
ggplot() +
  geom_tile(data = dataPlot[!is.na(lightatsurf),
                            ][,cluster:=res_dbscan$cluster],
            aes(x = day_departure, y = hour, fill = factor(cluster))) +
  theme_jjo() +
  facet_grid(.id ~ .)+
  labs(x = "# of days since departure",
       y = "Hour",
       fill = "cluster",
       col = "Sunrise") +
  theme(legend.position = c("bottom"))

## ----phase-of-the-day-14----------------------------------------------------------------
# referential creation
ref_phase_day = dataPlot[!is.na(lightatsurf),] %>% 
  .[, cluster := res_dbscan$cluster] %>% 
  .[, cluster := fifelse(cluster == 1, "night", "day")] %>% 
  .[]

# reshape
ref_phase_day = melt(ref_phase_day, 
                     id.vars = c(".id","date","hour"), 
                     measure.vars = "cluster", 
                     value.name = "phase")

# set date format
ref_phase_day[, `:=` (date = date + hours(hour),
                      hour = NULL,
                      variable = NULL)]

# rolling join
data_2018 = ref_phase_day[data_2018, roll=T, on = .(.id, date)]

## ----phase-of-the-day-15, eval=FALSE----------------------------------------------------
#  # identification of transition
#  ref_phase_day[,transition := c(1,abs(diff(as.numeric(as.factor(phase)))))]
#  
#  # keep only the first date and the last date (i.e transition) by date and individual
#  ref_phase_day = ref_phase_day[transition==1, .SD[c(1,.N)], by=.(.id,date)]
#  
#  # convert date to take into account hour
#  ref_phase_day[, date:=date+hours(hour)]
#  
#  # add sunset
#  test = rbind(ref_phase_day[,.SD,.SDcols = -c("transition")][, date:= date+minutes(30)],
#               ref_phase_day[,.SD,.SDcols = -c("transition")][phase=="night",][, date := date - minutes(30)][,phase:="sunset"],
#               ref_phase_day[,.SD,.SDcols = -c("transition")][phase=="day",][, date := date - minutes(30)][,phase:="sunrise"])

