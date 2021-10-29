## ----setup, include=FALSE------------------------------------------------------
# command to build package without getting vignette error
# https://github.com/rstudio/renv/issues/833
devtools::check(build_args=c("--no-build-vignettes"))

# global option relative to rmarkdown
knitr::opts_chunk$set(echo = TRUE,
                      fig.align = 'center',
                      out.width = "100%",
                      message = FALSE,
                      warning=FALSE)

# library
library(data.table)
library(ggplot2)
library(kableExtra)
library(leaflet) 
library(gtsummary) #https://cran.r-project.org/web/packages/gtsummary/vignettes/tbl_summary.html
library(corrplot)
library(ggcorrplot)
library(ggnewscale)

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
      #plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # figure area
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # axes
      #axis.title = element_text(size = rel(0.85), face = "bold"),
      #axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.2, "lines"), type = "closed")),
      # legend
      # legend.title = element_text(size = rel(0.85), face = "bold"),
      # legend.text = element_text(size = rel(0.70), face = "bold"),
      # legend.key = element_rect(fill = "transparent", colour = NA),
      # legend.key.size = unit(1.5, "lines"),
      # legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#888888", color = "#888888"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

## ------------------------------------------------------------------------------
# load library
library(weanlingNES)

# load data
data("data_nes", package = "weanlingNES")

## ------------------------------------------------------------------------------
# list structure
str(data_nes$year_2016, max.level = 1, give.attr = F, no.list = T)

## ---- eval=FALSE---------------------------------------------------------------
#  # combine all individuals
#  data_2016 = rbindlist(data_nes$year_2016, use.name = TRUE, idcol = TRUE)
#  
#  # display
#  DT::datatable(data_2016[sample.int(.N,100),],options=list(scrollX=T))

## ---- echo=FALSE, results='asis'-----------------------------------------------
# combine all individuals
data_2016 = rbindlist(data_nes$year_2016, use.name = TRUE, idcol = TRUE)

# title
cat("<table style='width: 50%'>",paste0("<caption>", "(#tab:myDThtmltools)", "Sample of 100 random rows from `data_2016`", "</caption>"),"</table>", sep ="\n")

# display
DT::datatable(data_2016[sample.int(.N,100),],options=list(scrollX=T))

## ------------------------------------------------------------------------------
# raw_data
data_2016[, .(
  nb_days_recorded = uniqueN(as.Date(date)),
  max_depth = max(maxpress_dbars),
  sst_mean = mean(sst2_c),
  sst_sd = sd(sst2_c)
), by =.id] %>%
  sable(caption="Summary diving information relative to each 2016 individual", 
        digits=2)

## ---- fig.cap="Distribution of raw `sst2` for the four individuals in 2016"----
ggplot(data_2016, aes(x = sst2_c, fill = .id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  theme_jjo()

## ---- fig.cap="Distribution of filtered `sst2` for the four individuals in 2016"----
data_2016_filter = data_2016[sst2_c < 500, ]
ggplot(data_2016_filter, aes(x = sst2_c, fill = .id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  theme_jjo()

## ------------------------------------------------------------------------------
# nbrow removed
data_2016[sst2_c>500,.(nb_row_removed = .N),by=.id] %>%
  sable(caption = "# of rows removed by 2016-individuals")

## ---- fig.cap="Where and when the `sst2` outliers occured", fig.width=9--------
# max depth
ggplot(data_2016,
       aes(y = -maxpress_dbars, x=as.Date(date), col=.id)) +
  geom_path(show.legend = FALSE) +
  geom_point(data = data_2016[sst2_c>500,], col="black") +
  scale_x_date(date_labels = "%m/%Y") +
  labs(y="Pressure (dbar)", x="Date") +
  facet_wrap(.id ~ .) +
  theme_jjo() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## ---- fig.cap="It is supposed to be the track of `ind_3449`... (red dots are the location of removed rows)", fig.width=8----
# interactive map
leaflet() %>%
  setView(lng = -122, lat = 38, zoom = 2) %>%
  addTiles() %>%
  addPolylines(lat = data_2016[.id == "ind_3449", latitude_degs],
               lng = data_2016[.id == "ind_3449", longitude_degs],
               weight = 2) %>%
  addCircleMarkers(lat = data_2016[.id == "ind_3449" & sst2_c>500, latitude_degs],
                   lng = data_2016[.id == "ind_3449" & sst2_c>500, longitude_degs],
                   radius = 3,
                   stroke=FALSE,
                   color="red",
                   fillOpacity=1)

## ------------------------------------------------------------------------------
# summary of the coordinates by individuals
data_2016[, .(.id, longitude_degs, latitude_degs)] %>%
  tbl_summary(by = .id) %>%
  modify_caption("Summary of `longitude_degree` and `latitude_degree`")

## ---- fig.width=9, fig.cap="Distribution of coordinates per seal"--------------
# distribution coordinates
ggplot(
  data = melt(data_2016[, .(Longitude = longitude_degs, 
                            Latitude = latitude_degs, 
                            .id)], id.vars =
                ".id", value.name = "Coordinate"),
  aes(x = Coordinate, fill = .id)
) +
  geom_histogram(show.legend = F) +
  facet_grid(variable ~ .id) +
  theme_jjo()

## ---- fig.cap="An attempt to display the `ind_3449`'s track", fig.width=8------
# interactive map
leaflet() %>%
  setView(lng = -122, lat = 50, zoom = 3) %>%
  addTiles() %>%
  addPolylines(lat = data_2016[.id == "ind_3449", abs(latitude_degs)],
               lng = data_2016[.id == "ind_3449", -abs(longitude_degs)],
               weight = 2)

## ----fig.cap="Check for missing value in 2016-individuals", out.width="100%"----
# build dataset to check for missing values
dataPlot = melt(data_2016_filter[, .(.id, is.na(.SD)), .SDcol = -c(".id",
                                                            "rec#",
                                                            "date",
                                                            "time")])
# add the id of rows
dataPlot[, id_row := c(1:.N), by = c("variable",".id")]

# plot
ggplot(dataPlot, aes(x = variable, y = id_row, fill = value)) +
  geom_tile() +
  labs(x = "Attributes", y = "Rows") +
  scale_fill_manual(values = c("white", "black"),
                    labels = c("Real", "Missing")) +
  facet_wrap(.id ~ ., scales = "free_y") +
  theme_jjo() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_rect(colour = "black")
  )

## ------------------------------------------------------------------------------
# list structure
str(data_nes$year_2018, max.level = 1, give.attr = F, no.list = T)

## ---- eval=FALSE---------------------------------------------------------------
#  # combine all individuals
#  data_2018 = rbindlist(data_nes$year_2018, use.name = TRUE, idcol = TRUE)
#  
#  # display
#  DT::datatable(data_2018[sample.int(.N,100),],options=list(scrollX=T))

## ---- echo=FALSE, results='asis'-----------------------------------------------
# combine all individuals
data_2018 = rbindlist(data_nes$year_2018, use.name = TRUE, idcol = TRUE)

# title
cat("<table style='width: 50%'>",paste0("<caption>", "(#tab:myDThtmltools)", "Sample of 100 random rows from `data_2018`", "</caption>"),"</table>", sep ="\n")

# display

DT::datatable(data_2018[sample.int(.N,100),],options=list(scrollX=T))

## ------------------------------------------------------------------------------
# raw_data
data_2018[, .(
  nb_days_recorded = uniqueN(as.Date(date)),
  nb_dives = .N,
  maxdepth_mean = mean(maxdepth),
  dduration_mean = mean(dduration),
  botttime_mean = mean(botttime),
  pdi_mean = mean(pdi, na.rm=T)
), by =.id] %>%
  sable(caption="Summary diving information relative to each 2018 individual", 
        digits=2)

## ----fig.cap="Check for missing value in 2018-individuals", fig.width=9--------
# build dataset to check for missing values
dataPlot = melt(data_2018[, .(.id, is.na(.SD)), .SDcol = -c(".id",
                                                            "divenumber",
                                                            "year",
                                                            "month",
                                                            "day",
                                                            "hour",
                                                            "min",
                                                            "sec",
                                                            "juldate",
                                                            "divetype",
                                                            "date")])
# add the id of rows
dataPlot[, id_row := c(1:.N), by = c("variable",".id")]

# plot
ggplot(dataPlot, aes(x = variable, y = id_row, fill = value)) +
  geom_tile() +
  labs(x = "Attributes", y = "Rows") +
  scale_fill_manual(values = c("white", "black"),
                    labels = c("Real", "Missing")) +
  facet_wrap(.id ~ ., scales = "free_y") +
  theme_jjo() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_rect(colour = "black")
  )

## ------------------------------------------------------------------------------
# table with percent
table_inter = data_2018[, lapply(.SD, function(x) {
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
  "date"
)]

# find which are different from 0
cond_inter = sapply(table_inter,function(x){x==0})

# display the percentages that are over 0
table_inter[,which(cond_inter) := NULL] %>%
  sable(caption="Percentage of missing values per columns having missing values!")%>% 
  scroll_box(width = "100%")

## ---- fig.cap='Distribution of `dduration` for each seal. The dashed line highlight the "subjective" threshold used to remove outliers (3000 sec)', fig.height=3----
ggplot(data_2018[,.SD][,state:="Before"], 
       aes(x=dduration, fill = .id))+
  geom_histogram(show.legend = FALSE)+
  geom_vline(xintercept = 3000, linetype = "longdash") +
  facet_grid(state~.id,
             scales="free")+
  labs(y="# of dives", x="Dive duration (s)")+
  theme_jjo()


## ---- fig.cap='Same distribution of `dduration` for each seal but after removing any `dduration` > 3000 sec. The dashed line highlight the "subjective" threshold used to remove outliers', fig.height=3----
ggplot(data_2018[dduration<3000,][][,state:="After"], 
       aes(x=dduration, fill = .id))+
  geom_histogram(show.legend = FALSE)+
  geom_vline(xintercept = 3000, linetype = "longdash") +
  facet_grid(state~.id,
             scales="free")+
  labs(x="# of dives", y="Dive duration (s)")+
  theme_jjo()

## ------------------------------------------------------------------------------
# filter data
data_2018_filter = data_2018[dduration < 3000, ]

# nbrow removed
data_2018[dduration>= 3000,.(nb_row_removed = .N),by=.id] %>%
  sable(caption = "# of rows removed by 2018-individuals")

## ---- results='asis', cache=TRUE-----------------------------------------------
names_display = names(data_2018_filter[, -c(
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
  "day_departure"
)])
for (i in names_display) {
  cat('####', i, '{-} \n')
  if (i == "maxdepth") {
    print(
      ggplot() +
        geom_point(
          data = data_2018_filter[, .(.id,
                                                          date,
                                                          thermoclinedepth)],
          aes(
            x = as.Date(date),
            y = -thermoclinedepth,
            colour = "Thermocline (m)"
          ),
          
          alpha = .2,
          size = .5
        ) +
        geom_point(
          data = data_2018_filter[, .(.id,
                                                          date,
                                                          euphoticdepth)],
          aes(
            x = as.Date(date),
            y = -euphoticdepth,
            colour = "Euphotic (m)"
          ),
          alpha = .2,
          size = .5
        ) +
        scale_colour_manual(
          values = c("Thermocline (m)" = 'red',
                     "Euphotic (m)" = "black"),
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
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom")
    )
    cat("<blockquote> Considering `ind_2018074` has slightly different values than other individuals for the thermocline depth, it would be interesting to see where the animal went. </blockquote>")
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
  
  cat(' \n \n')
}

## ---- results='asis', cache=TRUE-----------------------------------------------
for (i in names_display) {
  cat('####', i, '{-} \n')
  if (i == "maxdepth") {
    print(
      ggplot() +
        geom_point(
          data = data_2018_filter[day_departure < 32, .(.id,
                                                          day_departure,
                                                          thermoclinedepth)],
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
          data = data_2018_filter[day_departure < 32, .(.id,
                                                          day_departure,
                                                          euphoticdepth)],
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
          values = c("Thermocline (m)" = 'red',
                     "Euphotic (m)" = "black"),
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
        theme(legend.position="bottom")
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
  
  cat(' \n \n')
}

## ---- fig.cap="Correlation matrix (crosses indicate non significant correlation)", fig.width=10, fig.height=10----
# compute correlation
corr_2018 = round(cor(data_2018_filter[, names_display, with = F], 
                      use = "pairwise.complete.obs"), 1)

# replace NA value by 0
corr_2018[is.na(corr_2018)] = 0

# compute p_values
corr_p_2018 = cor_pmat(data_2018_filter[, names_display, with = F])

# replace NA value by 0
corr_p_2018[is.na(corr_p_2018)] = 1

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

## ------------------------------------------------------------------------------
# flatten correlation matrix
cor_result_2018 = flat_cor_mat(corr_2018, corr_p_2018)

# keep only the one above .7
cor_result_2018[cor>=.7,][order(-abs(cor))] %>%
  sable(caption="Pairwise correlation above 0.75 and associated p-values")

## ---- fig.cap="Proportion dive types"------------------------------------------
# dataset to plot proportional area plot
data_2018_filter[, sum_id := .N, by = .(.id, day_departure)][, sum_id_days := .N, by = .(.id, day_departure, divetype)][, prop := sum_id_days /
                                                                                                                          sum_id]
dataPlot = unique(data_2018_filter[, .(prop, .id, divetype, day_departure)])

# area plot
ggplot(dataPlot, aes(
  x = as.numeric(day_departure),
  y = prop,
  fill = as.character(divetype)
)) +
  geom_area(alpha = 0.6 , size = 1) +
  facet_wrap(.id ~ ., scales = "free") +
  theme_jjo() +
  theme(legend.position="bottom") +
  labs(x="# of days since departure", y="Proportion of dives", fill = "Dive types")

## ---- fig.cap="Dive duration vs. Maximum Depth colored 2018-individuals"-------
# plot
ggplot(data = data_2018_filter, aes(y = dduration, x = maxdepth, col = .id)) +
  geom_point(size = .5, alpha = .1, show.legend = FALSE) +
  facet_wrap(.id ~ .) +
  labs(x="Maximum depth (m)", y="Dive duration (s)")+
  theme_jjo()

## ---- fig.cap="Dive duration vs. Maximum Depth colored by Dive Type"-----------
# plot
ggplot(data = data_2018_filter, aes(y = dduration, x = maxdepth, col = divetype)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  labs(x="Maximum depth (m)", y="Dive duration (s)")+
  theme_jjo() +
  theme(legend.position="bottom")

## ---- fig.cap="Dive duration vs. Maximum Depth colored by # days since departure"----
# plot
ggplot(data = data_2018_filter[,prop_track := (day_departure*100)/max(day_departure),by=.id], aes(y = dduration, x = maxdepth, col = prop_track)) +
  geom_point(size = .5, alpha = .1) +
  facet_wrap(.id ~ .) +
  labs(x="Maximum depth (m)", y="Dive duration (s)", col="Proportion of completed track (%)")+
  scale_color_continuous(type = "viridis")+
  theme_jjo() +
  theme(legend.position="bottom")

## ---- fig.cap="Drift rate vs. Bottom time", fig.height=7-----------------------
# plot
ggplot(data_2018_filter[,.(driftrate=median(driftrate,na.rm=T),
                           botttime=median(botttime,na.rm=T),
                           maxdepth=median(maxdepth,na.rm=T),
                           dduration=median(dduration,na.rm=T)), by=.(.id,day_departure)],
       aes(x=botttime, y=driftrate, col=.id))+
  geom_point(size=.5,alpha=.5)+
  geom_smooth(method="lm")+
  guides(color=FALSE)+
  facet_wrap(.id~.)+
  scale_x_continuous(limits = c(0,700))+
  labs(x = "Daily median Bottom time (s)", y = "Daily median drift rate (m.s-1)")+
  theme_jjo()

## ---- fig.cap="Drift rate vs. Maximum depth", fig.height=7---------------------
# plot
ggplot(data_2018_filter[,.(driftrate=median(driftrate,na.rm=T),
                           botttime=median(botttime,na.rm=T),
                           maxdepth=median(maxdepth,na.rm=T),
                           dduration=median(dduration,na.rm=T)), by=.(.id,day_departure)],
       aes(x=maxdepth, y=driftrate, col=.id))+
  geom_point(size=.5,alpha=.5)+
  geom_smooth(method="lm")+
  guides(color=FALSE)+
  facet_wrap(.id~.)+
  labs(x = "Daily median Maximum depth (m)", y = "Daily median drift rate (m.s-1)")+
  theme_jjo()

## ---- fig.cap="Drift rate vs. Dive duration", fig.height=7---------------------
# plot
ggplot(data_2018_filter[,.(driftrate=median(driftrate,na.rm=T),
                           botttime=median(botttime,na.rm=T),
                           maxdepth=median(maxdepth,na.rm=T),
                           dduration=median(dduration,na.rm=T)), by=.(.id,day_departure)],
       aes(x=dduration, y=driftrate, col=.id))+
  geom_point(size=.5,alpha=.5)+
  geom_smooth(method="lm")+
  guides(color=FALSE)+
  facet_wrap(.id~.)+
  labs(x = "Daily median Dive duration (s)", y = "Daily median drift rate (m.s-1)")+
  theme_jjo()

