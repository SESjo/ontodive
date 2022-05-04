#' @title Display summary information for a seal
#'
#' This function displays several key information regarding a seal.
#'
#' @details
#' * A map of the seal's trip at sea with a color code based on the number of days since departure
#' * The max depth reached for each dives across time
#' * The evolution of their daily median drift rate across time
#' * the evolution of their ADL across time
#'
#' @param data_seal A dataset containing information dive information on one animal (maxdepth, lat, lon, date, driftrate)
#' @param col_text The color of the text
#' @param col_back The color of the background
#'
#' @return A ggplot object which contains all figures
#'
#' @export
#'
#' @rawNamespace import(data.table, except = c(first,last))
#' @import magrittr
#' @import scales
#' @import ggplot2
#' @import ggOceanMaps
#' @import ggOceanMapsData
#' @import ggpubr
#' @import ggspatial
#' @import patchwork
#'
#' @examples
#' # load data
#' data("data_nes")
#'
#' # plot result
#' plot_ind(data_nes$year_2018$ind_2018070)
plot_ind <- function(data_seal,
                     col_text = "black",
                     col_back = "transparent") {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  . <-
    lat <-
    lon <-
    day_departure <-
    sp <-
    long <-
    region <-
    nb_dives <-
    maxdepth <-
    divetype <-
    median <-
    quantile <-
    dduration <-
    value <-
    botttime <-
    pdi <-
    .id <-
    driftrate <-
    NULL

  # get warning status
  oldw <- getOption("warn")

  # remove warnings
  options(warn = -1)

  # check variables
  if (!is.character(col_text)) {
    warning(paste0(
      "Color has to be a character, please change col_text=",
      col_text
    ))
  }
  if (!is.character(col_text)) {
    warning(paste0(
      "Color has to be a character, please change col_back=",
      col_back
    ))
  }

  # recursive function if several individuals
  if (data_seal[, uniqueN(.id)] > 1) {
    return(lapply(split(data_seal, by = ".id"), plot_ind))
  }

  # transparent = element_blank
  if (col_back == "transparent") {
    col_back <- element_blank()
  }

  # build a theme
  theme_plot_ind <- function(base_size = 12) {
    theme(
      plot.background = col_back,
      strip.background = col_back,
      strip.text = element_text(colour = col_text),
      legend.background = col_back,
      legend.text = element_text(colour = col_text),
      legend.title = element_text(colour = col_text),
      panel.background = col_back,
      panel.border = col_back,
      axis.text = element_text(colour = col_text),
      axis.title = element_text(colour = col_text),
      axis.ticks = col_back,
      axis.line = element_line(
        color = col_text,
        arrow = arrow(length = unit(0.2, "lines"), type = "closed")
      )
    )
  }

  # color palette creation
  colPal <- col_numeric(palette = "RdYlGn",
                        domain = data_seal[!is.na(lat), day_departure])

  # plot the trip at sea
  if (data_seal[, unique(sp)] == "nes") {
    trip <- basemap(limits = c(-165, -120, 35, 59),
                    bathymetry = TRUE) +
      geom_path(
        data = data_seal[!is.na(lat), ],
        aes(x = lon, y = lat),
        col = data_seal[!is.na(lat), colPal(day_departure)],
        size = 1
      ) +
      annotation_scale(location = "br") +
      annotation_north_arrow(location = "tr", which_north = "true") +
      theme_plot_ind()
    # set the number of dives to have a complete day
    nb_dives_to_complete_day = 50
  } else {
    trip <-
      basemap(
        limits = c(45, 110, -65, -40),
        bathymetry = TRUE,
        rotate = TRUE
      ) +
      # geom_spatial_path instead of geom_point because rotate = T
      ggspatial::geom_spatial_path(
        data = data_seal[!is.na(lat), ],
        aes(x = lon, y = lat),
        crs = 4326,
        col = data_seal[!is.na(lat), colPal(day_departure)],
        size = 1
      ) +
      annotation_scale(location = "br") +
      annotation_north_arrow(location = "tr", which_north = "true") +
      theme_plot_ind()
    # set the number of dives to have a complete day
    nb_dives_to_complete_day = 8
  }

  # download a map world
  world <- map_data("world")

  # plot the map world
  worldmap_inter <- ggplot() +
    geom_map(
      data = world,
      map = world,
      aes(x = long, y = lat, map_id = region),
      color = NA,
      fill = "grey60"
    ) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
    coord_map("ortho",
              orientation = c(
                ifelse(data_seal[, unique(sp)] == "nes", 31,-30),
                ifelse(data_seal[, unique(sp)] == "nes", -120, 70),
                0
              )) +
    geom_rect(
      data = data.frame(),
      aes(
        xmin = fifelse(data_seal[, unique(sp)] == "nes", -165, 35),
        xmax = fifelse(data_seal[, unique(sp)] == "nes", -120, 115),
        ymin = fifelse(data_seal[, unique(sp)] == "nes", 35, -65),
        ymax = fifelse(data_seal[, unique(sp)] == "nes", 59, -40)
      ),
      color = "red",
      fill = NA
    ) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.border = col_back,
      plot.background = element_rect(fill = "transparent",
                                     color = NA),
      panel.background = element_rect(fill = "transparent")
    )

  # # create a background plot
  # background_plot <- ggplot(data = data.frame(x=1,y=1),
  #                           aes(x=x,y=y)) +
  #   geom_point(size = 48, color = "blue") +
  #   theme(
  #     axis.title = element_blank(),
  #     axis.text = element_blank(),
  #     axis.ticks = element_blank(),
  #     axis.line = element_blank(),
  #     panel.grid.major = element_blank(), #remove major gridlines
  #     panel.grid.minor = element_blank(), #remove minor gridline
  #     plot.background = element_rect(fill = "transparent",
  #                                    color = NA),
  #     panel.background = element_rect(fill = "transparent")
  #   )
  #
  # # add white background
  # worldmap <-
  #   background_plot +
  #   worldmap_inter +
  #   plot_layout(design =  c(
  #     area(t = 1, l = 1, b = 1, r = 1),
  #     area(t = 1, l = 1, b = 1, r = 1)
  #   ))

  # add the map world on the trip plot
  main_plot <- trip +
    inset_element(worldmap_inter,
                  left = -0.05, bottom = 0, right = 0.2, top = 0.2)

  # # add the map world on the trip plot
  # main_plot <- cowplot::ggdraw(trip) +
  #   cowplot::draw_plot(
  #     plot = worldmap,
  #     halign = 0,
  #     valign = 0,
  #     x = .07,
  #     y = ifelse(data_seal[, unique(sp)] == "nes", .17, .3),
  #     scale = .2
  #   )

  # select day that have at least 50 dives
  days_to_keep <- data_seal[, .(nb_dives = .N),
                            by = .(.id, day_departure)] %>%
    .[nb_dives >= nb_dives_to_complete_day, ]

  # keep only those days
  data_seal_complete_day <- merge(data_seal,
                                  days_to_keep,
                                  by = c(".id", "day_departure"))

  # maxdepth
  dataPlot_1 <- melt(
    data_seal_complete_day[, .(date,
                               "Maximum Depth (m)" = maxdepth,
                               day_departure)],
    id.vars = c("date", "day_departure"),
    measure.vars = c("Maximum Depth (m)")
  )

  # driftrate
  dataPlot_2 <- melt(
    data_seal_complete_day[divetype == "2: drift", .(
      date = first(date),
      "Daily Drift Rate (m/s)" = median(driftrate, na.rm = T),
      day_departure = first(day_departure)
    ),
    by = as.Date(date)],
    id.vars = c("date", "day_departure"),
    measure.vars = c("Daily Drift Rate (m/s)")
  )

  # bADL
  dataPlot_3 <- melt(
    data_seal_complete_day[divetype == "1: foraging",
                           .(
                             date = first(date),
                             "Behavioral ADL (min)" = round(quantile(dduration, 0.95) /
                                                              60),
                             day_departure = first(day_departure)
                           ),
                           by = as.Date(date)],
    id.vars = c("date", "day_departure"),
    measure.vars = c("Behavioral ADL (min)")
  )

  # combine
  dataPlot <- rbind(dataPlot_1, dataPlot_2, dataPlot_3)

  # print
  plot_1 <- ggplot() +
    geom_point(
      data = dataPlot_1,
      aes(x = as.Date(date),
          y = value),
      col = dataPlot_1[, colPal(day_departure)],
      alpha = fifelse(data_seal[, unique(sp)] == "nes", 0.2, 0.6),
      size = .5,
      show.legend = FALSE
    ) +
    scale_x_date(date_labels = "%m/%Y") +
    scale_y_reverse() +
    labs(x = "Date", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "Top") +
    facet_grid(variable ~ ., scales = "free") +
    theme_bw() +
    theme_plot_ind() +
    theme(
      axis.title.x = col_back,
      axis.text.x = col_back,
      axis.ticks.x = col_back
    )
  plot_2 <- ggplot() +
    geom_point(
      data = dataPlot_2,
      aes(x = as.Date(date),
          y = value),
      col = dataPlot_2[, colPal(day_departure)],
      size = 1,
      show.legend = TRUE
    ) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      size = 1.5,
      col = col_text
    ) +
    scale_x_date(date_labels = "%m/%Y") +
    labs(x = "Date", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "Top") +
    facet_grid(variable ~ ., scales = "free") +
    theme_bw() +
    theme_plot_ind() +
    theme(
      axis.title.x = col_back,
      axis.text.x = col_back,
      axis.ticks.x = col_back
    )

  plot_3 <- ggplot() +
    geom_point(
      data = dataPlot_3,
      aes(x = as.Date(date),
          y = value),
      col = dataPlot_3[, colPal(day_departure)],
      size = 1,
      show.legend = FALSE
    ) +
    geom_smooth(data = dataPlot_3,
                aes(x = as.Date(date),
                    y = value)) +
    scale_x_date(date_labels = "%m/%Y") +
    labs(x = "Date", y = "") +
    theme_bw() +
    theme_plot_ind() +
    facet_grid(variable ~ ., scales = "free")

  # summary table
  table_1 <- transpose(data_seal[, .(
    "Nb of days recorded" = uniqueN(as.Date(date)),
    "Duty cycle" = if(data_seal[,unique(sp)=="nes"]) "All dives" else {"1 dive every ~2.25 hr"},
    "Nb of dives recorded" = .N,
    "Median Max Depth (m)" = round(quantile(maxdepth, 0.5), 1),
    "Median Dive Duration (min)" = round(quantile(dduration, 0.5) / 60, 1),
    "Median Bottom Duration (min)" = round(quantile(botttime, 0.5) / 60, 1)
  ), by = .("Seal ID" = .id)], keep.names = " ", make.names = 1)
  table_1 <-
    ggtexttable(table_1,
                rows = NULL,
                theme = ttheme(colnames.style = colnames_style(fill = "white"))) %>%
    tab_add_hline(at.row = c(1, nrow(table_1) + 1),
                  row.side = c("bottom"))

  # set warning status the way it was
  options(warn = oldw)

  # final plot
  final_plot = table_1 / main_plot / plot_1 / plot_2 / plot_3 +
    plot_layout(heights = c(1,2,1,1,1))

  # return
  return(final_plot)
}
