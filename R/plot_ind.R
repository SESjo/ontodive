#' Display summary information for a seal
#'
#' This function displays several key information regarding a seal.
#'
#'
#' * A map of the seal's trip at sea with a color code based on the number of days since departure
#'
#' * The max depth reached for each dives across time
#'
#' * The evolution of their daily median drift rate across time
#'
#' * the evolution of their ADL across time
#'
#' @param data_seal A dataset containing information on the dives (maxdepth, lat, lon, date, driftrate)
#' @param ind The name of the individual considered
#' @param col_text The color of the text
#' @param col_back The color of the background
#'
#' @return A ggplot object which contains all figures
#'
#' @export
#'
#' @import data.table
#' @import magrittr
#' @import scales
#' @importFrom cowplot ggdraw draw_plot
#' @import ggplot2
#' @import ggOceanMaps
#' @import ggOceanMapsData
#' @import ggpubr
#'
#' @examples
#' # load data
#' data("data_nes")
#'
#' # plot result
#' plot_ind(data_nes$year_2018$ind_2018070, ind = "ind_2018070")
plot_ind <- function(data_seal,
                     ind,
                     col_text = "black",
                     col_back = "transparent") {
  # require package
  require(data.table)
  require(magrittr)
  require(scales)
  require(ggplot2)
  require(ggpubr)
  require(ggOceanMaps)
  require(cowplot)

  # remove some warnings
  suppressWarnings(library(ggplot2))

  # check variables
  if (!is.character(ind)) {
    warning("The seal ID has to be a character")
  }
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

  # make sure the individual is in the dataset
  if (data_seal[.id == ind, .N] == 0) {
    warning(paste0("The seal ", ind, " is not present in the data"))
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
      axis.line = element_line(colour = col_text),
      axis.ticks = col_back
    )
  }

  # color palette creation
  colPal <- col_numeric(
    palette = "RdYlGn",
    domain = data_seal[
      .id == ind &
        !is.na(lat),
      day_departure
    ]
  )

  # plot the trip at sea
  trip <- basemap(limits = c(-165, -120, 35, 59), bathymetry = TRUE) +
    geom_path(
      data = data_seal[.id == ind &
        !is.na(lat), ],
      aes(x = lon, y = lat),
      col = data_seal[
        .id == ind &
          !is.na(lat),
        colPal(day_departure)
      ],
      size = 1
    ) +
    theme_plot_ind()

  # download a map world
  world <- map_data("world")

  # plot the map world
  worldmap <-
    ggplot() +
    geom_map(
      data = world,
      map = world,
      aes(x = long, y = lat, map_id = region),
      color = NA,
      fill = "grey60"
    ) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
    coord_map("ortho", orientation = c(31, -120, 0)) +
    geom_rect(
      data = data.frame(),
      aes(
        xmin = -165,
        xmax = -120,
        ymin = 35,
        ymax = 59
      ),
      color = "red",
      fill = NA
    ) +
    theme_bw() +
    theme(
      panel.border = col_back,
      axis.title = col_back,
      axis.text = col_back,
      axis.ticks = col_back,
      plot.background = element_rect(fill = "transparent", color = NA)
    )

  # add the map world on the trip plot
  main_plot <- ggdraw(trip) +
    draw_plot(
      plot = worldmap,
      halign = 0,
      valign = 0,
      x = .07,
      y = .17,
      scale = .2
    )

  # select day that have at least 50 dives
  days_to_keep <- data_seal[, .(nb_dives = .N),
    by = .(.id, day_departure)
  ] %>%
    .[nb_dives >= 50, ]

  # keep only those days
  data_seal_complete_day <- merge(data_seal,
    days_to_keep,
    by = c(".id", "day_departure")
  )


  # maxdepth
  dataPlot_1 <- melt(
    data_seal_complete_day[.id == ind, .(date,
      "Maximum Depth (m)" = -maxdepth,
      day_departure
    )],
    id.vars = c("date", "day_departure"),
    measure.vars = c("Maximum Depth (m)")
  )

  # driftrate
  dataPlot_2 <- melt(
    data_seal_complete_day[.id == ind &
      divetype == "2: drift", .(
      date = first(date),
      "Daily Drift Rate (m/s)" = median(driftrate, na.rm = T),
      day_departure = first(day_departure)
    ),
    by = as.Date(date)
    ],
    id.vars = c("date", "day_departure"),
    measure.vars = c("Daily Drift Rate (m/s)")
  )

  # bADL
  dataPlot_3 <- melt(
    data_seal_complete_day[.id == ind &
      divetype == "1: foraging",
    .(
      date = first(date),
      "Behavioral ADL (min)" = round(quantile(dduration, 0.95) /
        60),
      day_departure = first(day_departure)
    ),
    by = as.Date(date)
    ],
    id.vars = c("date", "day_departure"),
    measure.vars = c("Behavioral ADL (min)")
  )

  # combine
  dataPlot <- rbind(dataPlot_1, dataPlot_2, dataPlot_3)

  # print
  plot_1 <- ggplot() +
    geom_point(
      data = dataPlot_1,
      aes(
        x = as.Date(date),
        y = value
      ),
      col = dataPlot_1[, colPal(day_departure)],
      alpha = 1 / 5,
      size = .5,
      show.legend = FALSE
    ) +
    scale_x_date(date_labels = "%m/%Y") +
    labs(x = "Date", y = "") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "Top"
    ) +
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
      aes(
        x = as.Date(date),
        y = value
      ),
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
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "Top"
    ) +
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
      aes(
        x = as.Date(date),
        y = value
      ),
      col = dataPlot_3[, colPal(day_departure)],
      size = 1,
      show.legend = FALSE
    ) +
    geom_smooth(
      data = dataPlot_3,
      aes(
        x = as.Date(date),
        y = value
      )
    ) +
    scale_x_date(date_labels = "%m/%Y") +
    labs(x = "Date", y = "") +
    theme_bw() +
    theme_plot_ind() +
    facet_grid(variable ~ ., scales = "free")

  # summary table
  table_1 <- transpose(data_seal[.id == ind, .(
    "Nb of days recorded" = uniqueN(as.Date(date)),
    "Nb of dives" = .N,
    "Average Max Depth (min)" = round(mean(maxdepth) / 60, 1),
    "Average Dive Duration (min)" = round(mean(dduration) / 60, 1),
    "Average Bottom Duration (min)" = round(mean(botttime) / 60, 1),
    "Average Post-dive interval Duration (min)" = round(mean(pdi, na.rm = T) / 60, 1)
  ), by = .("Seal ID" = .id)], keep.names = " ", make.names = 1)
  table_1 <-
    ggtexttable(
      table_1,
      rows = NULL,
      theme = ttheme(colnames.style = colnames_style(fill = "white"))
    ) %>% tab_add_hline(at.row = c(1, nrow(table_1) + 1), row.side = c("bottom"))

  # return
  return(
    plot_grid(
        table_1,
        main_plot,
      plot_1,
      plot_2,
      plot_3,
      ncol = 1,
      axis = "rl",
      align = "v",
      rel_heights = c(1, 2, 1, 1, 1, 1)
    )
  )
}
