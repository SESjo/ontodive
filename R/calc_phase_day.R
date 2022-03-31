#' @title Determine the different phases of the day
#'
#' @description This function can estimate day-time and night-time based on
#' light level (`dbscan`) or using the location and time data (`noaa`).
#'
#' @details
#' Methods:
#'
#' * DBSCAN: use the dbscan clustering algorithm to identify night time and so daytime.
#' * NOAA: using the algorithms provided by the Notional Oceanic & Atmospheric Administration (NOAA)
#' * ...: to be implemented
#'
#' @param dataset A dataset containing the light level per individual per time
#' @param light Name of the light-level column
#' @param date Name of the datetime column
#' @param id Name of the individual ID column
#' @param lon Name of the Longitude column
#' @param lat Name of the Latitude column
#' @param method Method used to identify phases of day
#'
#' @return Return the same data.table with an additional "phase" columns
#'
#' @export
#'
#' @references `vignette("phase_of_the_day")`
#'
#' @importFrom fpc dbscan
#'
#' @import data.table
#' @import magrittr
#' @import maptools
#'
#' @seealso \code{\link[maptools]{crepuscule}}
#'
#' @examples
#' \dontrun{
#' # load data
#' data("data_nes")
#'
#' # night and day calculation
#' result <- calc_phase_day(rbindlist(data_nes$year_2018,
#'   use.name = TRUE,
#'   idcol = TRUE
#' ))
#' }
#'
calc_phase_day <- function(dataset,
                           light = "lightatsurf",
                           date = "date",
                           id = ".id",
                           lon = "lon",
                           lat = "lat",
                           method = "noaa") {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  seconds <-
    .id <-
    . <-
    median <-
    lightatsurf <-
    day_departure <-
    day_departure_test <-
    cluster <-
    phase <-
    hours <-
    sunrise_yesterday <-
    sunrise_today <-
    sunrise_tomorrow <-
    sunset_yesterday <-
    sunset_today <-
    sunset_tomorrow <-
    NULL

  # checks dataset is a data.table, otherwise convert it
  if (!weanlingNES::check_dt(dataset)) setDT(dataset)

  # rename colnames to match the rest of the function
  names(dataset)[names(dataset) == light] <- "lightatsurf"
  names(dataset)[names(dataset) == date] <- "date"
  names(dataset)[names(dataset) == id] <- ".id"
  names(dataset)[names(dataset) == lon] <- "lon"
  names(dataset)[names(dataset) == lat] <- "lat"

  # switch method
  if (method == "dbscan") {
    # add the number of days since departure to account for the evolution across
    # time using a numeric variable (so not using date) in dbscan
    dataset[, day_departure_test := as.numeric(
      # round
      ceiling(
        # the difference between
        difftime(
          # any dates
          date,
          # and the first date trip
          first(
            # (make sure xxxx-xx-xx 00:00:00 is the same day as xxxx-xx-xx 00:00:01)
            as.Date(date) - seconds(1)
          ),
          units = "days"
        )
      )
    ),
    # of every animals
    by = .id
    ]

    # let's first average `light` by individuals, day since departure and hour
    data_inter <- dataset[, .(lightatsurf = median(lightatsurf)),
      by = .(.id,
        day_departure,
        date = as.Date(date),
        hour
      )
    ]

    # remove nan value
    df_clust <- data_inter[!is.na(lightatsurf), .(
      hour,
      day_departure,
      lightatsurf
    )]

    # DBSCAN clustering
    res_dbscan <- dbscan(df_clust,
      eps = 8,
      MinPts = nrow(data_inter) * 0.001,
      method = "raw"
    )

    # referential creation
    ref_phase_day <- data_inter[!is.na(lightatsurf), ] %>%
      # add cluster column from the DBSCAN
      .[, cluster := res_dbscan$cluster] %>%
      # cluster 1 = night, the other = "day"
      .[, phase := fifelse(
        cluster == 1,
        "night",
        "day"
      )] %>%
      # just a trick to avoid calling it twice in the console to display
      .[]

    # split ref_phase by individual and by day
    ref_phase_day_split <- split(ref_phase_day, by = c(".id", "day_departure"))

    # reconstruction of whole day
    ref_phase_day_split_list <- lapply(ref_phase_day_split, function(x) {
      if ("night" %in% x[, unique(phase)]) {
        # get the first and last hour of night in a day
        night_period <- x[phase == "night", .SD[c(1, .N)]]

        # number of night hours
        nb_night_hours <- night_period[, diff(hour)] + 1

        # night table generation
        night_data <- night_period[, .(.id, day_departure, date, phase)] %>%
          .[rep(1, nb_night_hours), ] %>%
          .[, hour := night_period[, seq(min(hour), max(hour))]] %>%
          .[]

        # list of hours at day time
        day_hours <- seq(0, 23)[!seq(0, 23) %in% night_period[, seq(min(hour), max(hour))]]

        # day table generation
        day_data <- night_period[, .(.id, day_departure, date, phase = "day")] %>%
          .[rep(1, 24 - nb_night_hours), ] %>%
          .[, hour := day_hours] %>%
          .[]

        # merge day and night data
        return(rbind(day_data, night_data)[order(.id, day_departure, hour)])
      } else {
        return(x[, .(.id, day_departure, date, phase, hour)])
      }
    })

    # unlist
    ref_phase_day <- rbindlist(ref_phase_day_split_list)

    # set date format
    ref_phase_day[, `:=`(
      date = date + hours(hour),
      hour = NULL,
      day_departure = NULL
      # lightatsurf = NULL,
      # cluster = NULL
    )]

    # rolling join
    dataset <- ref_phase_day[dataset, roll = T, on = .(.id, date)]

    # return
    return(dataset)
  } else if (method == "noaa") {

    # add column sunrise and sunset
    dataset[!is.na(lat), `:=`(
      sunrise_yesterday = maptools::sunriset(matrix(c(lon, lat), ncol = 2),
        date - (3600 * 24),
        direction = "sunrise",
        POSIXct.out = TRUE
      )$time,
      sunset_yesterday = maptools::sunriset(matrix(c(lon, lat), ncol = 2),
        date - (3600 * 24),
        direction = "sunset",
        POSIXct.out = TRUE
      )$time,
      sunrise_today = maptools::sunriset(matrix(c(lon, lat), ncol = 2),
        date,
        direction = "sunrise",
        POSIXct.out = TRUE
      )$time,
      sunset_today = maptools::sunriset(matrix(c(lon, lat), ncol = 2),
        date,
        direction = "sunset",
        POSIXct.out = TRUE
      )$time,
      sunrise_tomorrow = maptools::sunriset(matrix(c(lon, lat), ncol = 2),
        date + (3600 * 24),
        direction = "sunrise",
        POSIXct.out = TRUE
      )$time,
      sunset_tomorrow = maptools::sunriset(matrix(c(lon, lat), ncol = 2),
        date + (3600 * 24),
        direction = "sunset",
        POSIXct.out = TRUE
      )$time
    ), ]

    # add phase columns
    dataset[, phase := ifelse((date %between% list(
      # if date is between sunrise and sunset from the day before,
      sunrise_yesterday,
      sunset_yesterday
    )) |
      (date %between% list(
        # or the day today,
        sunrise_today,
        sunset_today
      )) |
      (date %between% list(
        # or the day after
        sunrise_tomorrow,
        sunset_tomorrow
      )),
    # then it's day, if not it's night
    "day", "night"
    )]

    # remove unwanted columns
    dataset[, `:=`(
      sunset_yesterday = NULL,
      sunrise_yesterday = NULL,
      sunset_today = NULL,
      sunrise_today = NULL,
      sunset_tomorrow = NULL,
      sunrise_tomorrow = NULL
    )]
    # return
    return(dataset)
  } else {
    # return
    print("Nothing implemented yet!")
  }
}
