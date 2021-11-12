#' Determine the different phasesof the day
#'
#' This function can estimate daytime and nightime based on light level using several different methods.
#'
#' method = DBSCAN: use the dbscan clustering algorithm to identify night time and so daytime.
#'
#' @param dataset A dataset containing the light level per individual per time
#' @param method Method used to identify phases of day
#'
#' @return Return the same data.table with an additional "phase" columns
#' @export
#'
#' @importFrom fpc dbscan
#' @import data.table
#' @examples
calc_phase_day <- function(dataset, method = "dbscan") {
  # check class data
  if (!check_dt(dataset)) {
    warning("The data used in calc_phase_of_day should be a data.table")
  }

  # switch method
  if (method == "dbscan") {
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
    ref_phase_day <- data_inter[!is.na(lightatsurf),
    ][, cluster := res_dbscan$cluster
    ][, phase := fifelse(
      cluster == 1,
      "night",
      "day"
    )][]


    # split ref_phase by individual and by day
    ref_phase_day_split = split(ref_phase_day, by = c(".id", "day_departure"))

    # reconstruction of whole day
    ref_phase_day_split_list = lapply(ref_phase_day_split, function(x) {
      if ("night" %in% x[,unique(phase)]){
        # get the first and last hour of night in a day
        night_period = x[phase == "night", .SD[c(1, .N)], by = .(day_departure, .id)]

        # number of night hours
        nb_night_hours = night_period[, diff(hour)] + 1

        # night table generation
        night_data = night_period[, .(.id, day_departure, date, phase)][rep(1, nb_night_hours), ][, hour := night_period[, seq(min(hour), max(hour))]][]

        # list of hours at day time
        day_hours = seq(0, 23)[!seq(0, 23) %in% night_period[, seq(min(hour), max(hour))]]

        # day table generation
        day_data = night_period[, .(.id, day_departure, date, phase = "day")][rep(1, 24 - nb_night_hours), ][, hour := day_hours][]

        # merge day and night data
        return(rbind(day_data, night_data)[order(.id, day_departure, hour)])
      } else {
        return(x[, .(.id, day_departure, date, phase, hour)])
      }
    })

    # unlist
    ref_phase_day = rbindlist(ref_phase_day_split_list)

    # set date format
    ref_phase_day[, `:=`(
      date = date + hours(hour),
      hour = NULL,
      day_departure = NULL
    )]

    # rolling join
    dataset <- ref_phase_day[dataset, roll = T, on = .(.id, date)]

    # return
    return(dataset)
  } else {
    print("Nothing implemented yet!")
  }
}
