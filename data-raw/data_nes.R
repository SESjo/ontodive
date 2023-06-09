#' This script is used to import data from 2016 and 2018 individuals as R files.
#' You might need to edit this file to process new data since it has been
#' created based on 4 individuals for 2018 and 6 individuals for 2016
#'
# import library
library(data.table)
library(stringr)
library(lubridate)
library(magrittr)
library(geosphere)
library(here)
library(ontodive)
library(marmap)

# set the maximum number of threads
setDTthreads(parallel::detectCores())

# set the right locale to deal with date
Sys.setlocale(locale = "C")

# path to dataraw folder
path_raw <- here("inst", "extdata")

# list of files for 2016-individuals
list_file_name_2016 <- list.files(
  path = path_raw,
  pattern = "^LAT290.*csv$",
  full.names = TRUE
)

# extract name of individuals
name_2016 <-
  paste0("ind_", sapply(str_split(sapply(
    str_split(list_file_name_2016, "/"),
    function(x) {
      tail(x, 1)
    }
  ), "_"), "[[", 2))

# import 2016 files
data_2016 <- lapply(list_file_name_2016, fread)

# add names
names(data_2016) <- name_2016

# which colnames in common
col_2016 <- Reduce(intersect, lapply(data_2016, colnames))

# keep only those common columns names
data_2016 <- lapply(data_2016, function(x) {
  x[, ..col_2016, ]
})

# test if columns name are all the same across data sets
stopifnot(length(unique(lapply(data_2016, colnames))) == 1)

# reformat
col_2016_reformat <- format_col(col_2016)
data_2016 <- lapply(data_2016, function(x) {
  # colnames
  setnames(x, col_2016, col_2016_reformat)
  # time
  x[, date := as.POSIXct(paste(date, time),
    format = "%m/%d/%Y %H:%M:%S",
    tz = "GMT"
  )]
})

# list of diving parameters' files for 2018-individuals
list_file_name_data_2018 <- list.files(
  path = path_raw,
  pattern = "^2018.*RSB.csv$",
  full.names = TRUE
)

# list of gps data for 2018-individuals
list_gps_file_name_data_2018 <- list.files(
  path = path_raw,
  pattern = "^2018.*GPE3.csv$",
  full.names = TRUE
)

# extract name of individuals
name_ind_2018 <-
  paste0("ind_", sapply(str_split(sapply(
    str_split(list_file_name_data_2018, "/"),
    function(x) {
      tail(x, 1)
    }
  ), "_"), "[[", 1))
name_gps_2018 <-
  paste0("ind_", sapply(str_split(sapply(
    str_split(list_gps_file_name_data_2018, "/"),
    function(x) {
      tail(x, 1)
    }
  ), "_"), "[[", 1))

# import 2018 files
data_2018 <- lapply(list_file_name_data_2018, fread)
gps_2018 <- lapply(list_gps_file_name_data_2018, fread, skip = 5)

# which colnames in common
col_data_2018 <- Reduce(intersect, lapply(data_2018, colnames))
col_gps_2018 <- Reduce(intersect, lapply(gps_2018, colnames))

# keep only those common columns names
data_2018 <- lapply(data_2018, function(x) {
  x[, ..col_data_2018, ]
})
gps_2018 <- lapply(gps_2018, function(x) {
  x[, ..col_gps_2018, ]
})

# test if columns name are all the same across data sets
stopifnot(length(unique(lapply(data_2018, colnames))) == 1)

# reformat colnames
col_data_2018_reformat <- format_col(col_data_2018)
col_gps_2018_reformat <- format_col(col_gps_2018)

# add names
names(data_2018) <- name_ind_2018
names(gps_2018) <- name_gps_2018

# reformat dataset
data_2018 <- lapply(data_2018, function(x) {
  # colnames
  setnames(x, col_data_2018, col_data_2018_reformat)
  # time
  x[, date := as.POSIXct(paste(year, month, day, hour, min, sec),
    format = "%Y %m %d %H %M %S",
    tz = "GMT"
  )]
  # convert divetype
  x[, divetype := as.character(divetype)]
  x %>%
    .[divetype == "0", divetype := "0: transit"] %>%
    .[divetype == "1", divetype := "1: foraging"] %>%
    .[divetype == "2", divetype := "2: drift"] %>%
    .[divetype == "3", divetype := "3: benthic"]

  # number of days since departure
  x[, day_departure := as.numeric(ceiling(difftime(date,
    # to make sure xxxx-xx-xx 00:00:00 is the same day as xxxx-xx-xx 00:00:01
    first(
      as.Date(date) - seconds(1)
    ),
    units = "days"
  )))]

  # add sp column
  x[, sp := "nes"]
})
gps_2018 <- lapply(gps_2018, function(x) {
  # colnames
  setnames(x, col_gps_2018, col_gps_2018_reformat)
  # time
  if (all(x[, unique(nchar(date))] == 20)) {
    # reformat
    x[, `:=`(
      date = as.POSIXct(date,
        format = "%d-%b-%Y %H:%M:%S",
        tz = "GMT"
      ),
      sunset = as.POSIXct(sunset,
        format = "%d-%b-%Y %H:%M:%S",
        tz = "GMT"
      ),
      sunrise = as.POSIXct(sunrise,
        format = "%%d-%b-%Y %H:%M:%S",
        tz = "GMT"
      )
    )]
  } else {
    # warning
    warning(paste("Pleae make sure the date has been well formatted for", .n()))
    # reformat
    x[, `:=`(
      date = as.POSIXct(date,
        format = "%m/%d/%Y %H:%M",
        tz = "GMT"
      ),
      sunset = as.POSIXct(sunset,
        format = "%m/%d/%Y %H:%M",
        tz = "GMT"
      ),
      sunrise = as.POSIXct(sunrise,
        format = "%m/%d/%Y %H:%M",
        tz = "GMT"
      )
    )]
  }
})

# import the already pre-treated ncdf to add temp, ssh, psu and vel
data("data_cop", package = "ontodive")

# keep northern data
data_cop$southern <- NULL

# clear memory
gc()

# import the bathymetric data from marmap packages
north_pacific <- getNOAA.bathy(
  lon1 = -165,
  lon2 = -115,
  lat1 = 55,
  lat2 = 25,
  resolution = 1
)

# convert into data.frame
north_pacific <- setDT(fortify.bathy(north_pacific))

# let's only add lat and long to data_2018
data_2018 <- lapply(data_2018, function(x) {
  # check if 2018-ind is also in gps_2018
  if (!is.null(gps_2018[[.n()]])) {
    # let's merge with gps_2018
    x <- gps_2018[[ontodive:::.n()]] %>%
      .[, c(
        "date",
        "mostlikelylatitude",
        "mostlikelylongitude"
      )] %>%
      .[x, roll = T, on = "date"] %>%
      .[, `:=`(
        lat = mostlikelylatitude,
        lon = mostlikelylongitude,
        mostlikelylatitude = NULL,
        mostlikelylongitude = NULL
      )]
    # then calculate distance since the departure, i.e. the first location
    # 1. add two columns with the coordinate of the first location
    x[, `:=`(lon_dep = first(lon), lat_dep = first(lat))]
    # 2. calculate the distance
    res_inter <- distGeo(
      as.matrix(x[, .(lon_dep, lat_dep)]),
      as.matrix(x[, .(lon, lat)])
    )
    # 3. add the result in dataset
    x[, dist_dep := res_inter]
    # 4. remove lat_dep and lon_dep column
    x[, `:=`(lat_dep = NULL, lon_dep = NULL)]
    # 5. add oceanographic data based on the nearest set of (date, lon, lat)
    x <- x[,
      {
        # number of row from oceanographic date to join
        k <- 1
        # date "xxxx-xx-xx"
        gg <- as.Date(date)
        # k-nearest neighbor
        kn <- nabor::knn(
          data_cop$northern[time == gg, .(
            lon = longitude,
            lat = latitude
          )],
          matrix(c(lon, lat), ncol = 2),
          k
        )
        # keep all columns from x
        c(
          .SD[rep(seq.int(.N), k)],
          # add columns found in data_cop$northern
          data_cop$northern[time == gg][
            as.vector(kn$nn.idx),
            .(
              temp = thetao,
              ssh = zos,
              psu = so,
              vel
            )
          ]
        )
      },
      by = .(date)
    ]
    # 6. add bathymetric data based on the nearest set of (lon, lat)
    x <- x[, {
      # number of row from oceanographic date to join
      k <- 1
      # k-nearest neighbor
      kn <- nabor::knn(
        north_pacific[, .(
          lon = x,
          lat = y
        )],
        matrix(c(lon, lat), ncol = 2),
        k
      )
      # keep all columns from x
      c(
        .SD[rep(seq.int(.N), k)],
        # add columns found in north_pacific
        north_pacific[
          as.vector(kn$nn.idx),
          .(
            bathy = z
          )
        ]
      )
    }]
  } else {
    x
  }
})

# add phase of the day
data_2018 <- split(calc_phase_day(rbindlist(
  data_2018,
  use.name = TRUE, idcol = TRUE, fill = TRUE
)), by = ".id")

# merge data sets
data_nes <- list(
  "year_2016" = data_2016,
  "year_2018" = data_2018
)

# export
usethis::use_data(data_nes, overwrite = TRUE)
