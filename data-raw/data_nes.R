#' This script is used to import data from 2016 and 2018 individuals as R files.
#' You might need to edit this file to process new data since it has been
#' created based on 4 individuals for 2018 and 6 individuals for 2016
#'
# import library
library(data.table)
library(stringr)

# set the maximum number of threads
setDTthreads(parallel::detectCores())

# path to dataraw
path_raw <- system.file("extdata",
                        package = "weanlingNES")

# list of files for 2016-individuals
list_2016 <- list.files(path = path_raw,
                        pattern = "^LAT290.*csv$",
                        full.names = TRUE)

# extract name of individuals
name_2016 <-
  paste0("ind_", sapply(str_split(sapply(str_split(list_2016, "/"),
                                         function(x) {
                                           tail(x, 1)
                                         }), "_"), "[[", 2))

# import 2016 files
data_2016 <- lapply(list_2016, fread)

# add names
names(data_2016) <- name_2016

# which colnames in common
col_2016 <- Reduce(intersect, lapply(data_2016, colnames))

# keep only those common columns names
data_2016 <- lapply(data_2016, function(x) {
  x[, ..col_2016,]
})

# test if columns name are all the same across data sets
stopifnot(length(unique(lapply(data_2016, colnames))) == 1)

# reformat
col_2016_reformat <- format_col(col_2016)
data_2016 <- lapply(data_2016, function(x) {
  # colnames
  setnames(x, col_2016, col_2016_reformat)
  # time
  x[, date := as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M:%S")]
})

# list of files for 2018-individuals
list_2018 <- list.files(path = path_raw,
                        pattern = "^2018.*csv$",
                        full.names = TRUE)

# extract name of individuals
name_2018 <-
  paste0("ind_", sapply(str_split(sapply(str_split(list_2018, "/"),
                                         function(x) {
                                           tail(x, 1)
                                         }), "_"), "[[", 1))

# import 2018 files
data_2018 <- lapply(list_2018, fread)

# which colnames in common
col_2018 <- Reduce(intersect, lapply(data_2018, colnames))

# keep only those common columns names
data_2018 <- lapply(data_2018, function(x) {
  x[, ..col_2018,]
})

# test if columns name are all the same across data sets
stopifnot(length(unique(lapply(data_2018, colnames))) == 1)

# reformat
col_2018_reformat <- format_col(col_2018)
data_2018 <- lapply(data_2018, function(x) {
  # colnames
  setnames(x, col_2018, col_2018_reformat)
  # time
  x[, date := as.POSIXct(paste(year, month, day, hour, min, sec),
                         format = "%Y %m %d %H %M %S")]
  # convert divetype
  x[,divetype:=as.character(divetype)]
  x[divetype == "0", divetype := "0: transit"
  ][divetype == "1", divetype := "1: foraging"
  ][divetype == "2", divetype := "2: drift"
  ][divetype == "3", divetype := "3: benthic"]

  # number of days since departure
  x[, day_departure := ceiling(difftime(date,
                                        first(date),
                                        units = "days"))]

})

# add names
names(data_2018) <- name_2018



# merge data sets
data_nes <- list("year_2016" = data_2016,
                 "year_2018" = data_2018)

# export
usethis::use_data(data_nes, overwrite = TRUE)
