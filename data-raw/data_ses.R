#' ---
#' title: "Process 2014 Weanling Southern Individual Data"
#' author: "Joffrey Joumaa"
#' date: "01/03/2022"
#' ---
#'
#' This script is used to import data from southern individuals. For that, we're
#' going to first convert \*.rds file into \*.csv. This \*.rds files can be
#' retrieved from CEBC server at this location:
#' /EarlyLife/PostDoc Sam Cox/SES_PupData/Processing_DSAhaute/DataOut_2014-2015/One_RawDataCombined
#' please ask Baptiste Picard (baptiste.picard@cebc.cnrs.fr) to get access to
#' this server. Once the \*.rds files have been processed into \*.csv, we're
#' using the IKNOS program to end up with a table where each row is a dive.
#'
#+
# let's import the required packages
library(data.table)
library(here)
library(stringr)
library(lubridate)
library(magrittr)
library(weanlingNES)

# set the maximum number of threads
setDTthreads(parallel::detectCores())

# set the right locale to deal with date
Sys.setlocale(locale = "C")

# convert \*.rds into \*.csv
lapply(list.dirs(
  "./inst/extdata/cebc",
  full.names = T,
  recursive = F
), function(x) {
  # print
  print(x)
  # list files
  files_data <- list.dirs(x, full.names = F)
  # seal name
  seal_id <- tolower(last(strsplit(x, "/")[[1]]))
  # data.table to store results
  if ("DSA_HauteRes" %in% files_data) {
    # import *.rds files
    data_high <- readRDS(paste0(
      x,
      # high resolution
      "/DSA_HauteRes/Pup_",
      # seal number
      last(strsplit(seal_id, "_")[[1]]),
      # rds file
      ".rds"
    ))
    # since ~10Hz, keep only TDR data at 1Hz
    name_column_depth <- colnames(data_high)[6]
    data_high <- data_high[!is.na(get(name_column_depth))]
    # rename column
    colnames(data_high)[1] <- "datetime"
    colnames(data_high)[6] <- "depth"
    # convert date and time
    data_high[, datetime := as.POSIXct(datetime,
      format = "%m/%d/%Y %T",
      tz = "GMT"
    )]
    # create new columns required to run IKNOS program
    data_high[, `:=`(
      year = year(datetime),
      month = month(datetime),
      day = lubridate::day(datetime),
      hour = hour(datetime),
      minute = minute(datetime),
      second = second(datetime)
    )]
    # write data as output
    fwrite(
      x = data_high[, .(year, month, day, hour, minute, second, depth)],
      file = paste0("./inst/extdata/", seal_id, ".csv"),
    )
  }
})

#'
#' After converting files from \*.rds to \*.csv, we need to process these files
#' using the IKNOS program with MATLAB. This was done using MATLAB R2021b using
#' the following code piece of code:
#'
#' `names = {'ind_140059.csv','ind_140060.csv','ind_140062.csv','ind_140063.csv','ind_140068.csv','ind_140069.csv','ind_140072.csv' 'ind_140073.csv' 'ind_140075.csv'};
#' for k = 1:length(names)
#'   yt_iknos_da(names{k},'year month day hour minute second depth', 10, 2, 20, 'wantfile_yes','is_southern',true);
#' end`
#'
#' Once processed, we end it up with \*.csv files where each row is a dive.
#' We're going to processed these files the same way we did for the northern
#' individual (in 2018, cf ./data_raw/data_nes.R)
#'
#+
# before manipulating these new file, we might need to make sure we have the
# right file permissions set-up
system("chmod ugo+rwx ./inst/extdata/*.csv")

# path to dataraw folder
path_raw <- here("inst","extdata")

# list of diving parameters' files for 2014 southern individuals
list_file_name_data_2014 <- list.files(
  path = path_raw,
  pattern = "^ind.*\\DiveStat.csv$",
  full.names = TRUE
)

# extract name of individuals
name_ind_2014 <-
  paste0("ind_", sapply(str_split(sapply(
    str_split(list_file_name_data_2014, "/"),
    function(x) {
      tail(x, 1)
    }
  ), "_"), "[[", 2))

# import 2014 files
data_2014 <- lapply(list_file_name_data_2014, fread)

# which colnames in common
col_data_2014 <- Reduce(intersect, lapply(data_2014, colnames))

# keep only those common columns names
data_2014 <- lapply(data_2014, function(x) {
  x[, ..col_data_2014, ]
})

# test if columns name are all the same across data sets
stopifnot(length(unique(lapply(data_2014, colnames))) == 1)

# reformat colnames
col_data_2014_reformat <- format_col(col_data_2014)

# add names
names(data_2014) <- name_ind_2014

# reformat dataset
data_2014 <- lapply(data_2014, function(x) {
  # colnames
  setnames(x, col_data_2014, col_data_2014_reformat)
  # time
  x[, date := as.POSIXct(paste(year, month, day, hour, min, sec),
                         format = "%Y %m %d %H %M %S",
                         tz = "GMT"
  )]
  # # convert divetype
  # x[, divetype := as.character(divetype)]
  # x %>%
  #   .[divetype == "0", divetype := "0: transit"] %>%
  #   .[divetype == "1", divetype := "1: foraging"] %>%
  #   .[divetype == "2", divetype := "2: drift"] %>%
  #   .[divetype == "3", divetype := "3: benthic"]

  # number of days since departure
  x[, day_departure := as.numeric(ceiling(difftime(date,
                                                   # to make sure xxxx-xx-xx 00:00:00 is the same day as xxxx-xx-xx 00:00:01
                                                   first(
                                                     as.Date(date) - seconds(1)
                                                   ),
                                                   units = "days"
  )))]
})

# merge data sets
data_ses <- list("year_2014" = data_2014)

# export
usethis::use_data(data_ses, overwrite = TRUE)
