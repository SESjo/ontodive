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
library(matlabr)
library(geosphere)
library(marmap)

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
    data_high <- readRDS(paste0(x,
                                # high resolution
                                "/DSA_HauteRes/Pup_",
                                # seal number
                                last(strsplit(seal_id, "_")[[1]]),
                                # rds file
                                ".rds"))
    # since ~10Hz, keep only TDR data at 1Hz
    name_column_depth <- colnames(data_high)[6]
    data_high <- data_high[!is.na(get(name_column_depth))]
    # rename column
    colnames(data_high)[1] <- "datetime"
    colnames(data_high)[6] <- "depth"
    # convert date and time
    data_high[, datetime := as.POSIXct(datetime,
                                       format = "%m/%d/%Y %T",
                                       tz = "GMT")]
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
    fwrite(x = data_high[, .(year, month, day, hour, minute, second, depth)],
           file = paste0("./inst/extdata/", seal_id, ".csv"),)
    # list file in Spot folder
    list_files_Spot_folder <- list.files(paste0(x, "/Spot"),
                                         full.names = T,
                                         pattern = "*.csv")
    # import and combine all files location data to be treated
    data_location <-
      rbindlist(lapply(list_files_Spot_folder, fread), fill = T)
    # fit fit_ssm with foieGras
    data_location_output <- location_treatment(data_location)
    # rename id in deploy_id_argos
    names(data_location_output)[names(data_location_output) == "id"] <-
      "deploy_id_argos"
    # export
    fwrite(x = data_location_output[, .(date, lon, lat)],
           file = paste0("./inst/extdata/", seal_id, "_location.csv"),)
  }
})

#'
#' After converting files from \*.rds to \*.csv, we need to process these files
#' using the IKNOS program with MATLAB. This was done using MATLAB R2021b using
#' the following code piece of code:
#'
#+
# get absolute paths to input data files (NOTE: only keeping first two for now)
input_data <- dir("inst/extdata",
                  pattern = "ind_[0-9]{6}.csv",
                  full.names = TRUE) %>%
  sprintf("'%s'", .) %>%
  paste(collapse = ", ")

# Generate Matlab code
iknos_matlab_code <- glue::glue(
  "
  addpath('inst/extdata/IKNOS/');
  names = {{{input_data}}};
  for k = 1:length(names)'
    yt_iknos_da(names{{k}},'year month day hour minute second depth', 10, 2, 20, 'wantfile_yes','is_southern',true);
  end
"
)

# run the matlab code
matlabr::run_matlab_code(iknos_matlab_code)

#'
#' Then, we need to process

# get absolute paths to input data files (NOTE: only keeping first two for now)
input_data_divestat <- dir("inst/extdata",
                           pattern = "ind_[0-9]{6}.*\\DiveStat.csv",
                           full.names = TRUE) %>%
  sort(.) %>%
  sprintf("'%s'", .)
input_data_rawdata <- dir("inst/extdata",
                          pattern = "ind_[0-9]{6}.*raw_data.csv",
                          full.names = TRUE) %>%
  sort(.) %>%
  sprintf("'%s'", .)

# check both object have the same size
stopifnot(length(input_data_divestat) == length(input_data_rawdata))

# paste each object
input_data_divestat <- paste(input_data_divestat, collapse = ", ")
input_data_rawdata <- paste(input_data_rawdata, collapse = ", ")

# Generate Matlab code
divetype_matlab_code <- glue::glue(
  "
  % configure working environment
  addpath('inst/extdata/IKNOS/dive_type');

  % retrieve file name
  names_divestat = {{{input_data_divestat}}};
  names_rawdata = {{{input_data_rawdata}}};

  % loop over individual
  for k = 1:length(names_divestat)

    % setup files name
    DiveStat_name = names_divestat{{k}};
    RawData_name = names_rawdata{{k}};

    % detect data format for import
    RawData_params = detectImportOptions(RawData_name);
    DiveStat_params = detectImportOptions(DiveStat_name);

    % import data
    RawData=readmatrix(RawData_name,RawData_params);
    DiveStat=readmatrix(DiveStat_name,DiveStat_params);

    % run dive typing
    Output=pwr_DiveTyperStep1V3(RawData(1:8:end,:),DiveStat);
    DiveType=pwr_DiveTyperV3(Output);

    % column names of the new columns
    var_test = {{'DriftDiveIndex','DriftRate','BenthicDiveIndex','BenthicDiveVertRate','CornerIndex','ForagingIndex','VerticalSpeed90Perc','VerticalSpeed95Perc','DiveType'}};

    % build a table combining DiveStat + new columns associated with drift dives
    result = array2table([DiveStat DiveType], 'VariableNames',[DiveStat_params.VariableNames var_test]);

    % write table
    writetable(result, [DiveStat_name(1:end-4) '_DiveType.csv']);

  % end of the loop
  end
"
)

# run the matlab code
matlabr::run_matlab_code(divetype_matlab_code)

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
path_raw <- here("inst", "extdata")

# list of diving parameters' files for 2014 southern individuals
list_file_name_data_2014 <- list.files(
  path = path_raw,
  pattern = "^ind.*\\DiveStat_DiveType.csv$",
  full.names = TRUE
)

# list of location files for 2014 southern individuals
list_location_file_name_data_2014 <- list.files(
  path = path_raw,
  pattern = "*location.csv$",
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
name_location_ind_2014 <-
  paste0("ind_", sapply(str_split(sapply(
    str_split(list_location_file_name_data_2014, "/"),
    function(x) {
      tail(x, 1)
    }
  ), "_"), "[[", 2))

# import 2014 files
data_2014 <- lapply(list_file_name_data_2014, fread)
data_location_2014 <-
  lapply(list_location_file_name_data_2014, fread)

# add names
names(data_2014) <- name_ind_2014
names(data_location_2014) <- name_location_ind_2014

# which colnames in common
col_data_2014 <- Reduce(intersect, lapply(data_2014, colnames))

# keep only those common columns names
data_2014 <- lapply(data_2014, function(x) {
  x[, ..col_data_2014,]
})

# test if columns name are all the same across data sets
stopifnot(length(unique(lapply(data_2014, colnames))) == 1)

# reformat colnames
col_data_2014_reformat <- format_col(col_data_2014)

# reformat dataset
data_2014 <- lapply(data_2014, function(x) {
  # colnames
  setnames(x, col_data_2014, col_data_2014_reformat)

  # time
  x[, date := as.POSIXct(paste(year, month, day, hour, min, sec),
                         format = "%Y %m %d %H %M %S",
                         tz = "GMT")]

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
  x[, sp := "ses"]

  # reorder column
  setcolorder(x, c(
    "divenumber",
    "date",
    "day_departure",
    setdiff(colnames(x), c("divenumber", "date", "day_departure"))
  ))
})

# import the already pre-treated ncdf to add temp, ssh, psu and vel
data("data_cop", package = "weanlingNES")

# keep only southern data
data_cop$northern <- NULL

# import the bathymetric data from marmap packages
south_indian <- getNOAA.bathy(
  lon1 = 35,
  lon2 = 115,
  lat1 = -65,
  lat2 = -40,
  resolution = 1
)

# convert into data.frame
south_indian <- setDT(fortify.bathy(south_indian))

# let's only add lat and long to data_2014
data_2014 <- lapply(data_2014, function(x) {
  # check if 2014-ind is also in data_location_2014
  if (!is.null(data_location_2014[[.n()]])) {
    # let's merge with data_location_2014
    x <- data_location_2014[[.n()]] %>%
      .[x, roll = T, on = "date"]
    # then calculate distance since the departure, i.e. the first location
    # 1. add two columns with the coordinate of the first location
    x[, `:=`(lon_dep = first(lon), lat_dep = first(lat))]
    # 2. calculate the distance
    res_inter <- distGeo(as.matrix(x[, .(lon_dep, lat_dep)]),
                         as.matrix(x[, .(lon, lat)]))
    # 3. add the result in dataset
    x[, dist_dep := res_inter]
    # 4. remove lat_dep and lon_dep column
    x[, `:=`(lat_dep = NULL, lon_dep = NULL)]
    # 5. add oceanographic date based on the nearest set of (date, lon, lat)
    x <- x[,
           {
             # number of row from oceanographic date to join
             k <- 1
             # date "xxxx-xx-xx"
             gg <- as.Date(date)
             # k-nearest neighbor
             kn <-
               nabor::knn(data_cop$southern[time == gg, .(lon = longitude,
                                                          lat = latitude)],
                          matrix(c(lon, lat), ncol = 2),
                          k)
             # keep all columns from x
             c(.SD[rep(seq.int(.N), k)],
               # add columns found in data_cop$southern
               data_cop$southern[time == gg][as.vector(kn$nn.idx),
                                             .(temp = thetao,
                                               ssh = zos,
                                               psu = so,
                                               vel)])
           },
           by = .(date)]
    # 6. add bathymetric data based on the nearest set of (lon, lat)
    x <- x[, {
      # number of row from oceanographic date to join
      k <- 1
      # k-nearest neighbor
      kn <- nabor::knn(south_indian[, .(lon = x,
                                        lat = y)],
                       matrix(c(lon, lat), ncol = 2),
                       k)
      # keep all columns from x
      c(.SD[rep(seq.int(.N), k)],
        # add columns found in south_indian
        south_indian[as.vector(kn$nn.idx),
                     .(bathy = z)])
    }]
  } else {
    x
  }
})

# add phase of the day
data_2014 <- split(calc_phase_day(rbindlist(
  data_2014,
  # create a column .id with individual's name
  use.name = TRUE,
  idcol = TRUE,
  fill = TRUE
)), by = ".id")

# merge data sets
data_ses <- list("year_2014" = data_2014)

# export
usethis::use_data(data_ses, overwrite = TRUE)
