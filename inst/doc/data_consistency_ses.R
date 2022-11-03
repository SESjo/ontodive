## ---- include = FALSE--------------------------------------------------------
# global option relative to rmarkdown
knitr::opts_chunk$set(
  echo = TRUE,
  fig.align = "center",
  out.width = "100%",
  message = FALSE,
  warning = FALSE,
  # tidy = TRUE,
  cache.lazy = FALSE,
  optipng = "-o7 -quiet",
  pngquant = "--speed=1"
)

# library
library(data.table)
library(ggplot2)
library(weanlingNES)
library(kableExtra)
library(magrittr)
library(gt)

# remove some warnings
suppressWarnings(library(ggplot2))

# define my own table format: https://github.com/haozhu233/kableExtra/issues/374
sable <- function(x, escape = T, ...) {
  knitr::kable(x, escape = escape, ...) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "responsive"),
      full_width = F
    )
}

## ----------------------------------------------------------------------------
# summarize data
summary_data = rbindlist(lapply(list.dirs("../inst/extdata/cebc/", full.names = T, recursive = F), function(x){
  # list files
  files_data = list.dirs(x, full.names = F)
  # data.table to store results
  data.table(.id = tolower(last(strsplit(x, split = "/")[[1]])),
             low_res = fifelse("DSA_BasseRes" %in% files_data, T, F),
             high_res = fifelse("DSA_HauteRes" %in% files_data, T, F),
             spot = fifelse("Spot" %in% files_data, T, F))
  
}))

# print
summary_data %>% sable(caption = "Summary information regarding datasets on southern elephant seals")

## ----cache = T---------------------------------------------------------------
# calculate start and end of each file
summary_files = rbindlist(lapply(list.dirs(
  "../inst/extdata/cebc",
  full.names = T,
  recursive = F
), function(x) {
  # list files
  files_data = list.dirs(x, full.names = F)
  
  # id of the seal
  ind_name = last(strsplit(x, split = "/")[[1]])
  
  # check for high resolution
  if ("DSA_HauteRes" %in% files_data) {
    # list csv files
    list_files = list.files(paste0(x, "/DSA_HauteRes"),
                            pattern = "*-Archive.csv",
                            full.names = T)
    
    # extract start and end date
    summary_files_inter = rbindlist(lapply(list_files, function(y) {
      # import first col => Time
      df = fread(y, select = 1, fill = T)
      start = df[1, 1][[1]]
      end = df[.N, 1][[1]]
      # export
      return(data.table(
        start_date = start,
        end_date = end,
        file = gsub(".*[.]([^.]+)[-].*", "\\1", y)
      ))
    }))
    # add id seals
    summary_files_inter[, .id := tolower(ind_name)]
    # return
    return(summary_files_inter)
  }
}))

# format into Posixt
summary_files[, `:=` (start_date = as.character(as.POSIXct(start_date, format = "%d/%M/%Y %T", tz = "GMT")),
                      end_date = as.character(as.POSIXct(end_date, format = "%d/%M/%Y %T", tz = "GMT")))]

# reformat the output
summary_files = dcast(summary_files, .id~file, value.var = c("start_date","end_date"))

# reorder the column
setcolorder(summary_files,
            neworder = c(".id",
                         "start_date_wch000","end_date_wch000",
                         "start_date_wch001","end_date_wch001",
                         "start_date_wch002","end_date_wch002",
                         "start_date_wch003","end_date_wch003",
                         "start_date_wch004","end_date_wch004"))
# lets rename column
col_labs <- lapply(colnames(summary_files), function(x) {
  if (x == ".id") {
    return(".id")
  } else {
    ifelse(grepl("start", x), return("Start"), return("End"))
  }
})
names(col_labs) = colnames(summary_files)

## ----------------------------------------------------------------------------
# print
summary_files %>%
  gt() %>%
  tab_header(title = "Gap check within the data on the southern elephant seals") %>%
  tab_spanner(label = "wch000", columns = matches("wch000")) %>%
  tab_spanner(label = "wch001", columns = matches("wch001")) %>%
  tab_spanner(label = "wch002", columns = matches("wch002")) %>%
  tab_spanner(label = "wch003", columns = matches("wch003")) %>%
  tab_spanner(label = "wch004", columns = matches("wch004")) %>%
  cols_label(.list = col_labs) %>%
  fmt_datetime(
  columns = matches("date"),
  date_style = 14,
  time_style = 2
)

## ----------------------------------------------------------------------------
# load the data
ind_140075 = readRDS("../inst/extdata/cebc/Ind_140075/DSA_HauteRes/Pup_140075.rds")

# display the first rows
head(ind_140075) %>%
  sable(caption = "First rows of the data already pre-processed of the individual `ind_140075`")

## ----fig.cap="Time Gap Identification, if there were no time gap, all the dot should be at 1 second. The red line is the date when a new '*.wch' file has been created"----
# let's (i) round datetime to second, (ii) select unique values, (iii) and sort
data_plot = data.table(datetime = ind_140075[, sort(as.POSIXct(unique(substr(V1, 1, 19)), 
                                              format = "%m/%d/%Y %T", 
                                              tz = "GMT"))])

# display difftime
data_plot[, diff_time := c(NA, diff(datetime))] %>%
  ggplot(aes(x = datetime, y = diff_time)) +
  geom_vline(xintercept = as.POSIXct("2015-03-02 16:07:00"), col = "red") +
  geom_point() +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(x = "Date", y = "Time difference between two rows (s)")

## ----fig.cap="Same graph, with a zoom over two days"-------------------------
# zoom over two days
data_plot[datetime %between% c(as.POSIXct("2015-03-01 16:07:00"), as.POSIXct("2015-03-03 16:07:00")),] %>%
  ggplot(aes(x = datetime, y = diff_time)) +
  geom_vline(xintercept = as.POSIXct("2015-03-02 16:07:00"), col = "red") +
  geom_point() +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(x = "Date", y = "Time difference between two rows (s)")

## ----fig.cap="TDR over 6 hours of data recorded"-----------------------------
# zoom over two days
ind_140075[!is.na(V2), datetime := as.POSIXct(V1, format = "%m/%d/%Y %T", tz = "GMT")] %>%
  .[datetime %between% c(as.POSIXct("2015-03-02 12:00:00", tz = "GMT"), as.POSIXct("2015-03-02 18:00:00", tz = "GMT")),] %>%
  ggplot(aes(x = datetime, y = -V6)) +
  geom_point() +
  labs(x = "Date", y = "Depth (m)")

