#' @title Retrieve data
#'
#' @description
#' This function will first look for datasets in the \code{ontodive} package. If datasets
#' are not included, it will then tried to download the dataset based on the DOI
#' pre-reserved by Dryad when submitting the paper. If somehow it still doesn't work
#' you will be asked to enter the DOI of the dataset you're looking for.
#'
#' @param species \code{"all"}, \code{"nes"} or \code{"ses"} to retrieve respectively the
#' dataset for both species or only the one associated with northern
#' or southern elephant seals.
#'
#' @rawNamespace import(data.table, except = c(first,last))
#' @import magrittr
#' @import cli
#' @import rdryad
#'
#' @return A table of class \code{data.table} if \code{species = "all"}, otherwise
#' a list by year and individual
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # retrieve all data
#' get_data("all")
#' }
get_data <- function(species = "all") {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  data <-
    data_nes <-
    data_ses <-
    . <-
    .id <-
    divenumber <-
    maxdepth <-
    dduration <-
    botttime <-
    desctime <-
    descrate <-
    asctime <-
    ascrate <-
    pdi <-
    dwigglesdesc <-
    dwigglesbott <-
    dwigglesasc <-
    totvertdistbot <-
    driftrate <-
    divetype <-
    day_departure <-
    lightatsurf <-
    lat <-
    lon <-
    sp <-
    dist_dep <-
    ssh <-
    psu <-
    vel <-
    temp <-
    bathy <-
    phase <-
    dryad_download <-
    NULL

  # stop if species is neither all, nes, ses
  if (!species %in% c("all", "nes", "ses")) {
    stop(cli_abort('{.strong species} must either be {.field "all"}, {.field "ses"} or {.field "nes"}'))
  }
  # check if all data
  if (species == "all") {
    # if the both dataset are available in ontodive
    if (all(c("data_nes", "data_ses") %in%
      data(package = "ontodive")$results[, "Item"])) {
      # load them
      data("data_nes", package = "ontodive", envir = environment())
      data("data_ses", package = "ontodive", envir = environment())
      # and combine them into one dataset
      dt <- rbind(
        rbindlist(data_nes$year_2018),
        rbindlist(data_ses$year_2014),
        use.names = T,
        fill = T
      )
      # remove unnecessary dataset
      rm(data_ses)
      rm(data_nes)
      # keep only the required columns
      dt <-
        dt[, .(
          .id,
          divenumber,
          date,
          maxdepth,
          dduration,
          botttime,
          desctime,
          descrate,
          asctime,
          ascrate,
          pdi,
          dwigglesdesc,
          dwigglesbott,
          dwigglesasc,
          totvertdistbot,
          driftrate,
          divetype,
          day_departure,
          lightatsurf,
          lat,
          lon,
          dist_dep,
          sp,
          ssh,
          psu,
          vel,
          temp,
          bathy,
          phase
        )]
    }
    # if not in ontodive
    else {
      dt_location <- tryCatch(
        # try to download the file
        dryad_download("10.5061/dryad.tht76hf3t"),
        error = function(e) {
          # build message
          ask_message <- function() {
            cli_ul()
            cli_alert_danger("Failed to load the dataset")
            cli_alert("Please enter a {.strong DOI} to download the associated dataset: ")
          }
          # if it didn't work ask for a doi
          doi <- readline(prompt = ask_message())
          # remove double quote if any
          doi_without_double_quote <- gsub('"', "", doi)
          # and use it to download the file
          return(dryad_download(doi_without_double_quote))
        }
      )
      # then load the first csv file found
      dt <- fread(dt_location[[1]][grep(".csv", dt_location[[1]])])
    }
  } else {
    # check if the species dataset is available in ontodive
    if (paste0("data_", species) %in% data(package = "ontodive")$results[, "Item"]) {
      # load them
      data("data_nes", package = "ontodive", envir = environment())
      data("data_ses", package = "ontodive", envir = environment())
      # and combine them into one dataset
      dt <- rbind(
        rbindlist(data_nes$year_2018),
        rbindlist(data_ses$year_2014),
        use.names = T,
        fill = T
      )
      # remove unnecessary dataset
      rm(data_ses)
      rm(data_nes)
      # keep only the required columns
      dt <-
        dt[, .(
          .id,
          divenumber,
          date,
          maxdepth,
          dduration,
          botttime,
          desctime,
          descrate,
          asctime,
          ascrate,
          pdi,
          dwigglesdesc,
          dwigglesbott,
          dwigglesasc,
          totvertdistbot,
          driftrate,
          divetype,
          day_departure,
          lightatsurf,
          lat,
          lon,
          dist_dep,
          sp,
          ssh,
          psu,
          vel,
          temp,
          bathy,
          phase
        )]
    }
    # if not available in ontodive
    else {
      dt_location <- tryCatch(
        # try to download the file
        suppressMessages(dryad_download("10.5061/dryad.tht76hf3t")),
        error = function(e) {
          # build message
          ask_message <- function() {
            cli_alert_danger("Failed to load the dataset")
            cli_alert("Please enter a {.strong DOI} to download the associated dataset",
              # make it prettier
              id = "DOI: "
            )
          }
          # if it didn't work ask for a doi
          doi <- readline(prompt = ask_message())
          # remove double if any
          doi_without_double_quote <- gsub('"', "", doi)
          # and use it to download the file
          return(suppressMessages(dryad_download(doi_without_double_quote)))
        }
      )
      # then load the first csv file found
      dt <- fread(dt_location[[1]][grep(".csv", dt_location[[1]])])
    }
    # subset on the species
    if (grepl("nes", species)) {
      dt_nes <- dt[sp == "nes", ]
      dt <- list(year_2018 = NULL)
      dt$year_2018 <- split(dt_nes, by = c(".id"))
    } else if (grepl("ses", species)) {
      dt_ses <- dt[sp == "ses", ]
      dt <- list(year_2014 = NULL)
      dt$year_2014 <- split(dt_ses, by = c(".id"))
    }
  }
  # return the loaded dataset
  return(dt)
}
