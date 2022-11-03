#' @title Treat location data with a continuous-time state-space model
#'
#' @description Using `fit_ssm` function from `foieGras` package, this function "clean" the
#' location data to be used for further analysis at the dive scale.
#'
#' @param data Dataset of observation, usually the file \*Argos.csv or
#' \*Location.csv files
#' @param model Choose to fit either a simple random walk ("rw") or correlated
#' random walk ("crw") as a continuous-time process model
#' @param time.step options: 1) the regular time interval, in hours, to predict
#' to; 2) a vector of prediction times, possibly not regular, must be specified as a data.frame with id and POSIXt dates; 3) NA - turns off prediction and locations are only estimated at observation times.
#' @param vmax The max travel rate (m/s) passed to sda to identify outlier locations
#' @param with_plot A diagnostic plot
#' @param export To export the new generated dataset
#'
#' @return A dataset with the new location data
#' @export
#'
#' @references
#' run_foieGras_generic.R (\email{tkeates@ucsc.edu})
#'
#' \href{https://ianjonsen.github.io/foieGras/}{https://ianjonsen.github.io/foieGras/}
#'
#' @rawNamespace import(data.table, except = c(first,last))
#' @import magrittr
#' @import ggplot2
#' @import foieGras
#' @importFrom lubridate is.POSIXt is.POSIXct is.POSIXlt
#'
#' @seealso \code{\link[foieGras]{fit_ssm}}
#'
#' @examples
#' # load library
#' library(foieGras)
#' library(data.table)
#'
#' # run this function on sese1 dataset included in foieGras package
#' output <- location_treatment(copy(sese1), with_plot = TRUE)
#'
location_treatment <- function(data,
                               model = "crw",
                               time.step = 1,
                               vmax = 3,
                               with_plot = FALSE,
                               export = NULL) {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  Date <-
    Latitude <-
    Longitude <-
    . <-
    DeployID <-
    Quality <-
    `Error Semi-major axis` <-
    `Error Semi-minor axis` <-
    `Error Ellipse orientation` <-
    origin <-
    region <-
    lon <-
    lat <-
    id <-
    NULL
  # checks data is a data.table, otherwise convert it
  if (!weanlingNES::check_dt(data)) {
    setDT(data)
  }

  # check if data already formated
  if (!all(c("id", "date", "lc", "lon", "lat") %in% colnames(data))) {
    # check if the Date column is in Posix
    if (!is.POSIXct(data[, Date]) |
      !is.POSIXlt(data[, Date]) |
      !is.POSIXt(data[, Date])) {
      # convert into Posix
      data[, Date := as.POSIXct(Date, tz = "GMT", format = "%T %d-%b-%Y")]
    }

    # keep and rename only what's necessary
    data <- data[!is.na(Latitude), .(
      id = DeployID,
      date = Date,
      lc = Quality,
      lon = Longitude,
      lat = Latitude,
      smaj = `Error Semi-major axis`,
      smin = `Error Semi-minor axis`,
      eor = `Error Ellipse orientation`
    )]
  }

  # run state space model- edit time step to interpolate to (in hours) and speed filter (in m/s) as desired:
  fitc <- fit_ssm(data,
    model = model,
    time.step = time.step,
    vmax = vmax
  )

  # retrieve location data from the fit
  output <- setDT(grab(fitc, "p", as_sf = FALSE))

  # export if required
  if (!is.null(export)) {
    # export
    fwrite(output, export)
  }

  # build plot if required
  if (with_plot) {
    # combine the observed and predict dataset to build the plot
    dataPlot <- rbind(data[][, origin := "Observed"],
      output[][, origin := "Predicted"],
      fill = T
    )

    # retrieve map
    world <- map_data("world")

    # plot
    p1 <- ggplot() +
      geom_map(
        data = world,
        map = world,
        aes(map_id = region)
      ) +
      coord_sf(
        xlim = c(dataPlot[, min(lon) - 5], dataPlot[, max(lon) + 5]),
        ylim = c(dataPlot[, min(lat) - 5], dataPlot[, max(lat) + 5]),
        expand = FALSE
      ) +
      geom_point(data = dataPlot, aes(
        x = lon,
        y = lat,
        color = date
      )) +
      facet_wrap(. ~ origin) +
      labs(
        x = "Latitude",
        y = "Longitude",
        color = "Date",
        title = paste0("ID: ", dataPlot[1, id])
      ) +
      weanlingNES::theme_jjo() +
      theme(legend.position = "bottom")

    # return
    print(p1)
  }
  # return
  return(output)
}
