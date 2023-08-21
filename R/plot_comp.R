#' @title Plot the evolution of a diving parameter accross time between two populations
#'
#' @description This function approximates the relationship between the
#' considered diving behavior and time in order to better represent this
#' evolution by a smooth curve, rather than by a scatterplot.
#'
#' @details This function fits a GAM with the species as a grouping factor and a random
#' effect (intercept + slope) on the individual (\emph{i.e.} diving parameter ~
#' species + s(time) + (1 time | individual). This allows to represent a curve
#' for each species, but also to access to the curve associated with each
#' individual.
#'
#' @param data A dataset containing the required information
#' @param diving_parameter The colname associated with the diving parameter to represent
#' @param group_to_compare The colname associated with the groups to compare
#' @param time The colname associated with time
#' @param id The colname associated with individual
#' @param nb_days How many days to represent
#' @param bs Smooth terms in GAM
#' @param k The dimension of the basis used to represent the smooth term
#' @param alpha_point The transparency of the point
#' @param alpha_ribbon The transparency of the ribbon
#' @param linetype_ribbon Line type for ribbon border
#' @param colours The colours to use
#' @param ribbon Should confidence interval be added
#' @param point Should the points be displayed
#' @param individual Should individuals curves be displayed
#' @param populational Should populational curve be displayed
#' @param rows The colname used for a facet in row
#' @param cols The colname used for a facet in column
#' @param scales Are scales shared across all facets (the default, "fixed")
#' @param method 	The smoothing parameter estimation method for the GAM (default \code{REML})
#' @param export_data_model Boolean to export the data of the underlying model
#'
#' @seealso \code{\link[mgcv]{smooth.terms}}
#' @seealso \code{\link[mgcv]{gam}}
#'
#' @return Return a ggplot
#'
#' @export
#'
#' @rawNamespace import(data.table, except = c(first,last))
#' @import magrittr
#' @import ggplot2
#' @import mgcv
#' @import scales
#' @import ggh4x
#' @import itsadug
#' @importFrom purrr pmap
#'
#' @references \href{https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred}{https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred}
#' @references \url{https://github.com/DistanceDevelopment/dsm/wiki/Why-is-the-default-smoothing-method-\%22REML\%22-rather-than-\%22GCV.Cp\%22\%3F}
#' @references James Grecian. (2022). jamesgrecian/harpPup: v1.0 (v1.0). Zenodo. \href{https://doi.org/10.5281/zenodo.5901391}{https://doi.org/10.5281/zenodo.5901391}
#'
#' @examples
#' \dontrun{
#' # load data
#' data_nes <- get_data("nes")
#' data_ses <- get_data("ses")
#'
#' # combine
#' data_comp <- rbind(
#'   rbindlist(data_nes$year_2018),
#'   rbindlist(data_ses$year_2014),
#'   use.names = T,
#'   fill = T
#' )
#'
#' # plot
#' plot_comp(data_comp, "maxdepth")
#' }
#'
plot_comp <- function(data,
                      diving_parameter = NULL,
                      group_to_compare = "sp",
                      time = "day_departure",
                      id = ".id",
                      nb_days = 100,
                      bs = "cs",
                      k = 6,
                      alpha_point = 0.01,
                      alpha_ribbon = 0.4,
                      linetype_ribbon = 2,
                      colours = NULL,
                      ribbon = TRUE,
                      point = TRUE,
                      individual = TRUE,
                      populational = TRUE,
                      rows = NULL,
                      cols = NULL,
                      scales = "fixed",
                      method = "REML",
                      export_data_model = FALSE) {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  . <-
    fit_ind <-
    fit_pop <-
    fit_pop_se <-
    NULL

  # checks if data is a data.table, otherwise convert it
  if (!ontodive::check_dt(data)) {
    # convert
    setDT(data)
  }

  # make sure a diving_parameter is set up
  if (is.null(diving_parameter)) {
    stop("Please choose the diving parameter you want to represent")
  }

  # make sure "rows" contains only one column name
  if (length(rows) != 1 & !is.null(rows)) {
    stop("Please, used only one column name to build the facet in row")
  }

  # make sure "cols" contains only one column name
  if (length(rows) != 1 & !is.null(rows)) {
    stop("Please, used only one column name to build the facet in col")
  }

  # make sure the parameter can be associated with columns in data
  if (!all(c(
    diving_parameter,
    group_to_compare,
    time,
    id,
    rows,
    cols
  ) %in% colnames(data))) {
    stop(
      paste0(
        "Please make sure the parameters you've entered correspond to ",
        # https://stackoverflow.com/questions/16742951/print-dataframe-name-in-function-output
        deparse(substitute(data)),
        "'s column names"
      )
    )
  }

  # keep only the relevant columns
  col_to_keep <-
    c(group_to_compare, time, id, diving_parameter, rows, cols)
  data <- data[, col_to_keep, with = FALSE]

  # rename colnames to match the rest of the function
  names(data)[names(data) == group_to_compare] <- "group_to_compare"
  names(data)[names(data) == time] <- "time"
  names(data)[names(data) == id] <- "id"
  names(data)[names(data) == diving_parameter] <- "diving_parameter"

  # if colour is set
  if (!is.null(colours)) {
    # and if the number of group_to_compare is no equal to the number of colours
    if (data[, uniqueN(group_to_compare)] != length(colours)) {
      # then stop to enter the proper number of colour
      stop(
        paste0(
          "Please enter the same number of colours as the number of ",
          group_to_compare
        )
      )
    }
    # if not
  } else {
    # and if the number of group_to_compare is two then
    if (data[, uniqueN(group_to_compare)] == 2) {
      # use this two colour
      colours <- c("#e08214", "#8073ac")
      # otherwise
    } else {
      # set the colour based on the number of group_to_compare
      colours <- hue_pal()(data[, uniqueN(group_to_compare)])
    }
  }

  # if facet required
  if (!is.null(rows) | !is.null(cols)) {
    # split the data set according rows and cols
    data_split <- split(data, by = c(rows, cols))
    # otherwise
  } else {
    # put data into a list to be able to run the following code
    data_split <- list(data)
  }

  # apply the gam to each dataset
  res_pred_list <- lapply(data_split, function(data) {
    # fit BAM with autocorrelation
    # https://jacolienvanrij.com/Tutorials/GAMM.html
    # https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred
    mdl_tot_wo_autocor <- bam(
      diving_parameter ~ group_to_compare +
        s(time, bs = bs, k = k) +
        s(
          time,
          by = group_to_compare,
          m = 1,
          bs = bs,
          k = k
        ) +
        s(id, bs = "re") +
        s(time, id, bs = "re"),
      data = data[, .(
        id = as.factor(id),
        time,
        diving_parameter,
        group_to_compare = as.factor(group_to_compare)
      )],
      family = "gaussian",
      # default "GCV.Cp" was faster but didn't make sense for some specific case
      # group_to_compare "maxdepth", diving_parameter "maxdepth", cols "sp"
      # https://github.com/DistanceDevelopment/dsm/wiki/Why-is-the-default-smoothing-method-%22REML%22-rather-than-%22GCV.Cp%22%3F
      method = method
    )

    # identify the start of each animals' trip
    simdat <- start_event(data[, .(
      id = as.factor(id),
      time,
      diving_parameter,
      group_to_compare = as.factor(group_to_compare)
    )], column = "time", event = "id")

    # gets de first order value, i.e. lag1 (acf[2])
    valRho <- acf(resid(mdl_tot_wo_autocor), plot=FALSE)$acf[2]

    # refit taking into account autocorrelation
    mdl_tot <- bam(
      diving_parameter ~ group_to_compare +
        s(time, bs = bs, k = k) +
        s(
          time,
          by = group_to_compare,
          m = 1,
          bs = bs,
          k = k
        ) +
        s(id, bs = "re") +
        s(time, id, bs = "re"),
      data = data[, .(
        id = as.factor(id),
        time,
        diving_parameter,
        group_to_compare = as.factor(group_to_compare)
      )],
      family = "gaussian",
      AR.start=simdat$start.event,
      rho=valRho,
      # default "GCV.Cp" was faster but didn't make sense for some specific case
      # group_to_compare "maxdepth", diving_parameter "maxdepth", cols "sp"
      # https://github.com/DistanceDevelopment/dsm/wiki/Why-is-the-default-smoothing-method-%22REML%22-rather-than-%22GCV.Cp%22%3F
      method = method
    )

    # new dataset generation for individual level (column time, id)
    ind_pred_inter <- expand.grid(
      time = 0:nb_days,
      id = unique(data$id)
    )

    # add group_to_compare column
    ind_pred <-
      data[, .(group_to_compare = unique(group_to_compare)), by = id] %>%
      # merge with prediction to add group_to_compare column
      merge(ind_pred_inter, ., by = c("id")) %>%
      # convert as data.table
      setDT(.) %>%
      # sort by group_to_compare, time and id
      .[order(group_to_compare, time, id), ] %>%
      # add individual prediction
      .[, fit_ind := predict.gam(mdl_tot,
        .SD,
        type = "response"
      )] %>%
      # sort by group_to_compare, time and id
      .[order(group_to_compare, time, id), ] %>%
      # trick to avoid calling twice this object in the console for display
      .[]

    # new dataset generation for population level (column time)
    pop_pred <- setDT(
      expand.grid(
        time = 0:nb_days,
        group_to_compare = unique(data$group_to_compare),
        id = "population_level",
        stringsAsFactors = FALSE
      )
    ) %>%
      # sort by group_to_compare, time and id
      .[order(group_to_compare, time, id), ] %>%
      # retrieve population prediction
      .[, fit_pop := predict.gam(mdl_tot,
        # select the first individual by group_to_compare
        ind_pred[, .SD[id == first(id)], by = group_to_compare],
        type = "response",
        exclude = c(
          "s(id)",
          "s(time,id)"
        )
      )] %>%
      # add standard error at the population level
      .[, fit_pop_se := predict.gam(
        mdl_tot,
        # select the first individual by group_to_compare
        ind_pred[, .SD[id == first(id)], by = group_to_compare],
        type = "response",
        exclude = c(
          "s(time,id)",
          "s(id)"
        ),
        se.fit = T
      )$se.fit] %>%
      # trick to avoid calling twice this object in the console for display
      .[]

    # add facet if rows is not NULL
    if (!is.null(rows)) {
      # to population level dataset
      pop_pred[, (rows) := data[, unique(get(rows))]]
      # and individual level dataset
      ind_pred[, (rows) := data[, unique(get(rows))]]
    }

    # add facet if cols is not NULL
    if (!is.null(cols)) {
      # to population level dataset
      pop_pred[, (cols) := data[, unique(get(cols))]]
      # and individual level dataset
      ind_pred[, (cols) := data[, unique(get(cols))]]
    }

    # return
    return(list(pop_pred, ind_pred))
  })

  # consolidate result
  res_pred <- pmap(res_pred_list, rbind)

  # retrieve pop level result
  pop_pred <- res_pred[[1]]

  # retrieve individual level result
  ind_pred <- res_pred[[2]]

  # plot
  p1 <- ggplot()

  # if point
  if (point) {
    # intiate with geom_point
    p1 <- p1 +
      # add individuals points
      geom_point(
        aes(x = time, y = diving_parameter, colour = group_to_compare),
        data = data[time <= nb_days, ],
        alpha = alpha_point,
        # to make sure having a point without border
        # https://stackoverflow.com/questions/34398418/geom-point-borders-in-ggplot2-2-0-0
        shape = 16,
        size = 1
      ) +
      theme(legend.position = "none") +
      scale_x_continuous(
        limits = c(0, nb_days),
        oob = scales::squish,
        expand = c(0, 0)
      )
  }

  # if ribbon
  if (ribbon) {
    p1 <- p1 +
      # add confidence interval for populational model
      geom_ribbon(
        aes(
          x = time,
          # min of confidence interval at 95%
          ymin = fit_pop - (1.96 * fit_pop_se),
          # max of confidence interval at 95%
          ymax = fit_pop + (1.96 * fit_pop_se),
          group = group_to_compare,
          fill = group_to_compare
        ),
        # since pop fit is
        data = pop_pred,
        alpha = alpha_ribbon,
        color = "darkgrey",
        linetype = linetype_ribbon,
        outline.type = "both"
      )
  }

  # if individual
  if (individual) {
    p1 <- p1 +
      # add individuals lines
      geom_line(
        aes(
          x = time,
          y = fit_ind,
          group = interaction(group_to_compare, id),
          colour = group_to_compare
        ),
        data = ind_pred,
        alpha = 0.4
      )
  }

  # if populational
  if (populational) {
    p1 <- p1 +
      # add populational line
      geom_line(
        aes(
          x = time,
          y = fit_pop,
          group = group_to_compare,
          colour = group_to_compare
        ),
        data = pop_pred,
        size = 1
      )
  }

  p1 <- p1 +
    scale_fill_manual(values = colours) +
    scale_color_manual(values = colours) +
    # use "vars(get())" to setup facet since "facet_grid" needs characters or colnames
    facet_grid2(
      rows = if (is.null(rows)) {
        NULL
      } else {
        vars(get(rows))
      },
      cols = if (is.null(cols)) {
        NULL
      } else {
        vars(get(cols))
      },
      scales = scales,
      # Horizontal strips
      strip = strip_themed(
        background_x = elem_list_rect(fill = "grey")
      )
    )

  # if we want to export the data from the model
  if (export_data_model) {
    # return
    return(list(p1, list(pop_pred, ind_pred)))
  } else {
    # return
    return(p1)
  }
}
