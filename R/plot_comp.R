#' @title Plot the evolution of a diving parameter accross time between two populations
#'
#' @description This function approximate the relationship between the considered diving
#' behavior and time in order to better represent this evolution with a smooth
#' curve, rather than scatterplot
#'
#' @details This function fits a GAM with the species as a grouping factor and a random
#' effect (intercept + slope) on the individual (*i.e.* diving parameter ~
#' species + s(time) + (1 time | individual). This allows to represent a curve
#' for each species, but also to access to the curve associated with each
#' individual.
#'
#' @param data A dataset containing the required information
#' @param diving_parameter The colname associated with the diving parameter to represent
#' @param sp The colname associated with species
#' @param time The colname associated with time
#' @param id The colname associated with individual
#' @param nb_days How many days to represent
#' @param bs Smooth terms in GAM
#' @param k The dimension of the basis used to represent the smooth term
#' @param alpha_point The transparency of the point
#'
#' @seealso \code{\link[mgcv]{smooth.terms}}
#' @seealso \code{\link[mgcv]{gam}}
#'
#' @return Return a ggplot
#'
#' @export
#'
#' @import data.table
#' @import magrittr
#' @import ggplot2
#' @import mgcv
#'
#' @references
#' \href{https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred}{https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred}
#'
#' James Grecian. (2022). jamesgrecian/harpPup: v1.0 (v1.0). Zenodo. \href{https://doi.org/10.5281/zenodo.5901391}{https://doi.org/10.5281/zenodo.5901391}
#'
#' @examples
#' \dontrun{
#' # load data
#' data("data_nes")
#' data("data_ses")
#'
#' # combine
#' data_comp <- rbind(
#'   rbindlist(data_nes$year_2018) %>%
#'     .[, sp := "nes"],
#'   rbindlist(data_ses$year_2014) %>%
#'     .[, sp := "ses"],
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
                      sp = "sp",
                      time = "day_departure",
                      id = ".id",
                      nb_days = 100,
                      bs = "cs",
                      k = 6,
                      alpha_point = 0.01) {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  . <-
    fit_ind <-
    fit_pop <-
    fit_pop_se <-
    NULL

  # checks data is a data.table, otherwise convert it
  if (!weanlingNES::check_dt(data)) setDT(data)

  # make sure a diving_parameter is set up
  if (is.null(diving_parameter)) {
    stop("Please choose the diving parameter you want to represent")
  }

  # make sure the parameter can be associated with columns in data
  if (!all(c(diving_parameter, sp, time, id) %in% colnames(data))) {
    stop(paste0(
      "Please make sure the parameters you've entered correspond to ",
      # https://stackoverflow.com/questions/16742951/print-dataframe-name-in-function-output
      deparse(substitute(data)),
      "'s column names"
    ))
  }

  # rename colnames to match the rest of the function
  names(data)[names(data) == sp] <- "sp"
  names(data)[names(data) == time] <- "time"
  names(data)[names(data) == id] <- "id"
  names(data)[names(data) == diving_parameter] <- "diving_parameter"

  # fit GAM
  # https://stats.stackexchange.com/questions/403772/different-ways-of-modelling-interactions-between-continuous-and-categorical-pred
  mdl_tot <- gam(diving_parameter ~ sp +
    s(time, bs = bs, k = k) +
    s(time, by = sp, m = 1, bs = bs, k = k) +
    s(id, bs = "re") +
    s(id, time, bs = "re"),
  data = data[, .(
    id = as.factor(id),
    time,
    diving_parameter,
    sp = as.factor(sp)
  )],
  family = "gaussian"
  )

  # new dataset generation for individual level (column time, id)
  ind_pred_inter <- expand.grid(
    time = 0:nb_days,
    id = unique(data$id)
  )

  # add sp column
  ind_pred <- data[, .(sp = unique(sp)), by = id] %>%
    # merge to add sp column
    merge(ind_pred_inter, ., by = c("id")) %>%
    # convert as data.table
    setDT(.) %>%
    # sort by sp, time and id
    .[order(sp, time, id), ] %>%
    # add individual prediction
    .[, fit_ind := predict.gam(mdl_tot,
      .SD,
      type = "response"
    )] %>%
    # trick to avoid calling twice this object in the console for display
    .[]

  # new dataset generation for population level (column time)
  pop_pred <- setDT(expand.grid(
    time = 0:nb_days,
    sp = unique(data$sp),
    id = "population_level"
  )) %>%
    # sort by sp, time and id
    .[order(sp, time, id), ] %>%
    # retrieve population prediction
    .[, fit_pop := predict.gam(mdl_tot,
      # select the first individual by sp
      ind_pred[id %in% ind_pred[, first(id), by = sp]$V1, ],
      type = "response",
      exclude = c(
        "s(time,id)",
        "s(id)"
      )
    )] %>%
    # add standard error at the population level
    .[, fit_pop_se := predict.gam(mdl_tot,
      # select the first individual by sp
      ind_pred[id %in% ind_pred[, first(id), by = sp]$V1, ],
      type = "response",
      exclude = c(
        "s(time,id)",
        "s(id)"
      ),
      se.fit = T
    )$se.fit] %>%
    # trick to avoid calling twice this object in the console for display
    .[]

  # plot
  p1 <- ggplot() +
    geom_point(aes(x = time, y = diving_parameter, colour = sp),
      data = data[time <= nb_days, ],
      alpha = alpha_point
    ) +
    theme(legend.position = "none") +
    scale_x_continuous(
      limits = c(0, nb_days),
      oob = scales::squish,
      expand = c(0, 0)
    ) +
    geom_line(aes(
      x = time,
      y = fit_ind,
      group = id,
      colour = sp
    ),
    data = ind_pred,
    alpha = 0.4
    ) +
    geom_ribbon(
      aes(
        x = time,
        # min of confidence interval at 95%
        ymin = fit_pop - (1.96 * fit_pop_se),
        # max of confidence interval at 95%
        ymax = fit_pop + (1.96 * fit_pop_se),
        group = sp,
        fill = sp
      ),
      # since pop fit is
      data = pop_pred,
      alpha = 0.5
    ) +
    geom_line(
      aes(
        x = time,
        y = fit_pop,
        group = sp,
        colour = sp
      ),
      data = pop_pred,
      size = 1
    )

  # return
  return(p1)
}
