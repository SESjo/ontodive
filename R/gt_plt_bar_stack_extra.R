#' @title Add a percent stacked barchart in place of existing data.
#'
#' @description The \code{gt_plt_bar_stack_extra} is a copy paste of the function
#' \code{gt_plt_bar_stack} available in the \code{gtExtras} package where the
#' limitation of 3 user-specified has been risen to 4. It takes an existing
#' \code{gt_tbl} object and converts the existing values into a percent stacked
#' barchart. The bar chart will represent either 2, 3 or 4 user-specified values
#' per row, and requires a list column ahead of time. The palette and labels need
#' to be equal length. The values must either add up to 100 \emph{i.e.} as
#' percentage points if using \code{position = 'fill'}, or can be raw values
#' with \code{position = 'stack'}. Note that the labels can be controlled via
#' the \code{fmt_fn} argument and the \code{⁠scales::lab_xxx} family of function.
#'
#' @param gt_object An existing gt table object of class gt_tbl
#' @param column The column wherein the percent stacked barchart should replace existing data. Note that the data must be represented as a list of numeric values ahead of time.
#' @param palette A color palette of length 2 or 3, represented either by hex colors (\code{#ff4343}) or named colors (\code{red}).
#' @param labels A vector of strings of length 2 or 3, representing the labels for the bar chart, will be colored according to the palette as well.
#' @param position An string indicator passed to ggplot2 indicating if the bar should be a percent of total \code{fill} or stacked as the raw values \code{stack}.
#' @param width An integer representing the width of the bar chart in pixels.
#' @param fmt_fn A specific function from \code{⁠scales::lab_xxx}⁠ family. Defaults to \code{scales::label_number}
#'
#' @return An object of class gt_tbl.
#'
#' @seealso \code{\link[gtExtras]{gt_plt_bar_stack}}
#'
#' @references \url{https://github.com/jthomasmock/gtExtras/blob/HEAD/R/gt_pct_bar.R}
#'
#' @export
#'
#' @rawNamespace import(dplyr, except = c(between))
#' @importFrom rlang as_label enquo
#' @import gt
#' @import scales
#' @import glue
#' @importFrom gtExtras gt_index
#'
#' @examples
#' \dontrun{
#' # load library
#' library(gt)
#' library(dplyr)
#'
#' # dummy dataset
#' ex_df <- dplyr::tibble(
#'   x = c(
#'     "Example 1", "Example 1",
#'     "Example 1", "Example 1", "Example 2", "Example 2", "Example 2", "Example 2",
#'     "Example 3", "Example 3", "Example 3", "Example 3", "Example 4", "Example 4", "Example 4",
#'     "Example 4"
#'   ),
#'   measure = c(
#'     "Measure 1", "Measure 2",
#'     "Measure 3", "Measure 4", "Measure 1", "Measure 2", "Measure 3", "Measure 4",
#'     "Measure 1", "Measure 2", "Measure 3", "Measure 4", "Measure 1", "Measure 2",
#'     "Measure 3", "Measure 4"
#'   ),
#'   data = c(30, 20, 40, 10, 30, 30, 20, 20, 30, 10, 30, 30, 30, 50, 10, 10)
#' )
#'
#' # display results
#' ex_df %>%
#'   group_by(x) %>%
#'   summarise(list_data = list(data)) %>%
#'   gt() %>%
#'   gt_plt_bar_stack_extra(column = list_data)
#' }
#'
gt_plt_bar_stack_extra <- function(gt_object,
                                   column = NULL,
                                   palette = c(
                                     "#ff4343", "#bfbfbf",
                                     "#0a1c2b", "#e1be6a"
                                   ),
                                   labels = c("Group 1", "Group 2", "Group 3", "Group 4"),
                                   position = "fill",
                                   width = 70,
                                   fmt_fn = label_number(
                                     scale_cut = cut_short_scale(),
                                     trim = TRUE
                                   )) {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  . <-
    NULL

  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in%
    class(gt_object))
  stopifnot(`There must be 2 to 4 labels` = (length(labels) %in%
    c(2:4)))
  stopifnot(`There must be 2 to 4 colors in the palette` = (length(palette) %in%
    c(2:4)))
  stopifnot(`\`position\` must be one of 'stack' or 'fill'` = (position %in%
    c("stack", "fill")))
  var_sym <- enquo(column)
  var_bare <- as_label(var_sym)
  all_vals <- gt_index(gt_object, {{ column }}) %>%
    lapply(X = ., FUN = sum, na.rm = TRUE) %>%
    unlist()
  if (length(all_vals) == 0) {
    return(gt_object)
  }
  total_rng <- max(all_vals, na.rm = TRUE)
  tab_out <- text_transform(
    gt_object,
    locations = cells_body({{ column }}),
    fn = function(x) {
      bar_fx <- function(x_val) {
        if (x_val %in% c("NA", "NULL")) {
          return("<div></div>")
        }
        col_pal <- palette
        vals <- strsplit(x_val, split = ", ") %>%
          unlist() %>%
          as.double()
        n_val <- length(vals)
        stopifnot(`There must be 2 to 4 values` = (n_val %in%
          c(2:4)))
        col_fill <- if (n_val == 2) {
          c(1, 2)
        } else {
          c(1:n_val)
        }
        df_in <- tibble(
          x = vals,
          y = rep(1, n_val),
          fill = col_pal[col_fill]
        )
        plot_out <-
          df_in %>% ggplot(aes(
            x = .data$x,
            y = factor(.data$y),
            fill = I(.data$fill),
            group = .data$y
          )) +
          geom_col(
            position = position,
            color = "white",
            size = 1
          ) +
          geom_text(
            aes(label = fmt_fn(x)),
            hjust = 0.5,
            size = 3,
            family = "sans",
            fontface = "bold",
            position = if (position ==
              "fill") {
              position_fill(vjust = 0.5)
            } else if (position == "stack") {
              position_stack(vjust = 0.5)
            },
            color = "white"
          ) +
          scale_x_continuous(expand = if (position ==
            "stack") {
            expansion(mult = c(0, 0.1))
          } else {
            c(0, 0)
          }, limits = if (position == "stack") {
            c(0, total_rng)
          } else {
            NULL
          }) +
          scale_y_discrete(expand = c(0, 0)) +
          coord_cartesian(clip = "off") +
          theme_void() +
          theme(
            legend.position = "none",
            plot.margin = margin(0, 0, 0, 0, "pt")
          )
        out_name <- file.path(tempfile(
          pattern = "file",
          tmpdir = tempdir(),
          fileext = ".svg"
        ))
        ggsave(
          out_name,
          plot = plot_out,
          dpi = 25.4,
          height = 5,
          width = width,
          units = "mm",
          device = "svg"
        )
        img_plot <-
          readLines(out_name) %>%
          paste0(collapse = "") %>%
          html()
        on.exit(file.remove(out_name), add = TRUE)
        img_plot
      }
      tab_built <- lapply(X = x, FUN = bar_fx)
    }
  )
  # set up labels
  label_built <- if (max(all_vals) == 2) {
    lab_pal1 <- palette[1]
    lab_pal2 <- palette[2]
    lab1 <- labels[1]
    lab2 <- labels[2]
    glue(
      "<span style='color:{lab_pal1}'><b>{lab1}</b></span>",
      " | ",
      "<span style='color:{lab_pal2}'><b>{lab2}</b></span>"
    ) %>%
      html()
  } else if (max(all_vals) == 3) {
    lab_pal1 <- palette[1]
    lab_pal2 <- palette[2]
    lab_pal3 <- palette[3]
    lab1 <- labels[1]
    lab2 <- labels[2]
    lab3 <- labels[3]
    glue(
      "<span style='color:{lab_pal1}'><b>{lab1}</b></span>",
      " | ",
      "<span style='color:{lab_pal2}'><b>{lab2}</b></span>",
      " | ",
      "<span style='color:{lab_pal3}'><b>{lab3}</b></span>"
    ) %>%
      html()
  } else {
    lab_pal1 <- palette[1]
    lab_pal2 <- palette[2]
    lab_pal3 <- palette[3]
    lab_pal4 <- palette[4]
    lab1 <- labels[1]
    lab2 <- labels[2]
    lab3 <- labels[3]
    lab4 <- labels[4]
    glue(
      "<span style='color:{lab_pal1}'><b>{lab1}</b></span>",
      " | ",
      "<span style='color:{lab_pal2}'><b>{lab2}</b></span>",
      " | ",
      "<span style='color:{lab_pal3}'><b>{lab3}</b></span>",
      " | ",
      "<span style='color:{lab_pal4}'><b>{lab4}</b></span>"
    ) %>%
      html()
  }
  tab_out <- gt:::dt_boxhead_edit_column_label(
    data = tab_out,
    var = var_bare,
    column_label = label_built
  )
  suppressWarnings(tab_out)
}
