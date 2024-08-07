#' Grouped Bar Chart
#'
#' @param data `dataframe` a data frame with a minimum of 3 variables:
#' 1) a variable that contains the x coordinates of points (xvar)
#' 2) a variable that contains the y coordinates of points (yvar)
#' 3) a grouping variable (groupvar)
#' @param xvar `value` x-axis variable
#' @param yvar `value` y-axis variable
#' @param groupvar `Factor` grouping variable
#' @param chartcolors `vector` a vector of colors, the same number of levels as
#' the group variable
#'
#' @return a ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' grouped_barchart(
#'   data = comorbidities, xvar = "Comorbidities",
#'   yvar = "Prevalence", groupvar = "Severity",
#'   chartcolors = colfun()$fig4_colors
#' )
#'
grouped_barchart <- function(data, xvar, yvar, groupvar, chartcolors) {
  fig <- ggplot(data, aes(
    x = .data[[xvar]], y = .data[[yvar]], fill = .data[[groupvar]]
  )) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values = chartcolors) +
    guides(fill = guide_legend(title = paste0(groupvar, ":"))) +
    br_charts_theme(
      axis.line.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  fig
}
