#' Create a Grouped Bar Chart
#'
#' This function generates a grouped bar chart using ggplot2, where bars are grouped by a specified variable.
#'
#' @param data A data frame containing at least 3 variables: x-axis variable, y-axis variable, and a grouping variable.
#' @param xvar A string specifying the name of the variable to be plotted on the x-axis.
#' @param yvar A string specifying the name of the variable to be plotted on the y-axis.
#' @param groupvar A string specifying the name of the grouping variable.
#' @param chartcolors A vector of colors with the same length as the number of levels in the grouping variable.
#'
#' @return A ggplot object representing the grouped bar chart.
#'
#' @details
#' The function creates a bar chart where bars are grouped based on the specified grouping variable.
#' It uses position = "dodge" to place bars side by side within each group.
#' The y-axis line, minor grid lines, and y-axis ticks are removed for a cleaner appearance.
#'
#' @note
#' This function requires the ggplot2 package and assumes the existence of a custom theme function br_charts_theme().
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' # Assuming 'comorbidities' is a data frame with appropriate columns
#' # and 'colfun()$fig4_colors' returns a vector of colors
#' grouped_barchart(
#' data = comorbidities,
#' xvar = "Comorbidities",
#' yvar = "Prevalence",
#' groupvar = "Severity",
#' chartcolors = colfun()$fig4_colors
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
