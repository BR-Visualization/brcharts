#' Create a forest dot plot
#'
#' This function creates a composite forest dot plot using provided data and parameters.
#'
#' @param effects_table A data frame containing the effects data
#' @param filters Character string specifying filters to apply. Default is "None"
#' @param category Character string specifying the category. Default is "All"
#' @param type_graph Character string specifying the type of graph. Default is "Absolute risk"
#' @param type_risk Character string specifying the type of risk. Default is "Crude proportions"
#' @param select_nnx Character string specifying whether to select NNX. Default is "Y"
#' @param x_scale_fixed_free Character string specifying whether x-scale is fixed or free. Default is "Fixed"
#' @param ci_method Character string specifying the CI method. Default is "Calculated"
#' @param exclude_outcome Character string specifying an outcome to exclude. Default is "Liver"
#'
#' @return A ggplot object representing the composite forest dot plot
#' @noRd
forest_dot_plot <- function(effects_table,
                            filters = "None",
                            category = "All",
                            type_graph = "Absolute risk",
                            type_risk = "Crude proportions",
                            select_nnx = "Y",
                            x_scale_fixed_free = "Fixed",
                            ci_method = "Calculated",
                            exclude_outcome = "Liver") {
  # Data preparation
  dot_plot_src <- effects_table
  bdin <- subset(dot_plot_src, Factor == "Benefit")
  rdin <- subset(dot_plot_src, Factor == "Risk") %>% filter(!(Outcome %in% exclude_outcome))

  # Common parameters for create_dot_forest_plot
  common_params <- list(
    data = dot_plot_src,
    drug = unique(dot_plot_src$Trt1),
    benefit = unique(bdin$Outcome),
    risk = unique(rdin$Outcome),
    filters = filters,
    category = category,
    type_graph = type_graph,
    type_risk = type_risk,
    select_nnx = select_nnx,
    x_scale_fixed_free = x_scale_fixed_free,
    ci_method = ci_method
  )

  # Create dot forest plots
  dot_forest_plot1 <- do.call(create_dot_forest_plot, common_params)
  dot_forest_plot2 <- do.call(create_dot_forest_plot, c(common_params, space_btwn_out_yn = "N"))

  # Create individual plots
  finplot0 <- create_finplot0(dot_forest_plot2)
  finplot1 <- create_finplot1(dot_forest_plot1)
  finplot2 <- create_finplot2(dot_forest_plot1)

  # Combine plots
  finplot1 / finplot0 / finplot2 + plot_layout(heights = c(3, 7, 7))
}

#' Create the first (top) part of the forest dot plot
#'
#' @param dot_forest_plot A list containing the left and right parts of the forest plot
#' @return A ggplot object representing the top part of the forest dot plot
#' @noRd
create_finplot0 <- function(dot_forest_plot) {
  dot_forest_plot$myplot_lft0 +
    br_charts_theme(
      axis.ticks = element_blank(),
      base_font_size = 9,
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis_line = element_blank(),
      plot.title = element_text(margin = margin(0, 0, 0, 0)),
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      legend_position = "none"
    ) +
    dot_forest_plot$myplot_rgt0 +
    scale_x_continuous(breaks = seq(-60, 60, 20), limits = c(-60, 60)) +
    br_charts_theme(
      axis.ticks = element_blank(),
      axis_text_y_left = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      base_font_size = 9,
      axis_line = element_blank(),
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      legend_position = "none"
    )
}

#' Create the second (middle) part of the forest dot plot
#'
#' @param dot_forest_plot A list containing the left and right parts of the forest plot
#' @return A ggplot object representing the middle part of the forest dot plot
#' @noRd
create_finplot1 <- function(dot_forest_plot) {
  dot_forest_plot$myplot_lft1 +
    br_charts_theme(
      axis.ticks = element_blank(),
      base_font_size = 9,
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis_line = element_blank(),
      plot.title = element_text(margin = margin(0, 0, -20, 0)),
      plot.margin = unit(c(20, 5.5, 0, 5.5), "pt")
    ) +
    guides(colour = guide_legend(title = "Treatment:")) +
    dot_forest_plot$myplot_rgt1 +
    br_charts_theme(
      axis.ticks = element_blank(),
      axis_text_y_left = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      base_font_size = 9,
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      axis_line = element_blank(),
      legend_position = "none"
    )
}

#' Create the third (bottom) part of the forest dot plot
#'
#' @param dot_forest_plot A list containing the left and right parts of the forest plot
#' @return A ggplot object representing the bottom part of the forest dot plot
#' @noRd
create_finplot2 <- function(dot_forest_plot) {
  dot_forest_plot$myplot_lft2 +
    br_charts_theme(
      axis.ticks = element_blank(),
      base_font_size = 9,
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      axis_line = element_blank(),
      legend_position = "none"
    ) +
    dot_forest_plot$myplot_rgt2 +
    br_charts_theme(
      axis.ticks = element_blank(),
      axis_text_y_left = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      axis_line = element_blank(),
      base_font_size = 9,
      legend_position = "none"
    )
}
