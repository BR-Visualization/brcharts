# Function to extract legend from ggplot object
#' @noRd
extract_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legends <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  legend <- legends[[1]]
  return(legend)
}

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
#' @examples
#'
#' forest_dot_plot(effects_table,
#' filters = "None",
#' category = "All",
#' type_graph = "Absolute risk",
#' type_risk = "Crude proportions",
#' select_nnx = "Y",
#' x_scale_fixed_free = "Fixed",
#' ci_method = "Calculated",
#' exclude_outcome = "Liver")
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
                            exclude_outcome) {
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
  dot_forest_plot <- do.call(create_dot_forest_plot, c(common_params, space_btwn_out_yn = "N"))

  # Create individual plots
  finplot0 <- create_finplot0(dot_forest_plot)
  finplot1 <- create_finplot1(dot_forest_plot)
  finplot2 <- create_finplot2(dot_forest_plot)

  # Extract legend from one of the plots
  legend <- extract_legend(dot_forest_plot$myplot_lft1 + theme(legend.position = "top"))

  # Add padding above the legend
  padded_legend <- plot_grid(NULL, legend, ncol = 1, rel_heights = c(0.2, 1))

  # Combine plots with legend at the top
  plot_grid(
    padded_legend,
    finplot1 / finplot0 / finplot2 + plot_layout(heights = c(3, 7, 7)),
    ncol = 1,
    rel_heights = c(1, 12)
  )
}

#' Create the first (top) part of the forest dot plot
#'
#' @param dot_forest_plot A list containing the left and right parts of the forest plot
#' @return A ggplot object representing the middle part of the forest dot plot
#'
#' @examples
#'
#' dot_plot_src <- subset(effects_table, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' dot_forest_plot <- create_dot_forest_plot(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   select_nnx = "Y",
#'   x_scale_fixed_free = "Fixed",
#'   ci_method = "Calculated",
#'   space_btwn_out_yn = "N"
#' )
#'
#' create_finplot1(dot_forest_plot)
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
      plot.title = element_text(margin = margin(0, 0, -5, 0)), # Adjust margin here
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      legend.position = "none" # Remove legend from this plot
    ) +
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

#' Create the second (middle) part of the forest dot plot
#'
#' @param dot_forest_plot A list containing the left and right parts of the forest plot
#' @return A ggplot object representing the top part of the forest dot plot
#'
#' @examples
#'
#' dot_plot_src <- effects_table
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' dot_forest_plot <- create_dot_forest_plot(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   select_nnx = "Y",
#'   x_scale_fixed_free = "Fixed",
#'   ci_method = "Calculated",
#'   space_btwn_out_yn = "N"
#' )
#'
#' create_finplot0(dot_forest_plot)
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


#' Create the third (bottom) part of the forest dot plot
#'
#' @param dot_forest_plot A list containing the left and right parts of the forest plot
#' @return A ggplot object representing the bottom part of the forest dot plot
#'
#' @examples
#'
#' dot_plot_src <- subset(effects_table, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' dot_forest_plot <- create_dot_forest_plot(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   select_nnx = "Y",
#'   x_scale_fixed_free = "Fixed",
#'   ci_method = "Calculated",
#'   space_btwn_out_yn = "N"
#' )
#'
#' create_finplot2(dot_forest_plot)
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
