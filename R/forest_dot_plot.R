#' noRd
forest_dot_plot <- function(){
  dot_plot_src <- effects_table
  bdin <- subset(dot_plot_src, Factor == "Benefit")
  rdin <- subset(dot_plot_src, Factor == "Risk") %>% filter(Outcome != "Liver")

  dot_forest_plot <- create_dot_forest_plot(
    data = dot_plot_src,
    drug = unique(dot_plot_src$Trt1),
    benefit = unique(bdin$Outcome),
    risk = unique(rdin$Outcome),
    filters = "None",
    category = "All",
    type_graph = "Absolute risk",
    type_risk = "Crude proportions",
    select_nnx = "Y",
    x_scale_fixed_free = "Fixed",
    ci_method = "Calculated"
  )


  dot_forest_plot <- create_dot_forest_plot(
    data = dot_plot_src,
    drug = unique(dot_plot_src$Trt1),
    benefit = unique(bdin$Outcome),
    risk = unique(rdin$Outcome),
    filters = "None",
    category = "All",
    type_graph = "Absolute risk",
    type_risk = "Crude proportions",
    select_nnx = "Y",
    x_scale_fixed_free = "Fixed",
    ci_method = "Calculated",
    space_btwn_out_yn = "N"
  )


  finplot0 <- dot_forest_plot$myplot_lft0 +
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
      # to remove ticks in axis
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

  finplot1 <- dot_forest_plot$myplot_lft1 +
    br_charts_theme(
      axis.ticks = element_blank(),
      base_font_size = 9,
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis_line = element_blank(),
      plot.title = element_text(margin = margin(0, 0, -20, 0)),
      plot.margin = unit(c(20, 5.5, 0, 5.5), "pt"),
      # legend_position = "none"
    ) +
    guides(colour = guide_legend(title = "Treatment:")) +
    dot_forest_plot$myplot_rgt1 +
    br_charts_theme(
      # to remove ticks in axis
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

  finplot2 <- dot_forest_plot$myplot_lft2 +
    theme(legend.position = "none") +
    br_charts_theme(
      axis.ticks = element_blank(),
      base_font_size = 9,
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      axis_line = element_blank(),
      legend_position = "none"
    ) +
    dot_forest_plot$myplot_rgt2 +
    theme(legend.position = "none") +
    br_charts_theme(
      # to remove ticks in axis
      axis.ticks = element_blank(),
      axis_text_y_left = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"),
      axis_line = element_blank(),
      base_font_size = 9,
      legend_position = "none"
    )

  finplot <- finplot1 / finplot0 / finplot2 +
    plot_layout(heights = c(3, 7, 7))

  finplot

}
