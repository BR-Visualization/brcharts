#' Create a forest dot plot
#'
#' This function creates a composite forest dot plot using provided data and parameters.
#'
#' @param effects_table A data frame containing the effects data
#' The following variables are required columns. Note that the variables `Grouped_Outcome`, `Statistics`, and `Outcome_Status` are not required for generating a forest-dot plot, but are listed as required columns because they are key for generating a value tree, which is a starting point for all subsequent benefit-risk assessments.
#'  1) Factor: A character vector containing whether an outcome is a "Benefit" or a "Risk"
#'  2) Grouped_Outcome: A character vector containing the name of grouped outcomes, e.g., Infections
#'  3) Outcome: A character vector containing the name of outcomes, e.g., Herpes viral infections, upper respiratory tract infections
#'  4) Statistics: A character vector containing the summary statistics of outcomes, e.g., %, mean change from baseline
#'  5) Type: A character vector containing whether an outcome is a "Binary" or a "Continuous" variable
#'  6) Outcome_Status: A character vector containing whether an outcome is an "Identified" or a "Potential" outcome
#'  7) Filter: A character vector containing the filter for subgroup data, should be "None" if no filtre is applicable. Example: None; Sex.
#'  8) Category: A character vector containing  the category for filtering subgroup data, should be "All" if no filter is applicable. Example: All; Male, Female.
#'  9) Trt1: A character vector containing the name of active treatments
#'  10) Trt2: A character vector containing controlled term "Placebo"
#'  11) Drug_Status: A character vector containing whether a treatment is an "Approved" or a "Test" drug
#' The following variables are situational columns - Filled in only for the specific summary statistic related to the row outcome (ex. proportion):
#'  12) Rate_Type: A numeric vector containing whether an AE rate is "EventRate" or "IncRate". Required for risk outcomes displayed in exposure-adjusted event rate or incidence rate.
#'  13) Prop1: A numeric vector containing the proportion in active treatment. Required for binary outcomes displayed in proportions; can be populated by nSub1/N1 if both nSub1 and N1 are provided.
#'  14) IncRate1: A numeric vector containing the exposure-adjusted incidence rate per 100 PYs in active treatment. Required for risk outcomes displayed in exposure-adjusted incidence rates; can be populated by nSub1/PYAR1*100 if both nSub1 and PYAR1 are provided.
#'  15) EventRate1: A numeric vector containing the exposure-adjusted event rate per 100 PYs in active treatment. Required for risk outcomes displayed in exposure-adjusted event rates; can be populated by nEvent1/PEY1*100 if both nEvent1 and PEY1 are provided.
#'  16) Mean1: A numeric vector containing the mean in active treatment. Required for continuous outcomes.
#'  17) Prop2: A numeric vector containing the proportion in comparator treatment. Required for binary outcomes displayed in proportions; can be populated by nSub2/N2 if both nSub1 and N1 are provided.
#'  18) IncRate2: A numeric vector containing the exposure-adjusted incidence rate per 100 PYs in comparator treatment. Required for risk outcomes displayed in exposure-adjusted incidence rates; can be populated by nSub2/PYAR2*100 if both nSub1 and PYAR1 are provided.
#'  19) EventRate2: A numeric vector containing the exposure-adjusted event rate per 100 PYs in comparator treatment. Required for risk outcomes displayed in exposure-adjusted event rates; can be populated by nEvent2/PEY2*100 if both nEvent1 and PEY1 are provided.
#'  20) Mean2: A numeric vector containing the mean in comparator treatment. Required for continuous outcomes.
#' The following variables are optional columns - Can be either hand entered or calculated by the package (ex. confidence intervals):
#'  21) N1: An integer vector containing the total number of subjects in active treatment. Required when needing to calculate confidence intervals within the package for proportions.
#'  22) 100PYAR1: A numeric vector containing 100 patient-years at risk in active treatment. Required when needing to calculate confidence intervals within the app for exposure-adjusted incidence rates.
#'  23) 100PEY1: A vector containing 100 patient-years of exposure in active treatment. Required when needing to calculate confidence intervals within the app for exposure-adjusted event rates.
#'  24) Sd1: A numeric vector containing the standard deviation in active treatment. Required when needing to calculate confidence intervals within the app for continuous outcomes; can be populated by Se1/SQRT(N1) if Se1 and N1 are provided.
#'  25) N2: An integer vector containing the total number of subjects in comparator treatment. Required when needing to calculate confidence intervals within the package for proportions.
#'  26) 100PYAR2: A numeric vector containing 100 patient-years at risk in comparator treatment. Required when needing to calculate confidence intervals within the app for exposure-adjusted incidence rates.
#'  27) 100PEY2: A numeric vector containing 100 patient-years of exposure in comparator treatment. Required when needing to calculate confidence intervals within the app for exposure-adjusted event rates.
#'  28) Sd2: A numeric vector containing the standard deviation in comparator treatment. Required when needing to calculate confidence intervals within the app for continuous outcomes; can be populated by Se2/SQRT(N2) if Se2 and N2 are provided.
#'  29) Diff_LowerCI: A numeric vector containing the lower confidence interval for difference in proportions and continuous outcomes. Required when using supplied confidence intervals for difference in proportions and continuous outcomes.
#'  30) Diff_UpperCI: A numeric vector containing the upper confidence interval for difference in proportions and continuous outcomes. Required when using supplied confidence intervals for difference in proportions and continuous outcomes.
#'  31) Diff_IncRate_LowerCI: A numeric vector containing the lower confidence interval for difference in exposure-adjusted incidence rates. Required when using supplied confidence intervals for difference in exposure-adjusted incidence rates.
#'  32) Diff_IncRate_UpperCI: A numeric vector containing the upper confidence interval for difference in exposure-adjusted incidence rates. Required when using supplied confidence intervals for difference in exposure-adjusted incidence rates.
#'  33) Diff_EventRate_LowerCI: A numeric vector containing the lower confidence interval for difference in exposure-adjusted event rates. Required when using supplied confidence intervals for difference in exposure-adjusted event rates.
#'  34) Diff_EventRate_UpperCI: A numeric vector containing the upper confidence interval for difference in exposure-adjusted event rates. Required when using supplied confidence intervals for difference in exposure-adjusted event rates.
#'  35) RelRisk_LowerCI: A numeric vector containing the lower confidence interval for relative risk of binary outcomes. Required when using supplied confidence intervals for relative risk of binary outcomes.
#'  36) RelRisk_UpperCI: A numeric vector containing the upper confidence interval for relative risk of binary outcomes. Required when using supplied confidence intervals for relative risk of binary outcomes.
#'  37) OddsRatio_LowerCI: A numeric vector containing the lower confidence interval for odds ratio of binary outcomes. Required when using supplied confidence intervals for odds ratio of binary outcomes.
#'  38) OddsRatio_UpperCI: A numeric vector containing the upper confidence interval for odds ratio of binary outcomes. Required when using supplied confidence intervals for odds ratio of binary outcomes.
#' The following variables are supplementary columns - Used to calculate other columns are not required by the package(ex. number of subjects with events):
#'  39) nSub1: An integer vector containing the number of subjects with events in active treatment. Not required; can be used to calculate Prop1 by nSub1/N1.
#'  40) Dur1: A numeric vector containing the duration of treatment in active treatment. Not required; can be used to estimate 100PYAR1 and 100PEY1.
#'  41) nEvent1: An integer vector containing the number of events in active treatment. Not required; can be used to calculate EventRate1 by nEvent1/100PEY1.
#'  42) Se1: A numeric vector containing the standard error in active treatment. Not required; can be used to calculate Sd1 by Se1*SQRT(N1).
#'  43) nSub2: An integer vector containing the number of subjects with events in comparator treatment. Not required; can be used to calculate Prop2 by nSub2/N2.
#'  44) Dur2: A numeric vector containing the duration of treatment in comparator treatment. Not required; can be used to estimate 100PYAR2 and 100PEY2.
#'  45) nEvent2: An integer vector containing the number of events in comparator treatment. Not required; can be used to calculate EventRate2 by nEvent2/100PEY2.
#'  46) Se2: A numeric vector containing the standard error in comparator treatment. Not required; can be used to calculate Sd2 by Se2*SQRT(N2).
#' The following variables are documentation columns - Record the data source (ex. Study xyz, Table 1.2.3, date):
#'  47) MCDA_Weight: A numeric vector containing the MCDA weight
#'  48) Population: A character vector containing the population for the analysis (e.g., ITT, Safety Set)
#'  49) Data_Source: A character vector containing the source of data (e.g., Reference CSR Table xxx)
#'  50) Quality: A character vector containing the quality of data
#'  51) Notes: A character vector containing notes
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
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   select_nnx = "Y",
#'   x_scale_fixed_free = "Fixed",
#'   ci_method = "Calculated",
#'   exclude_outcome = "Liver"
#' )
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
