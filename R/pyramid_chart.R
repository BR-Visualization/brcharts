#' Pyramid Chart
#'
#' Creates a population pyramid chart to visualize the distribution of a demographic
#' variable (e.g., prevalence) across age groups and genders (or other binary groups).
#'
#' @param data `dataframe` Demography data containing at least the following columns:
#'   * `Age`: Age groups (factor or character).
#'   * `Gender`: Gender categories (factor or character with two levels).
#'   * `Prevalence`: Numeric values representing the prevalence or count for each age group and gender.
#'   * `Type`: An additional grouping variable (factor or character) to create separate pyramids for different types.
#' @param levelvar `Factor` The column name in `data` representing the grouping variable
#'   to create separate pyramids (e.g., "Type").
#'   Must be a factor, character, or numeric vector.
#' @param groupvar `Factor` The column name in `data` representing the binary groups
#'   to be displayed on each side of the pyramid (e.g., "Gender").
#'   Must be a factor or character with two levels.
#' @param alpha_set `Value` Specify the transparency of the bars in the chart (between 0 and 1).
#' @param xvar `value` The column name in `data` representing the numeric values to be plotted
#'    on the x-axis (e.g., "Prevalence" or a transformed version of it).
#' @param yvar `value` The column name in `data` representing the age groups to be plotted
#'    on the y-axis (e.g., "Age").
#' @param chartcolors `vector` A vector of two colors to be used for the two sides of the pyramid.
#' @param xlab `text` Label for the x-axis.
#'
#' @importFrom patchwork plot_layout
#' @importFrom dplyr mutate filter
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @return A ggplot object representing the population pyramid chart.
#' @export
#'
#' @examples
#' # Assuming 'demography' is your data frame
#' demography |>
#'   dplyr::mutate(
#'     Type = as.factor(paste0("Type ", Type)), # Create a factor for separate pyramids
#'     figprev = ifelse(
#'       Gender == "Females", -1 * Prevalence / 100000, Prevalence / 100000
#'     ), # Transform prevalence for females to negative values
#'     Sex = Gender # Rename "Gender" to "Sex" for the plot
#'   ) |>
#'   pyramid_chart(
#'     levelvar = "Type", xvar = "figprev", yvar = "Age",
#'     groupvar = "Sex", alpha_set = 0.7, chartcolors = colfun()$fig2_colors,
#'     xlab = "Prevalence (x 100 000)"
#'   )
pyramid_chart <-
  function(data,
           xvar,
           yvar,
           levelvar,
           groupvar,
           xlab,
           alpha_set,
           chartcolors) {
    data_filtered <- data[!is.na(data[[xvar]]), ] # Filter out NA values
    figlimits <- c(-1, 1) * ceiling(max(abs(data_filtered[[xvar]])) / 10) * 10

    # Convert levelvar to factor or character if not already
    if (is.numeric(data_filtered[[levelvar]]) |
      is.character(data_filtered[[levelvar]])) {
      data_filtered[[levelvar]] <- as.factor(data_filtered[[levelvar]])
    } else if (!is.factor(data_filtered[[levelvar]]) &&
      !is.character(data_filtered[[levelvar]])) {
      stop("levelvar should be a factor, character, or numeric vector")
    }

    scale_x <- scale_x_continuous(
      limits = ~figlimits,
      breaks = seq(figlimits[1], figlimits[2], 10),
      labels = abs(seq(figlimits[1], figlimits[2], 10))
    )

    fig2_1 <- data_filtered |>
      filter(.data[[levelvar]] == levels(.data[[levelvar]])[[1]]) |>
      ggplot(aes(
        x = .data[[xvar]],
        y = .data[[yvar]],
        fill = .data[[groupvar]],
        color = .data[[groupvar]]
      )) +
      geom_col(alpha = alpha_set) +
      scale_x +
      scale_y_discrete() +
      scale_color_manual(values = chartcolors) +
      scale_fill_manual(values = chartcolors) +
      labs(
        title = levels(data[[levelvar]])[[1]],
        x = xlab
      ) +
      guides(
        fill = guide_legend(title = paste0(groupvar, ":")),
        color = guide_legend(title = paste0(groupvar, ":"))
      ) +
      br_charts_theme(
        axis.ticks = element_blank(),
        plot.margin = margin(4.5, 0, 4.5, 4.5),
        axis_line = element_blank()
      )

    fig2_2 <- data_filtered |>
      filter(.data[[levelvar]] == levels(.data[[levelvar]])[[2]]) |>
      ggplot(aes(
        x = .data[[xvar]],
        y = .data[[yvar]],
        fill = .data[[groupvar]],
        color = .data[[groupvar]]
      )) +
      geom_col(alpha = alpha_set) +
      scale_x +
      scale_y_discrete() +
      scale_color_manual(values = chartcolors) +
      scale_fill_manual(values = chartcolors) +
      guides(
        fill = guide_legend(title = paste0(groupvar, ":")),
        color = guide_legend(title = paste0(groupvar, ":"))
      ) +
      labs(
        title = levels(data[[levelvar]])[[2]], x = xlab
      ) +
      br_charts_theme(
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(4.5, 4.5, 4.5, 4.5),
        axis_line = element_blank()
      )

    cplot <- fig2_1 + theme(
      axis.text.y.left = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
      fig2_2 + theme(
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) + plot_layout(guides = "collect") & theme(legend.position = "top")

    cplot
  }
