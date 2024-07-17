#' Line Chart
#'
#' @param func the name of a function, or a call or an expression written as a function of x
#' @param data_bands (`data.frame`) a data frame with 3 variables named the following:
#' 1) level: level each band represents
#' 2) ystart: starting point of each band
#' 3) yend: ending point of each band
#' 4) col: color of each band
#' @param data_lines (`data.frame`) a data frame with 7 variables named the following:
#' 1) stage: stage each line represents
#' 2) xstart: x-axis starting point of each line
#' 3) xend: x-axis ending point of each line
#' 4) y: y-axis position of each line
#' 5) col: color of each line
#' 6) xpos: x-axis position of each annotation (relative to xstart)
#' 7) ypos: y-axis position of each annotation (relative to y)
#' @param show_bands (`character`) a logical value indicating whether to show bands
#' in the plot (Y, N)
#' @param show_points (`character`) a logical value indicating whether to show points
#' at the start of the arrows (Y, N)
#' @param line_size  (`numeric`) size of the line
#' @param line_type (`character`) type of the line
#' @param xmin (`numeric`) starting point of x axis
#' @param xmax (`numeric`) ending point of x axis
#' @param ymin (`numeric`) starting point of y axis
#' @param ymax (`numeric`) ending point of y axis
#' @param xbreaks (`numeric`) x axis breaks
#' @param ybreaks (`numeric`) y axis breaks
#' @param xlab (`character`) x axis label
#' @param ylab (`character`) y axis label
#' @param legend_title (`character`) legend title
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' func <- function(x) 47.982 - 0.0305 * x - 0.57525 * x^2 + 0.0245 * x^3
#'
#' line_chart(func = func,
#'            data_bands = data_bands,
#'            data_lines = data_lines,
#'            xmin = 0,
#'            xmax = 14.2,
#'            ymin = 0,
#'            ymax = 50,
#'            xbreaks = seq(0, 14, 2),
#'            ybreaks = seq(0, 50, 5),
#'            xlab = "Years",
#'            ylab = "Functional Score",
#'            legend_title = "Severity: "
#' )
line_chart <- function(func,
                       data_bands,
                       data_lines,
                       show_bands = "Y",
                       show_points = "N",
                       line_size = 0.6,
                       line_type = "solid",
                       xmin,
                       xmax,
                       ymin,
                       ymax,
                       xbreaks,
                       ybreaks,
                       xlab,
                       ylab,
                       legend_title
) {
  # check if data_bands has all required columns
  data_bands_columns <- c(
    "level", "ystart", "yend", "col"
  )
  nonexistent_columns_bands <- setdiff(data_bands_columns, colnames(data_bands))

  if (length(nonexistent_columns_bands) > 0) {
    error_message <- paste0("You are missing a required variable in your data_bands dataframe:", nonexistent_columns_bands)
    stop(error_message)
  }

  # check if variables in data_bands are of the required data type
  if (!is.numeric(data_bands$ystart) | !is.numeric(data_bands$yend)){
    error_message <- paste0("The columns ystart and yend in your data_bands dataframe must be of type numeric")
    stop(error_message)
  }

  # check if data_bands have missing values
  data_bands <- data_bands %>%
    select(level, ystart, yend, col)

  if (any(is.na(data_bands))) {
    warning(paste(
      "you have a missing value in row(s)",
      which(rowSums(is.na(data_bands)) > 0),
      "of your data_bands dataframe"
    ))
    data_bands <- na.omit(data_bands)
  }

  # check if data_lines have all required columns
  data_lines_columns <- c(
    "stage", "xstart", "xend", "y", "col", "xpos", "ypos"
  )
  nonexistent_columns_lines <- setdiff(data_lines_columns, colnames(data_lines))

  if (length(nonexistent_columns_lines) > 0) {
    error_message <- paste0("You are missing a required variable in your data_lines dataframe:", nonexistent_columns_lines)
    stop(error_message)
  }

  # check if variables in data_bands are of the required data type
  if (!is.numeric(data_lines$xstart) |
      !is.numeric(data_lines$xend) |
      !is.numeric(data_lines$y) |
      !is.numeric(data_lines$xpos) |
      !is.numeric(data_lines$ypos)
  ){
    error_message <- paste0("The columns xstart, xend, y, xpos, and ypos in your data_lines dataframe must be of type numeric")
    stop(error_message)
  }

  # check if data_lines have missing values
  data_lines <- data_lines %>%
    select(stage, xstart, xend, y, col, xpos, ypos)

  if (any(is.na(data_lines))) {
    warning(paste(
      "you have a missing value in row(s)",
      which(rowSums(is.na(data_lines)) > 0),
      "of your data_lines dataframe"
    ))
    data_lines <- na.omit(data_lines)
  }

  data_lines2 <- data_lines %>% pivot_longer(cols = c(xstart, xend),
                                             values_to  = "xvar")
  fig <- ggplot() +
    geom_function(fun = func) +
    xlim(c(xmin, xmax)) +
    {
      if (show_bands == "Y")
        geom_rect(
          data = data_bands,
          inherit.aes = FALSE,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ystart,
            ymax = yend,
            group = factor(level),
            fill = factor(level)
          ),
          alpha = 0.3
        )
    } +
    scale_fill_manual(legend_title,
                      values = data_bands$col,
                      guide = guide_legend(override.aes = list(alpha = 1))) +
    geom_line(
      data = data_lines2,
      size = line_size,
      aes(
        x = xvar,
        y = y,
        colour = col,
        linetype = line_type
      ),
      arrow = arrow(
        length = unit(0.30, "cm"),
        ends = "last",
        type = "closed"
      ),
      show.legend = FALSE
    ) +
    scale_colour_manual(guide = "none",
                        values = rev(colfun()$fig3_colors)) +
    {
      if (show_bands != "Y")
        annotate(
          geom = "text",
          x = data_lines$xstart + data_lines$xpos,
          y = data_lines$y + data_lines$ypos,
          label = data_lines$stage,
          color = rev(data_lines$col)
        )
    } +
    {
      if (show_bands == "Y")
        annotate(
          geom = "text",
          x = data_lines$xstart + data_lines$xpos,
          y = data_lines$y + data_lines$ypos,
          label = data_lines$stage,
          color = "black",
          size = control_fonts()$p*0.35,
        )
    } +
    {
      if (show_points == "Y")
        geom_point(
          data = filter(data_lines, name == "xstart"),
          aes(x = xvar, y = y, colour = stage),
          size = 2
        )
    } +
    coord_cartesian(xlim = c(xmin, xmax),
                    ylim = c(ymin, ymax),
                    expand = FALSE) +
    scale_x_continuous(breaks = xbreaks) +
    scale_y_continuous(breaks = ybreaks) +
    labs(x = xlab, y = ylab) +
    theme_minimal() +
    br_charts_theme(
      panel_grid_minor = element_blank(),
      panel_grid_major = element_blank()
    )

  fig
}
