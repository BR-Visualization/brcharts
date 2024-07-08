#' Dot Plot order label
#'
#' Order dot plot labels and only show first occurrence of label
#'
#' @param indata (`data.frame`) data frame for dot plot
#' @param groupvars variables to order by
#' @param groupeff effects table outcome order
#' @param space_btwn_out_yn control spacing between outcomes
#' @param dataout (`data.frame`) ordered data frame for dot plot
#'
#' @return data frame for specified type of analysis
#'
#' @import magrittr dplyr
#' @importFrom glue glue
#' @export
#'
#' @examples
#' dot_plot_src <- subset(effects_table, Type == "Binary")
#' dot_data <- prepare_dot_data_b(dot_plot_src)
#'
#' dot_plot_data <- create_order_label_der(
#'   indata = dot_data,
#'   groupvars = c("type", "factor", "outcome"),
#'   groupeff = c("group"),
#'   space_btwn_out_yn = "N"
#' )
#'
#' head(dot_plot_data)
create_order_label_der <- function(indata,
                                   groupvars,
                                   groupeff,
                                   space_btwn_out_yn = "Y",
                                   dataout = NULL) {
  # Create group by variable to follow effects table order
  ordered_data <- indata %>%
    filter(!is.na(rate)) %>%
    select(all_of(c(groupeff, groupvars))) %>%
    group_by(across(all_of(groupvars))) %>%
    mutate(eff_order = min(!!sym(groupeff))) %>%
    ungroup() %>%
    arrange(across(all_of(
      c("type", "factor", "eff_order", "outcome")
    ))) %>%
    distinct(across(all_of(c(
      groupvars, groupeff
    )))) %>%
    mutate(neword = row_number())

  # Join and process data
  processed_data <- indata %>%
    inner_join(ordered_data, by = c(groupeff, groupvars)) %>%
    arrange(neword) %>%
    mutate(allobs = n() / 2) %>%
    group_by(across(all_of(c(
      "factor", "outcome"
    )))) %>%
    mutate(mylab = if_else(neword == min(neword), outcome, ""),
           neword = allobs - neword) %>%
    ungroup() %>%
    group_by(across(all_of(c("type", "factor")))) %>%
    mutate(fobs = n() / 2,
           neword = if_else(factor == "Benefit", neword, neword - fobs)) %>%
    ungroup()

  # Adjust spacing between groups
  adjust <- processed_data %>%
    distinct(across(all_of(c(
      "type", "factor", "outcome"
    )))) %>%
    mutate(adjust_number = row_number())

  final_data <- processed_data %>%
    inner_join(adjust, by = c("type", "factor", "outcome")) %>%
    group_by(factor) %>%
    mutate(
      neword = neword - if (space_btwn_out_yn == "Y")
        adjust_number
      else
        0,
      mins_y = if_else(factor == "Benefit", min(neword), max(neword))
    ) %>%
    mutate(neword = if_else(factor == "Benefit", neword - mins_y + 2, neword - mins_y - 2)) %>%
    ungroup()

  # Log message
  message(
    glue(
      '[{format(Sys.time(),"%F %T")}] > Dataout object from the create_order_label_der function is created'
    )
  )

  final_data
}
