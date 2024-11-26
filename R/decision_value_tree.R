#' Create a Collapsible Tree Diagram of Outcome Types and Requirements
#'
#' This function generates an interactive collapsible tree diagram that visualizes
#' the relationship between outcome types, summary statistics, treatment outcomes,
#' methods for populating confidence intervals, and required columns.
#'
#' @param data A data frame containing the relevant information. It should have
#'   columns named "Type of Outcome", "Summary Statistics for Primary Outcome",
#'   "Summary Statistics for Treatment Outcome", "Method for Populating CI", and
#'   "Required Columns".
#' @param width The width of the tree diagram in pixels. Default is 1000.
#' @param height The height of the tree diagram in pixels. Default is 800.
#' @param fontSize The font size for the tree labels. Default is 14.
#'
#' @return An interactive collapsible tree diagram.
#' @export
#'
#' @import dplyr
#' @importFrom collapsibleTree collapsibleTree
#'
#' @examples
#' \dontrun{
#' data <- read.csv("data-raw/decision_tree.csv", check.names = FALSE)
#' decision_value_tree(data)
#' }
decision_value_tree <- function(data, width = 1000, height = 800, fontSize = 14) {
  data <- preprocess_data(data)
  outcome <- prepare_outcome_data(data)

  create_collapsible_tree(outcome, width, height, fontSize)
}

#' Preprocess the input data
#'
#' @param data The input data frame
#' @return A preprocessed data frame
preprocess_data <- function(data) {
  data %>% select(-1)
}

#' Prepare outcome data for the tree diagram
#'
#' @param data The preprocessed data frame
#' @return A data frame with prepared outcome data
prepare_outcome_data <- function(data) {
  binary_data <- prepare_binary_data(data)
  continuous_data <- prepare_continuous_data(data)

  bind_rows(binary_data, continuous_data) %>%
    filter(!is.na(Type))
}

#' Prepare binary outcome data
#'
#' @param data The preprocessed data frame
#' @return A data frame with prepared binary outcome data
prepare_binary_data <- function(data) {
  data %>%
    filter(`Type of Outcome` == "Binary") %>%
    select(
      Type = `Type of Outcome`,
      Summary = `Summary Statistics for Primary Outcome`,
      Treatment = `Summary Statistics for Treatment Outcome`,
      Method = `Method for Populating CI`,
      Required = `Required Columns`
    )
}

#' Prepare continuous outcome data
#'
#' @param data The preprocessed data frame
#' @return A data frame with prepared continuous outcome data
prepare_continuous_data <- function(data) {
  data %>%
    filter(`Type of Outcome` == "Continuous") %>%
    transmute(
      Type = `Type of Outcome`,
      Summary = `Summary Statistics for Primary Outcome`,
      Treatment = `Summary Statistics for Treatment Outcome`
    )
}

#' Create the collapsible tree diagram
#'
#' @param data The prepared outcome data
#' @param width The width of the tree diagram
#' @param height The height of the tree diagram
#' @param fontSize The font size for the tree labels
#' @return A collapsible tree diagram
create_collapsible_tree <- function(Outcome, width, height, fontSize) {
  collapsibleTree(
    Outcome,
    hierarchy = c("Type", "Summary", "Treatment", "Method", "Required"),
    width = width,
    height = height,
    fontSize = fontSize,
    tooltip = TRUE,
    nodeSize = "leafCount",
    collapsed = TRUE
  )
}
