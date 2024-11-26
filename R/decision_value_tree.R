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
#' @examples
#' \dontrun{
#' # Assuming your data is in a CSV file named "dtable.csv"
#' data <- read.csv("dtable.csv", check.names = FALSE)
#' create_outcome_tree(data)
#' }
create_outcome_tree <- function(data, width = 1000, height = 800, fontSize = 14) {
  # Remove the index column
  data <- data %>% select(-1)

  # For Binary outcomes
  binary_data <- data %>%
    filter(`Type of Outcome` == "Binary") %>%
    select(
      Type = `Type of Outcome`,
      Summary = `Summary Statistics for Primary Outcome`,
      Treatment = `Summary Statistics for Treatment Outcome`,
      Method = `Method for Populating CI`,
      Required = `Required Columns`
    )

  # For Continuous outcomes
  continuous_data <- data %>%
    filter(`Type of Outcome` == "Continuous") %>%
    transmute(
      Type = `Type of Outcome`,
      Summary = `Summary Statistics for Primary Outcome`,
      Treatment = `Summary Statistics for Treatment Outcome`
    )

  # Combine the data
  Outcome <- bind_rows(binary_data, continuous_data) %>%
    filter(!is.na(Type))

  # Create the collapsible tree
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
