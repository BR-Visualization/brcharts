#' Create Value Tree
#'
#' Generate example value tree using provided mermaid graph syntax.
#' @inheritParams DiagrammeR::mermaid
#'
#' @param diagram A character string containing the mermaid graph syntax for the value tree.
#' @return A DiagrammeR mermaid object representing the value tree
#' @export
#'
#' @details
#' The function takes a mermaid graph syntax as input to create the value tree.
#' This allows for flexibility in defining the tree structure and node relationships.
#'
#' The example provided demonstrates a typical structure based on information from the `effects_table`:
#' - Top-level node represents the overall Benefit-Risk Balance
#' - Second-level nodes represent `Factor` for either Benefits or Risks (e.g., Benefit, Risk)
#' - Third-level nodes represent `Outcomes` for the specific categories of Benefits and Risks
#' - Fourth-level nodes include `Statistics` for specific endpoints or measures for each Outcome
#'
#' Colors are used to differentiate between different levels or types of effects:
#' - Green (#7ABD7E) for top-level and major category nodes
#' - Yellow (#FFE733) for secondary category nodes
#' - Orange (#FFAA1C) for specific endpoint nodes
#' - Grey (#C6C6C6) for nodes which have been pruned from the tree
#'
#' The mermaid syntax used in this function follows these conventions:
#' - The graph is defined as left-to-right (LR) using `graph LR;`
#' - Nodes are defined with letters (A, B, C, etc.) and their content is enclosed in parentheses
#' - Node content is wrapped in <B> tags for bold text
#' - Relationships between nodes are defined using arrows (-->)
#' - Node styles, including fill colors, are defined using the `style` keyword
#'
#' For example:
#' A(<B>Node A</B>)-->B(<B>Node B</B>)
#' style A fill:#7ABD7E
#'
#' This creates two nodes, A and B, with A linking to B, and sets the fill color of A to green.
#'
#' @examples
#' value_tree(
#'   diagram =
#'     "graph LR;
#'   A(<B>Benefit-Risk Balance</B>)-->B(<B>Benefits</B>)
#'   B-->C(<B>Primary Efficacy</B>)
#'   B-->D(<B>Secondary Efficacy</B>)
#'   B-->E(<B>Quality of life</B>)
#'   C-->F(<B>% Success</B>)
#'   D-->G(<B>Mean change</B>)
#'   E-->H(<B>Mean change</B>)
#'   A-->I(<B>Risks</B>)
#'   I-->J(<B>Recurring AE</B>)
#'   I-->K(<B>Rare SAE</B>)
#'   I-->L(<B>Liver Toxicity</B>)
#'   J-->M(<B>Event rate</B>)
#'   K-->N(<B>% Event</B>)
#'   L-->O(<B>% Event</B>)
#'   style A fill:#7ABD7E
#'   style B fill:#7ABD7E
#'   style I fill:#7ABD7E
#'   style C fill:#FFE733
#'   style D fill:#FFE733
#'   style E fill:#FFE733
#'   style J fill:#FFE733
#'   style K fill:#FFE733
#'   style L fill:#C6C6C6
#'   style F fill: #FFAA1C
#'   style G fill: #FFAA1C
#'   style H fill: #FFAA1C
#'   style M fill: #FFAA1C
#'   style N fill: #FFAA1C
#'   style O fill: #C6C6C6
#'   "
#' )
value_tree <- function(diagram, ...) {
  if (is.null(diagram) || nchar(trimws(diagram)) == 0) {
    stop("A diagram specification must be provided")
  }

  # Basic syntax check
  if (!grepl("^graph (TB|BT|RL|LR);", trimws(diagram))) {
    stop("Invalid graph specification: Must start with 'graph TB;', 'graph BT;', 'graph RL;', or 'graph LR;'")
  }

  # Attempt to create the DiagrammeR object
  tryCatch({
    result <- DiagrammeR::mermaid(diagram, ...)
    return(result)
  }, error = function(e) {
    stop(paste("Invalid graph specification:", e$message))
  })
}
