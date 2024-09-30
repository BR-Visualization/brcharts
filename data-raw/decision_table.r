# Load the tibble package for creating tibbles (modern data frames)
library(tibble)

# Create a tibble named 'decision_table' with information about different types of outcomes and their statistics
decision_table <- tibble(
  # Define the type of outcome: 10 Binary outcomes followed by 2 Continuous outcomes
  `Type of Outcome` = rep(c("Binary", "Continuous"), times = c(10, 2)),
  
  # Define the summary statistics for the primary outcome
  `Summary Statistics for Primary Outcome` = c(
    rep("Proportion", 6),      # First 6 rows are Proportions
    rep("Incidence Rate", 2),  # Next 2 are Incidence Rates
    rep("Event Rate", 2),      # Next 2 are Event Rates
    rep("Continuous", 2)       # Last 2 are Continuous
  ),
  
  # Define the summary statistics for the treatment outcome
  `Summary Statistics for Treatment Outcome` = c(
    rep("Risk Difference", 2), # First 2 are Risk Differences
    rep("Relative Risk", 2),   # Next 2 are Relative Risks
    rep("Odds Ratio", 2),      # Next 2 are Odds Ratios
    rep("Incidence Rate", 2),  # Next 2 are Incidence Rates
    rep("Event Rate", 2),      # Next 2 are Event Rates
    rep("Continuous", 2)       # Last 2 are Continuous
  ),
  
  # Define the method for populating confidence intervals
  # Alternates between "Supplied CI" and "Calculated CI" for each pair of rows
  `Method for Populating CI` = rep(c("Supplied CI", "Calculated CI"), 6),
  
  # Define the required columns for each type of analysis
  # Each entry specifies the necessary data for the corresponding row
  `Required Columns` = c(
    "Prop1 (or nSub1 and N1), Prop2 (or nSub2 and N2), Diff_LowerCI, Diff_UpperCI",
    "Prop1 (or nSub1), Prop2 (or nSub2), N1, N2",
    "Prop1 (or nSub1 and N1), Prop2 (or nSub2 and N2), RelRisk_LowerCI, RelRisk_UpperCI",
    "Prop1 (or nSub1), Prop2 (or nSub2), N1, N2",
    "Prop1 (or nSub1 and N1), Prop2 (or nSub2 and N2), OddsRatio_LowerCI, OddsRatio_UpperCI",
    "Prop1 (or nSub1), Prop2 (or nSub2), N1, N2",
    "IncRate1 (or nSub1 and 100PYAR1), IncRate2 (or nSub2 and 100PYAR2), Diff_IncRate_LowerCI, Diff_IncRate_UpperCI",
    "IncRate1 (or nSub1), IncRate2 (or nSub2), 100PYAR1, 100PYAR2",
    "EventRate1 (or nEvent1 and 100PEY1), EventRate2 (or nEvent2 and 100PEY2), Diff_EventRate_LowerCI, Diff_EventRate_UpperCI",
    "EventRate1 (or nEvent1), EventRate2 (or nEvent2), 100PEY1, 100PEY2",
    "Mean1, Mean2, Diff_LowerCI, Diff_UpperCI",
    "Mean1, Mean2, Sd1 (or Se1), Sd2 (or Se2), N1, N2"
  )
)

usethis::use_data(decision_table, overwrite = TRUE)