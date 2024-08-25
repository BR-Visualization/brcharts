
# Function to tests for forest_dot_plot.R function
  test_that("forest_dot_plot", {
    # Test 1
    test_data <- structure(
      list(
        Factor = c("Benefit", "Benefit", "Benefit", "Risk"),
        Grouped_Outcome = c(
          "Clinical Assessment",
          "Clinical Assessment",
          "Quality of Life",
          "Adverse Event"
        ),
        Outcome = c(
          "Primary Efficacy",
          "Secondary Efficacy",
          "HR Quality of Life",
          "Reoccurring AE"
        ),
        Statistics = c(
          "% Achieving Remission",
          "Mean Change from Baseline",
          "Mean Change from Baseline",
          "Event Rate"
        ),
        Type = c("Binary", "Continuous", "Continuous", "Binary"),
        Outcome_Status = c("Identified", "Identified", "Identified", "Identified"),
        Filter = c("None", "None", "None", "None"),
        Category = c("All", "All", "All", "All"),
        Trt1 = c("Drug A", "Drug A", "Drug A", "Drug A"),
        Trt2 = c("Placebo", "Placebo", "Placebo", "Placebo"),
        Prop1 = c(0.46, NA, NA, 0.19),
        Prop2 = c(0.05, NA, NA, 0.03),
        N1 = c(1000L, 1000L, 1000L, 1000L),
        N2 = c(1000L, 1000L, 1000L, 1000L),
        Mean1 = c(NA, 65L, 60L, NA),
        Mean2 = c(NA, 20L, 9L, NA),
        Sd1 = c(NA, 63L, 60L, NA),
        Sd2 = c(NA, 16L, 8L, NA)
      ),
      row.names = c(NA, -4L),
      class = "data.frame"
    )

    # Error in eval(e, x, parent.frame()) : object 'Factor' not found

    test_result <- forest_dot_plot(test_data,
                                   filters = "None",
                                   category = "All",
                                   type_graph = "Absolute risk",
                                   type_risk = "Crude proportions",
                                   select_nnx = "Y",
                                   x_scale_fixed_free = "Fixed",
                                   # ci_method = "Supplied",
                                   exclude_outcome = "None")

    expect_s3_class(test_result, "ggplot")
})

