# Helper function to create sample data
create_sample_data <- function() {
  data.frame(
    rate = runif(10, 0, 1),
    neword = 1:10,
    treatment = rep(c("A", "B"), each = 5),
    type = rep(c("Binary", "Continuous"), each = 5),
    factor = rep(c("Benefit", "Risk"), each = 5),
    diff = runif(10, -0.5, 0.5),
    lower = runif(10, -1, 0),
    upper = runif(10, 0, 1)
  )
}

# Function to test generate_fig_lft
test_generate_fig_lft <- function() {
  sample_data <- create_sample_data()
  test_that("generate_fig_lft returns a ggplot object", {
    p <- generate_fig_lft(
      data = sample_data,
      fact_subset = "Benefit",
      type_subset = "Binary",
      xlabel = "Treatment Response",
      select_nnx = "Yes"
    )
    expect_s3_class(p, "ggplot")
    expect_true("GeomPoint" %in% sapply(p$layers, function(layer) class(layer$geom)[1]))
  })
}

# Function to test generate_fig_rft
test_generate_fig_rft <- function() {
  sample_data <- create_sample_data()
  test_that("generate_fig_rft returns a ggplot object", {
    p <- generate_fig_rft(
      data = sample_data,
      fact_subset = "Benefit",
      type_subset = "Binary",
      xlabel = "Treatment Difference with 95% CI",
      select_nnx = "Yes"
    )
    expect_s3_class(p, "ggplot")
    expect_true("GeomPoint" %in% sapply(p$layers, function(layer) class(layer$geom)[1]))
    expect_true("GeomLinerange" %in% sapply(p$layers, function(layer) class(layer$geom)[1]))
  })
}

# Execute tests
test_generate_fig_lft()
test_generate_fig_rft()
