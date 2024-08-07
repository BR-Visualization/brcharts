# Test if the new plot is identical to the saved plot
# Example 1 - manuscript example
plot1 <- stacked_barchart(
  data = comp_outcome,
  chartcolors = colfun()$fig12_colors,
  xlabel = "Study Week"
)
ggsave(plot1, filename = "test_stacked_barchart1.png", width = 7, height = 5)

test_stacked_barchart1 <- png::readPNG("test_stacked_barchart1.png")
stacked_barchart1 <- png::readPNG(paste0(test_path(), "/snapshots/stacked_barchart1.png"))

test1 <- paste(100 * sum(test_stacked_barchart1 == stacked_barchart1) /
  length(test_stacked_barchart1), "%")
expect_equal(test1, "100 %")

file.remove("test_stacked_barchart1.png")

# Example 2 - unequal number of subjects across treatments
comp_outcome2 <- comp_outcome[
  (comp_outcome$trtn == 1 & comp_outcome$usubjid %in% c(1:40)) |
    (comp_outcome$trtn == 2 & comp_outcome$usubjid %in% c(101:160)) |
    (comp_outcome$trtn == 3 & comp_outcome$usubjid %in% c(201:250)),
]
plot2 <- stacked_barchart(
  data = comp_outcome2,
  chartcolors = colfun()$fig12_colors,
  xlabel = "Study Week"
)
ggsave(plot2, filename = "test_stacked_barchart2.png", width = 7, height = 5)

test_stacked_barchart2 <- png::readPNG("test_stacked_barchart2.png")
stacked_barchart2 <- png::readPNG(paste0(test_path(), "/snapshots/stacked_barchart2.png"))

test2 <- paste(100 * sum(test_stacked_barchart2 == stacked_barchart2) /
  length(test_stacked_barchart2), "%")
expect_equal(test2, "100 %")

file.remove("test_stacked_barchart2.png")

# Test if the function throws an error when a required field is missing
comp_outcome3 <- comp_outcome %>% select(
  usubjid, visit, trt, trtn, brcatn
)

test_that("test that stacked_barchart returns an error when a required field
          is missing", {
  expect_error(
    stacked_barchart(
      data = comp_outcome3,
      chartcolors = colfun()$fig12_colors,
      xlabel = "Study Week"
    ),
    "You are missing a required variable in your dataframe: brcat",
    fixed = TRUE
  )
})

# Test if the function returns warning message concerning missing data
comp_outcome4 <- comp_outcome[!(comp_outcome$trtn == 1 &
  comp_outcome$usubjid %in% c(1:40) & comp_outcome$visit == 5), ]

test_that("test that stacked_barchart returns warning message concerning missing data", {
  expect_warning(stacked_barchart(
    data = comp_outcome4,
    chartcolors = colfun()$fig12_colors,
    xlabel = "Study Week"
  ))
})
