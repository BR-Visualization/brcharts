library(testthat)

# create sample data for correlogram

set.seed(973)
corr1 <- data.frame(matrix(
  NA,
  nrow = 100,
  ncol = 6,
  dimnames = list(
    1:100,
    c(
      "Primary Efficacy",
      "Secondary Efficacy",
      "Quality of Life",
      "Recurring AE",
      "Rare SAE",
      "Liver Toxicity"
    )
  )
))

for (i in 1:6) {
  corr1[, i] <- rnorm(100, runif(1, 0, 100), runif(1, 0, 100))
}

corr1 <- corr1 %>% dplyr::rename(
  "Primary Efficacy" = "Primary.Efficacy",
  "Recurring AE" = "Recurring.AE",
  "Secondary Efficacy" = "Secondary.Efficacy",
  "Rare SAE" = "Rare.SAE",
  "Quality of Life" = "Quality.of.Life",
  "Liver Toxicity" = "Liver.Toxicity"
)

# testing create_correlogram for ggplot object

test_that("create_correlogram() will ouput a ggplot object", {
  expect_true(inherits(create_correlogram(corr1), "ggplot"))
})

# testing create_correlogram's ability to handle missing data

corr2 <- corr1
corr2[1, 1] <- NA

test_that("create_correlogram() will return a custom warning message concerning
missing data", {
  expect_warning(create_correlogram(corr2))
})

test_that("create_correlogram() will return a ggplot object with
missing data", {
  expect_true(inherits(create_correlogram(corr2), "ggplot"))
})

# testing create_correlogram must have more than one variable

corr3 <- corr2[, 2]

test_that(
  "create_correlogram() will return an error and custom message if there
          is only one column in the dataframe",
  {
    expect_error(create_correlogram(corr3))
  }
)
