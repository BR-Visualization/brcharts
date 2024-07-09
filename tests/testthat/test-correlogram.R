library(testthat)

# create sample data for correlogram

set.seed(973)
corr <- data.frame(matrix(
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
  corr[, i] <- rnorm(100, runif(1, 0, 100), runif(1, 0, 100))
}

corr <- corr %>% rename(
  "Primary Efficacy" = "Primary.Efficacy",
  "Recurring AE" = "Recurring.AE",
  "Secondary Efficacy" = "Secondary.Efficacy",
  "Rare SAE" = "Rare.SAE",
  "Quality of Life" = "Quality.of.Life",
  "Liver Toxicity" = "Liver.Toxicity"
)

