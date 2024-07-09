## code to prepare `correlogram` dataset goes here
set.seed(1234)
df <- data.frame(matrix(
  NA,
  nrow = 100,
  ncol = 31,
  dimnames = list(
    1:100,
    c(
      "subject_id",
      paste0("Benefit ", 1:5),
      paste0("Risk ", 6:10),
      paste0("Benefit ", 11:15),
      paste0("Risk ", 16:20),
      paste0("Benefit ", 21:25),
      paste0("Risk ", 26:30)
    )
  )
))

# Generate 5 continuous variables, 5 binary variables,
# and 5 nominal variables(has 3 levels), each with 100 observations
subject_id <- c(seq(1, 100))
df[, 1] <- subject_id
for (i in seq(1, 10)) {
  df[, i + 1] <- c(rnorm(100, runif(1, 0, 100), runif(1, 0, 100)))
  df[, i + 11] <- c(rbinom(n = 100, size = 1, prob = runif(1)))
  df[, i + 21] <- c(sample(c("Low", "Medium", "High"), 100, replace = TRUE))
}

# Select and rename columns
df <- df %>%
  select(subject_id:`Benefit.3`, `Risk.6`:`Risk.8`) %>%
  rename(
    `Primary Efficacy` = `Benefit.1`,
    `Secondary Efficacy` = `Benefit.2`,
    `Quality of life` = `Benefit.3`,
    `Recurring AE` = `Risk.6`,
    `Rare SAE` = `Risk.7`,
    `Liver Toxicity` = `Risk.8`
  )

# Remove subject_id if needed
df <- df %>% select(-subject_id)

# To compare with corr, ensure the same steps were taken for corr
corr <- df


usethis::use_data(corr, overwrite = TRUE)
