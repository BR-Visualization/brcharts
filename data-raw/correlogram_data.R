## code to prepare `correlogram` dataset goes here
set.seed(1234)
corr <-
  data.frame(matrix(
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

subject_id <- c(seq(1, 100))
corr[, 1] <- subject_id
for (i in seq(1, 10)) {
  corr[, i + 1] <- c(rnorm(100, runif(1, 0, 100), runif(1, 0, 100)))
  corr[, i + 11] <- c(rbinom(
    n = 100,
    size = 1,
    prob = runif(1)
  ))
  corr[, i + 21] <-
    c(sample(c("Low", "Medium", "High"), 100, replace = TRUE))
}

corr <- corr %>%
  select(subject_id:`Benefit.3`, `Risk.6`:`Risk.8`) %>%
  rename(
    `Primary Efficacy` = `Benefit.1`,
    `Secondary Efficacy` = `Benefit.2`,
    `Quality of Life` = `Benefit.3`,
    `Recurring AE` = `Risk.6`,
    `Rare SAE` = `Risk.7`,
    `Liver Toxicity` = `Risk.8`
  )

corr <- corr %>% select(-subject_id)


corr$`Secondary Efficacy` <- rnorm_pre(corr[, 1],
  mean(corr$`Secondary Efficacy`),
  sd(corr$`Secondary Efficacy`),
  r = .6
)
corr$`Recurring AE` <- rnorm_pre(corr[, 1:3],
  mean(corr$`Recurring AE`),
  sd(corr$`Recurring AE`),
  r = c(.3, .2, -.5)
)
corr$`Rare SAE` <- rnorm_pre(corr[, 1:4],
  mean(corr$`Rare SAE`),
  sd(corr$`Rare SAE`),
  r = c(.13, .3, -.09, -.1)
)
corr$`Liver Toxicity` <- rnorm_pre(
  corr[, 1:5],
  mean(corr$`Liver Toxicity`),
  sd(corr$`Liver Toxicity`),
  r = c(-.13, -.1, -.5, -0.1, 0)
)

usethis::use_data(corr, overwrite = TRUE)
