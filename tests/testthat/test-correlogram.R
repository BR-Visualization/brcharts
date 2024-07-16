library(testthat)

# create sample data for correlogram

set.seed(973)
corr1 <-
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

# Generate 5 continuous variables, 5 binary variables,
# and 5 ordinal variables(has 3 levels), each with 100 observations
subject_id <- c(seq(1, 100))
corr1[, 1] <- subject_id
for (i in seq(1, 10)) {
  corr1[, i + 1] <- c(rnorm(100, runif(1, 0, 100), runif(1, 0, 100)))
  corr1[, i + 11] <- c(rbinom(
    n = 100,
    size = 1,
    prob = runif(1)
  ))
  corr1[, i + 21] <-
    factor(c(sample(
      c("Low", "Medium", "High"), 100,
      replace = TRUE
    )))
}
corr1 <- select(corr1, -c(subject_id))

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

# testing the accuracy of create_correlogram's calculations

# create a corresponding "attributes" dataframe for testing correlation
# calculations
classes <- numeric()
shortcs <- numeric()
for (i in 2:length(corr1)) {
  ifelse(
    all(corr1[[i]] %in% c(0, 1)),
    c(classes[i] <- "binary", shortcs[i] <- "b"),
    ifelse(
      is.numeric(corr1[[i]]),
      c(classes[i] <- "continuous", shortcs[i] <- "c"),
      ifelse(
        is.factor(corr1[[i]]),
        c(classes[i] <- "ordinal", shortcs[i] <- "o")
      )
    )
  )
}


df_attribs <- data.frame(
  names = c(colnames(corr1)),
  category = c(classes),
  shortc = c(shortcs)
)

# testing the accuracy of continuous correlations

df_attribs1 <- df_attribs %>% filter(shortc == "c")
name1 <- c(df_attribs1$names)
corr5 <- corr1 %>% select(which(names(corr1) %in% name1))

mat <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat) <- list(names(corr5), names(corr5))

mat1 <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat1) <- list(names(corr5), names(corr5))

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    mat[i, j] <- cor(corr5[, i], corr5[, j])

    mean_x <- mean(corr5[, i])
    mean_y <- mean(corr5[, j])
    numerator <- sum((corr5[, i] - mean_x) * (corr5[, j] - mean_y))
    denominator <- sqrt(sum((corr5[, i] - mean_x)^2) *
      sum((corr5[, j] - mean_y)^2))
    r <- numerator / denominator
    mat1[i, j] <- r
  }
}

test_that("create_correlogram() correctly calculates correlations for continuous
  variables", {
  expect_equal(mat, mat1)
})

# testing the accuracy of binary correlations

df_attribs1 <- df_attribs %>% filter(shortc == "b")
name1 <- c(df_attribs1$names)
corr5 <- corr1 %>% select(which(names(corr1) %in% name1))

mat <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat) <- list(names(corr5), names(corr5))

mat1 <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat1) <- list(names(corr5), names(corr5))

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    mat[i, j] <- phi(corr5[, i], corr5[, j])

    x <- corr5[, i]
    y <- corr5[, j]

    contingency_table <- table(x, y)

    n11 <- contingency_table[2, 2]
    n10 <- contingency_table[2, 1]
    n01 <- contingency_table[1, 2]
    n00 <- contingency_table[1, 1]

    phir <- signif((n11 * n00 - n10 * n01) /
                   sqrt((n11 + n10) * (n01 + n00) * (n11 + n01) * (n10 + n00)),
                   3)
    mat1[i, j] <- phir
  }
}

test_that("create_correlogram() correctly calculates correlations for binary
  variables", {
    expect_equal(mat, mat1)
  })

# testing the accuracy of binary/continuous correlations

df_attribs1 <- df_attribs %>% filter(shortc == "c" | shortc == "b")
name1 <- c(df_attribs1$names)
corr5 <- corr1 %>% select(which(names(corr1) %in% name1))

mat <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat) <- list(names(corr5), names(corr5))

mat1 <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat1) <- list(names(corr5), names(corr5))

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    type <- paste0(xattr, yattr)
    ifelse(
      type == "cb",
      # calculates point biserial correlation with either two binary or a
      # continuous variable as the x attribute followed by a binary variable
      # as the y attribute.
      mat[i, j] <- biserial.cor(corr5[, i], corr5[, j]),
      ifelse(
        type %in% c("bc"),
        # calculates point biserial correlation with a binary variable as
        # the x attribute followed by a continuous variable as the y
        # attribute.
        mat[i, j] <-
          biserial.cor(corr5[, j], corr5[, i]),
        mat[i, j] <- NA
      )
    )
  }
}

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    type <- paste0(xattr, yattr)
    if (type == "cb") {
      # perform manual calculation of point biserial correlation
      x <- as.numeric(corr5[, i])
      y <- corr5[, j]
      n <- length(x)
      mean_x <- mean(x)
      mean_y <- mean(y)
      mat1[i, j] <- sum((x - mean_x) * (y - mean_y)) /
        sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
    } else if (type == "bc") {
      # perform manual calculation of point biserial correlation
      x <- corr5[, j]
      y <- as.numeric(corr5[, i])
      n <- length(x)
      mean_x <- mean(x)
      mean_y <- mean(y)
      mat1[i, j] <- sum((x - mean_x) * (y - mean_y)) /
        sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
    } else {
      mat1[i, j] <- NA
    }
  }
}
mat1 <- -mat1

test_that(
  "create_correlogram() correctly calculates correlations for continuous/binary
  combinations of variables",
  {
    expect_equal(mat, mat1)
  }
)

# testing the accuracy of ordinal correlations

df_attribs1 <- df_attribs %>% filter(shortc == "o")
name1 <- c(df_attribs1$names)
corr5 <- corr1 %>% select(which(names(corr1) %in% name1))

mat <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat) <- list(names(corr5), names(corr5))

mat1 <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat1) <- list(names(corr5), names(corr5))

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    type <- paste0(xattr, yattr)
    mat[i, j] <- cor(rank(corr5[, i]), rank(corr5[, j]))
    x <- rank(corr5[, i])
    y <- rank(corr5[, j])
    n <- length(x)
    mat1[i, j] <- cor(x, y, method = "spearman")
  }
}

test_that("create_correlogram() correctly calculates correlations for ordinal
  variables", {
  expect_equal(mat, mat1)
})

# testing the accuracy of continuous/ordinal correlations

df_attribs1 <- df_attribs %>% filter(shortc == "c" | shortc == "o")
name1 <- c(df_attribs1$names)
corr5 <- corr1 %>% select(which(names(corr1) %in% name1))

mat <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat) <- list(names(corr5), names(corr5))

mat1 <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat1) <- list(names(corr5), names(corr5))

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    type <- paste0(xattr, yattr)
    ifelse(type == "co",
      # calculates modified Pearson correlation with nonparametric
      # Spearman rank correlation, considering a continuous variable
      # as the x attribute and ordinal variable as the y attribute.
      mat[i, j] <- cor(corr1[, i], rank(corr1[, j])),
      ifelse(type == "oc", # calculates modified Pearson correlation with
        # non parametric Spearman rank correlation, considering an
        # ordinal variable as the x attribute and continuous variable
        # as the y attribute.
        mat[i, j] <- cor(rank(corr1[, i]), corr1[, j]),
        mat[i, j] <- NA
      )
    )
  }
}

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    type <- paste0(xattr, yattr)
    if (type %in% c("co", "oc")) {
      # Modified Pearson for continuous-ordinal or ordinal-continuous
      if (type == "co") {
        x <- corr1[, i]
        y <- rank(corr1[, j])
      } else {
        x <- rank(corr1[, i])
        y <- corr1[, j]
      }
      n <- length(x)
      mean_x <- mean(x)
      mean_y <- mean(y)
      mat1[i, j] <- sum((x - mean_x) * (y - mean_y)) /
        sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
    } else {
      mat1[i, j] <- NA
    }
  }
}

test_that(
  "create_correlogram() correctly calculates correlations for continuous/ordinal
  combinations of variables",
  {
    expect_equal(mat, mat1)
  }
)

# testing the accuracy of ordinal/binary correlations

df_attribs1 <- df_attribs %>% filter(shortc == "o" | shortc == "b")
name1 <- c(df_attribs1$names)
corr5 <- corr1 %>% select(which(names(corr1) %in% name1))

mat <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat) <- list(names(corr5), names(corr5))

mat1 <- data.frame(matrix(NA, nrow = ncol(corr5), ncol = ncol(corr5)))
dimnames(mat1) <- list(names(corr5), names(corr5))

for (i in seq(1, ncol(corr5))) {
  for (j in seq(1, ncol(corr5))) {
    xattr <-
      df_attribs[df_attribs$names %in% names(corr5)[i], ][["shortc"]]
    yattr <-
      df_attribs[df_attribs$names %in% names(corr5)[j], ][["shortc"]]

    type <- paste0(xattr, yattr)
    if (type %in% c("ob", "bo")) {
      if (type == "ob") {
        x <- corr5[, j]
        y <- corr5[, i]
      } else if (type == "bo") {
        x <- corr5[, i]
        y <- corr5[, j]
      }

      mat[i, j] <- enframe(wilcoxonRG(table(x, y)))[1, 2]
    } else {
      mat[i, j] <- NA
    }
  }
}

for (i in seq_along(names(corr5))) {
  for (j in seq_along(names(corr5))) {
    xattr <- df_attribs[df_attribs$names == names(corr5)[i], "shortc"]
    yattr <- df_attribs[df_attribs$names == names(corr5)[j], "shortc"]
    type <- paste0(xattr, yattr)

    if (type %in% c("ob", "bo")) {
      if (type == "ob") {
        x <- corr5[, j]
        y <- corr5[, i]
      } else if (type == "bo") {
        x <- corr5[, i]
        y <- corr5[, j]
      }

      n <- length(x)
      n_1 <- sum(x)
      n_2 <- n - n_1

      m_1 <- mean(rank(y)[x == 1])
      m_2 <- mean(rank(y)[x == 0])
      r_gb <- signif((2 / n_1) * (m_2 - ((n + 1) / 2)), 3)

      mat1[i, j] <- r_gb
    } else {
      mat1[i, j] <- NA
    }
  }
}

test_that(
  "create_correlogram() correctly calculates correlations for ordinal/binary
  combinations of variables",
  {
    expect_equal(mat, mat1)
  }
)

# testing create_correlogram's ability to handle incorrectly formatted variables

set.seed(973)
corr4 <-
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
corr4[, 1] <- subject_id
for (i in seq(1, 10)) {
  corr4[, i + 1] <- c(rnorm(100, runif(1, 0, 100), runif(1, 0, 100)))
  corr4[, i + 11] <- c(rbinom(
    n = 100,
    size = 1,
    prob = runif(1)
  ))
  corr4[, i + 21] <-
    c(sample(c("Low", "Medium", "High"), 100, replace = TRUE))
}

test_that(
  "create_correlogram() will return an error and custom message if there is a
  character-formatted variable in the dataframe",
  {
    expect_error(create_correlogram(corr4))
  }
)
