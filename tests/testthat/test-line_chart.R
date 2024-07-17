# create test data for line_chart
func <- function(x) 47.982 - 0.0305 * x - 0.57525 * x^2 + 0.0245 * x^3

# testing line_chart for ggplot object

test_that("line_chart() will ouput a ggplot object", {
  expect_true(inherits(line_chart(func = func,
                                  data_bands = data_bands,
                                  data_lines = data_lines,
                                  xmin = 0,
                                  xmax = 14.2,
                                  ymin = 0,
                                  ymax = 50,
                                  xbreaks = seq(0, 14, 2),
                                  ybreaks = seq(0, 50, 5),
                                  xlab = "Years",
                                  ylab = "Functional Score",
                                  legend_title = "Severity: "),
                       "ggplot"))
})

# testing data_bands must have required variables

data_bands1 <- data_bands %>% select(
  ystart, yend
)

test_that("line_chart() expects an error when data_bands is missing required
          variables", {
            expect_error(line_chart(func = func,
                                    data_bands = data_bands1,
                                    data_lines = data_lines,
                                    xmin = 0,
                                    xmax = 14.2,
                                    ymin = 0,
                                    ymax = 50,
                                    xbreaks = seq(0, 14, 2),
                                    ybreaks = seq(0, 50, 5),
                                    xlab = "Years",
                                    ylab = "Functional Score",
                                    legend_title = "Severity: "))
          })

# testing variables in data_bands must be of required type

data_bands2 <- data_bands
data_bands2$ystart <- as.character(data_bands2$ystart)

test_that("line_chart() expects an error when variables in data_bands are not
          of required type", {
            expect_error(line_chart(func = func,
                                    data_bands = data_bands2,
                                    data_lines = data_lines,
                                    xmin = 0,
                                    xmax = 14.2,
                                    ymin = 0,
                                    ymax = 50,
                                    xbreaks = seq(0, 14, 2),
                                    ybreaks = seq(0, 50, 5),
                                    xlab = "Years",
                                    ylab = "Functional Score",
                                    legend_title = "Severity: "))
          })

# testing data_lines must have required variables
data_lines1 <- data_lines %>% rename(color = col)

test_that("line_chart() expects an error when data_lines has misnamed
          variables", {
            expect_error(line_chart(func = func,
                                    data_bands = data_bands,
                                    data_lines = data_lines1,
                                    xmin = 0,
                                    xmax = 14.2,
                                    ymin = 0,
                                    ymax = 50,
                                    xbreaks = seq(0, 14, 2),
                                    ybreaks = seq(0, 50, 5),
                                    xlab = "Years",
                                    ylab = "Functional Score",
                                    legend_title = "Severity: "))
          })

# testing variables in data_lines must be of required type

data_line2 <- data_lines
data_line2$xpos <- as.character(data_lines$xpos)

test_that("line_chart() expects an error when variables in data_lines are not
          of required type", {
            expect_error(line_chart(func = func,
                                    data_bands = data_bands,
                                    data_lines = data_lines2,
                                    xmin = 0,
                                    xmax = 14.2,
                                    ymin = 0,
                                    ymax = 50,
                                    xbreaks = seq(0, 14, 2),
                                    ybreaks = seq(0, 50, 5),
                                    xlab = "Years",
                                    ylab = "Functional Score",
                                    legend_title = "Severity: "))
          })

# testing line_chart's ability to handle missing data

data_lines3 <- data_lines
data_lines3$y[1] <- NA

test_that("line_chart() will return a custom warning message concerning
missing data", {
  expect_warning(line_chart(func = func,
                            data_bands = data_bands,
                            data_lines = data_lines3,
                            xmin = 0,
                            xmax = 14.2,
                            ymin = 0,
                            ymax = 50,
                            xbreaks = seq(0, 14, 2),
                            ybreaks = seq(0, 50, 5),
                            xlab = "Years",
                            ylab = "Functional Score",
                            legend_title = "Severity: "))
})

test_that("line_chart() will return a visible object with
missing data", {
  expect_visible(line_chart(func = func,
                            data_bands = data_bands,
                            data_lines = data_lines3,
                            xmin = 0,
                            xmax = 14.2,
                            ymin = 0,
                            ymax = 50,
                            xbreaks = seq(0, 14, 2),
                            ybreaks = seq(0, 50, 5),
                            xlab = "Years",
                            ylab = "Functional Score",
                            legend_title = "Severity: "))
})
