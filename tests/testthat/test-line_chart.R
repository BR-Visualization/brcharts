# create test data for line_chart
func <- function(x) 47.982 - 0.0305 * x - 0.57525 * x^2 + 0.0245 * x^3

data_bands <- tribble(
  ~level, ~ystart, ~yend, ~col,
  "Mild [35-48)", 35, 48.5, "#ECEDED",
  "Moderate [15-35)", 15, 35, "#BCBCBC",
  "Severe [0-15)", 0, 15, "#888888"
)

data_lines <- tribble(
  ~stage, ~xstart, ~xend, ~y, ~col, ~xpos, ~ypos,
  "Symptoms", 0, 6, 45, colfun()$fig3_colors[1], 0.44444, -1.4,
  "Diagnosis", 1.99998, 8, 40, colfun()$fig3_colors[2], 0.36, -1.4,
  "Loss of physical function", 3.66664, 9, 33, colfun()$fig3_colors[3], 1, -1.4,
  "Loss of social function", 4.49998, 10, 27, colfun()$fig3_colors[4], 0.92, -1.4,
  "Loss of mental function", 5.3333, 11, 20, colfun()$fig3_colors[5], 0.92, -1.4,
  "Assisted care", 6.16664, 12, 12, colfun()$fig3_colors[6], 0.52, -1.4,
  "Death", 7.8333, 14.2, 2.5, colfun()$fig3_colors[7], 0.27778, -1.4
)



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
                                  legend_title = "Severity: "
  ),
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
