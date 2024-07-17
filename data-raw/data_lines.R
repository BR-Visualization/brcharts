line_colors <- colorBlindness::Blue2DarkOrange18Steps[12:18]
data_lines <- tribble(
  ~stage, ~xstart, ~xend, ~y, ~col, ~xpos, ~ypos,
  "Symptoms", 0, 6, 45, line_colors[1], 0.44444, -1.4,
  "Diagnosis", 1.99998, 8, 40, line_colors[2], 0.36, -1.4,
  "Loss of physical function", 3.66664, 9, 33, line_colors[3], 1, -1.4,
  "Loss of social function", 4.49998, 10, 27, line_colors[4], 0.92, -1.4,
  "Loss of mental function", 5.3333, 11, 20, line_colors[5], 0.92, -1.4,
  "Assisted care", 6.16664, 12, 12, line_colors[6], 0.52, -1.4,
  "Death", 7.8333, 14.2, 2.5, line_colors[7], 0.27778, -1.4
)

usethis::use_data(data_lines, overwrite = TRUE)
