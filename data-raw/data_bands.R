data_bands <- tribble(
  ~level, ~ystart, ~yend, ~col,
  "Mild [35-48)", 35, 48.5, "#ECEDED",
  "Moderate [15-35)", 15, 35, "#BCBCBC",
  "Severe [0-15)", 0, 15, "#888888"
)

usethis::use_data(data_bands, overwrite = TRUE)
