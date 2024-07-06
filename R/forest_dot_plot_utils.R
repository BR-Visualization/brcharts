#' Dot plot data for binary outcomes
#'
#' Prepare Dot plot data for binary outcomes
#'
#' @param data (`data.frame`) Effects table with Binary outcomes
#'
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#' dot_plot_src <- subset(effects_table, Type == "Binary")
#' dot_plot_data <- prepare_dot_data_b(dot_plot_src)
#' head(dot_plot_data)
prepare_dot_data_b <- function(data) {
  error_msg <- paste0(
    check_feature_string(
      data = data, feature = "Prop1",
      plots = "forest", func = is.numeric,
      check_range = c(0, 1)
    ),
    check_feature_string(
      data = data, feature = "Prop2",
      plots = "forest", func = is.numeric,
      check_range = c(0, 1)
    )
  )

  validate(need(error_msg == "", error_msg))

  prop1 <- data$Prop1
  prop2 <- data$Prop2


  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Prepare Dot plot data for binary outcomes")
  ))

  # generate dot plot data
  data.frame(
    rate = c(prop1, prop2),
    treatment = c(
      as.character(data$Trt1),
      as.character(data$Trt2)
    ),
    type = data$Type,
    factor = c(data$Factor, data$Factor),
    outcome = rep(factor(data$Outcome,
                         levels =
                           rev(unique(data$Outcome))
    ), 2),
    group = seq_len(length(prop1))
  )
}


#' Dot plot data for exposure-adjusted rates
#'
#' Prepare Dot plot data for exposure-adjusted rates
#'
#' @param data (`data.frame`) Effects table
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#' dot_plot_src <- subset(effects_table, !is.na(Rate_Type))
#' dot_plot_data <- prepare_dot_data_exp_adj(dot_plot_src)
#' head(dot_plot_data)
prepare_dot_data_exp_adj <- function(data) {
  data <- data[!is.na(data$Rate_Type), ]

  error_msg <- paste0(
    check_feature_string(
      data = data,
      feature = "Rate_Type",
      plots = "forest",
      func = is.character,
      values = c("EventRate", "IncRate")
    )
  )

  validate(need(error_msg == "", error_msg))

  if (nrow(data[data$Rate_Type == "EventRate", ]) > 0){
    error_msg <- paste0(
      check_feature_string(
        data = data[data$Rate_Type == "EventRate", ],
        feature = "EventRate1",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      ),
      check_feature_string(
        data = data[data$Rate_Type == "EventRate", ],
        feature = "EventRate2",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      )
    )
    validate(need(error_msg == "", error_msg))
  }

  if (nrow(data[data$Rate_Type == "IncRate", ]) > 0){
    error_msg <- paste0(
      check_feature_string(
        data = data[data$Rate_Type == "IncRate", ],
        feature = "IncRate1",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      ),
      check_feature_string(
        data = data[data$Rate_Type == "IncRate", ],
        feature = "IncRate2",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      )
    )
    validate(need(error_msg == "", error_msg))
  }

  rate1_risk <- ifelse(data$Rate_Type == "EventRate", data$EventRate1, data$IncRate1)
  rate2_risk <- ifelse(data$Rate_Type == "EventRate", data$EventRate2, data$IncRate2)

  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Prepare Dot plot data for exposure-adjusted rates")
  ))

  # generate dot plot data
  data.frame(
    rate = c(rate1_risk, rate2_risk),
    treatment = c(
      as.character(data$Trt1),
      as.character(data$Trt2)
    ),
    type = rep(data$Type, 2),
    factor = rep(data$Factor, 2),
    outcome = rep(
      factor(paste(data$Outcome, data$Rate_Type)),
      levels =
        rev(unique(paste(data$Outcome, data$Rate_Type))
        ),
      2
    ),
    group = seq_len(length(rate1_risk))
  )

}


#' Forest plot data for exposure-adjusted rates
#'
#' Prepare Forest plot data for exposure-adjusted rates
#'
#' @param data (`data.frame`) Effects table
#' @param change_ref Change reference group ("Y" or "N)
#' @param ci_method (`character`) confidence interval method,
#'  either `Supplied` (taken from the effect table)
#'  or `Calculated` (calculated within the program).
#'
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#' forest_plot_src <- subset(effects_table, !is.na(Rate_Type))
#'
#' forest_plot_data <- prepare_absolute_risk_exp_adj(
#'   forest_plot_src,
#'   "Y",
#'   "Calculated"
#' )
#' head(forest_plot_data)
prepare_absolute_risk_exp_adj <- function(data,
                                          change_ref = c("Y", "N"),
                                          ci_method = c(
                                            "Supplied", "Calculated"
                                          )
) {
  data <- data[!is.na(data$Rate_Type), ]

  error_msg <- paste0(
    check_feature_string(
      data = data,
      feature = "Rate_Type",
      plots = "forest",
      func = is.character,
      values = c("EventRate", "IncRate")
    )
  )

  validate(need(error_msg == "", error_msg))

  if (nrow(data[data$Rate_Type == "EventRate", ]) > 0){
    error_msg <- paste0(
      check_feature_string(
        data = data[data$Rate_Type == "EventRate", ],
        feature = "EventRate1",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      ),
      check_feature_string(
        data = data[data$Rate_Type == "EventRate", ],
        feature = "EventRate2",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      )
    )
    validate(need(error_msg == "", error_msg))
  }

  if (nrow(data[data$Rate_Type == "IncRate", ]) > 0){
    error_msg <- paste0(
      check_feature_string(
        data = data[data$Rate_Type == "IncRate", ],
        feature = "IncRate1",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      ),
      check_feature_string(
        data = data[data$Rate_Type == "IncRate", ],
        feature = "IncRate2",
        plots = "forest",
        func = is.numeric,
        na_check = T,
        check_positive = T
      )
    )
    validate(need(error_msg == "", error_msg))
  }

  change_ref <- match.arg(change_ref)
  ci_method <- match.arg(ci_method)

  rate1_risk <- ifelse(data$Rate_Type == "EventRate",
                       data$EventRate1,
                       data$IncRate1)
  rate2_risk <- ifelse(data$Rate_Type == "EventRate",
                       data$EventRate2,
                       data$IncRate2)

  diff <- rate1_risk - rate2_risk

  if (ci_method == "Supplied") {
    if (nrow(data[data$Rate_Type == "EventRate", ]) > 0){
      error_msg <- paste0(
        check_feature_string(
          data = data[data$Rate_Type == "EventRate", ],
          feature = "Diff_EventRate_LowerCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        ),
        check_feature_string(
          data = data[data$Rate_Type == "EventRate", ],
          feature = "Diff_EventRate_UpperCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        )
      )
      validate(need(error_msg == "", error_msg))
    }

    if (nrow(data[data$Rate_Type == "IncRate", ]) > 0){
      error_msg <- paste0(
        check_feature_string(
          data = data[data$Rate_Type == "IncRate", ],
          feature = "Diff_IncRate_LowerCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        ),
        check_feature_string(
          data = data[data$Rate_Type == "IncRate", ],
          feature = "Diff_IncRate_UpperCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        )
      )
      validate(need(error_msg == "", error_msg))
    }

    lower_risk <- ifelse(data$Rate_Type == "EventRate",
                         data$Diff_EventRate_LowerCI,
                         data$Diff_IncRate_LowerCI)
    upper_risk <- ifelse(data$Rate_Type == "EventRate",
                         data$Diff_EventRate_UpperCI,
                         data$Diff_IncRate_UpperCI)

    if (change_ref == "N") {
      est <- data.frame(
        diff = diff,
        lower = lower_risk,
        upper = upper_risk
      )
    } else {
      est <- data.frame(
        diff = -diff,
        lower = -upper_risk,
        upper = -lower_risk
      )
    }
  } else {
    if (nrow(data[data$Rate_Type == "EventRate", ]) > 0){
      error_msg <- paste0(
        check_feature_string(
          data = data[data$Rate_Type == "EventRate", ],
          feature = "100PEY1",
          plots = "forest",
          func = is.numeric,
          check_positive = T
        ),
        check_feature_string(
          data = data[data$Rate_Type == "EventRate", ],
          feature = "100PEY2",
          plots = "forest",
          func = is.numeric,
          check_positive = T
        )
      )
      validate(need(error_msg == "", error_msg))
    }

    if (nrow(data[data$Rate_Type == "IncRate", ]) > 0){
      error_msg <- paste0(
        check_feature_string(
          data = data[data$Rate_Type == "IncRate", ],
          feature = "100PYAR1",
          plots = "forest",
          func = is.numeric,
          check_positive = T
        ),
        check_feature_string(
          data = data[data$Rate_Type == "IncRate", ],
          feature = "100PYAR2",
          plots = "forest",
          func = is.numeric,
          check_positive = T
        )
      )
      validate(need(error_msg == "", error_msg))
    }

    py1_risk <- ifelse(data$Rate_Type == "EventRate", data[, "100PEY1"], data[, "100PYAR1"])
    py2_risk <- ifelse(data$Rate_Type == "EventRate", data[, "100PEY2"], data[, "100PYAR2"])

    if (change_ref == "N") {
      est <- calculate_diff_rates(
        rate1 = rate1_risk,
        rate2 = rate2_risk,
        py1 = py1_risk,
        py2 = py2_risk
      )
    } else {
      est <- calculate_diff_rates(
        rate1 = rate2_risk,
        rate2 = rate1_risk,
        py1 = py2_risk,
        py2 = py1_risk
      )
    }
  }

  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Prepare Forest plot data for exposure-adjusted rates")
  ))

  # generate forest plot data
  forest_data_risk_b <- data.frame(
    treatment = data$Trt1,
    type = data$Type,
    factor = data$Factor,
    outcome = factor(paste(data$Outcome, data$Rate_Type),
                     levels =
                       rev(unique(paste(data$Outcome, data$Rate_Type))
                       )),
    est,
    group = seq_len(length(rate1_risk))
  )
}


#' Forest plot data for absolute risk
#'
#' Prepare Forest plot data for absolute risk
#'
#' @param data (`data.frame`) Effects table
#' @param change_ref Change reference group ("Y" or "N)
#' @param ci_method (`character`) confidence interval method,
#'  either `Supplied` (taken from the effect table)
#'  or `Calculated` (calculated within the program).
#'
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#' forest_plot_src <- subset(effects_table, !is.na(Prop1))
#' forest_plot_data <- prepare_absolute_risk_data(
#'   forest_plot_src,
#'   "Y",
#'   "Calculated"
#' )
#' head(forest_plot_data)
prepare_absolute_risk_data <-
  function(data,
           change_ref = c("Y", "N"),
           ci_method = c("Supplied", "Calculated")) {
    change_ref <- match.arg(change_ref)
    ci_method <- match.arg(ci_method)

    error_msg <- paste0(
      check_feature_string(
        data = data,
        feature = "Prop1",
        plots = "forest",
        func = is.numeric,
        check_range = c(0, 1)
      ),
      check_feature_string(
        data = data,
        feature = "Prop2",
        plots = "forest",
        func = is.numeric,
        check_range = c(0, 1)
      )
    )
    validate(need(error_msg == "", error_msg))

    prop1 <- data$Prop1
    prop2 <- data$Prop2
    N1 <- data$N1
    N2 <- data$N2

    if (ci_method == "Supplied") {
      error_msg <- paste0(
        check_feature_string(
          data = data,
          feature = "Diff_LowerCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        ),
        check_feature_string(
          data = data,
          feature = "Diff_UpperCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        )
      )

      validate(need(error_msg == "", error_msg))

      if (change_ref == "N") {
        est <- data.frame(
          diff = prop1 - prop2,
          lower = data$Diff_LowerCI,
          upper = data$Diff_UpperCI
        )
      } else {
        est <- data.frame(
          diff = prop2 - prop1,
          lower = -data$Diff_UpperCI,
          upper = -data$Diff_LowerCI
        )
      }
    } else if (ci_method == "Calculated") {
      error_msg <- paste0(
        check_feature_string(
          data = data,
          feature = "N1",
          plots = "forest",
          func = is.integer,
          check_positive = T
        ),
        check_feature_string(
          data = data,
          feature = "N2",
          plots = "forest",
          func = is.integer,
          check_positive = T
        )
      )

      validate(need(error_msg == "", error_msg))
      if (change_ref == "N") {
        est <- calculate_diff_bin(
          prop1 = prop1,
          prop2 = prop2,
          N1 = N1,
          N2 = N2
        )
      } else {
        est <- calculate_diff_bin(
          prop1 = prop2,
          prop2 = prop1,
          N1 = N2,
          N2 = N1
        )
      }
    }

    message(paste0(
      glue("[{format(Sys.time(),'%F %T')}] > "),
      glue("Prepare Forest plot data for absolute risk")
    ))

    # generate forest plot data
    data.frame(
      treatment = data$Trt1,
      factor = data$Factor,
      type = data$Type,
      outcome = factor(data$Outcome,
                       levels =
                         rev(unique(data$Outcome))
      ),
      est,
      group = seq_len(length(prop1))
    )
  }

#' Forest plot data for relative risk
#'
#' Prepare Forest plot data for relative risk
#'
#' @param data (`data.frame`) Effects table
#' @param change_ref Change reference group ("Y" or "N)
#' @param ci_method (`character`) confidence interval method,
#'  either `Supplied` (taken from the effect table)
#'  or `Calculated` (calculated within the program).
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#' forest_plot_src <- subset(effects_table, !is.na(Prop1))
#'
#' forest_plot_data <- prepare_relative_risk_data(
#'   forest_plot_src,
#'   "Y",
#'   "Calculated"
#' )
#' head(forest_plot_data)
#' Forest plot data for relative risk
#'
#' Prepare Forest plot data for relative risk
#'
#' @param data (`data.frame`) Effects table
#' @param change_ref Change reference group ("Y" or "N)
#' @param ci_method (`character`) confidence interval method,
#'  either `Supplied` (taken from the effect table)
#'  or `Calculated` (calculated within the program).
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#' forest_plot_src <- subset(brdata, !is.na(Prop1))
#' forest_plot_data <- prepare_relative_risk_data(
#'   forest_plot_src,
#'   "Y",
#'   "Supplied"
#' )
#' head(forest_plot_data)
#'
#' forest_plot_data <- prepare_relative_risk_data(
#'   forest_plot_src,
#'   "Y",
#'   "Calculated"
#' )
#' head(forest_plot_data)
prepare_relative_risk_data <-
  function(data,
           change_ref = c("Y", "N"),
           ci_method = c("Supplied", "Calculated")) {
    change_ref <- match.arg(change_ref)
    ci_method <- match.arg(ci_method)

    error_msg <- paste0(
      check_feature_string(
        data = data,
        feature = "Prop1",
        plots = "forest",
        func = is.numeric,
        check_range = c(0, 1)
      ),
      check_feature_string(
        data = data,
        feature = "Prop2",
        plots = "forest",
        func = is.numeric,
        check_range = c(0, 1)
      )
    )

    validate(need(error_msg == "", error_msg))

    prop1 <- data$Prop1
    prop2 <- data$Prop2
    N1 <- data$N1
    N2 <- data$N2

    if (ci_method == "Supplied") {
      error_msg <- paste0(
        check_feature_string(
          data = data,
          feature = "RelRisk_LowerCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        ),
        check_feature_string(
          data = data,
          feature = "RelRisk_UpperCI",
          plots = "forest",
          func = is.numeric,
          add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
        )
      )

      validate(need(error_msg == "", error_msg))

      if (change_ref == "N") {
        diff <- prop1 / prop2
        diff[!is.finite(diff)] <- NA
        est <- data.frame(
          diff = diff,
          lower = data$RelRisk_LowerCI,
          upper = data$RelRisk_UpperCI
        )
      } else {
        diff <- prop2 / prop1
        diff[!is.finite(diff)] <- NA
        est <- data.frame(
          diff = diff,
          lower = 1/data$RelRisk_UpperCI,
          upper = 1/data$RelRisk_LowerCI
        )
      }
    } else if (ci_method == "Calculated") {
      error_msg <- paste0(
        check_feature_string(
          data = data,
          feature = "N1",
          plots = "forest",
          func = is.integer,
          check_positive = T
        ),
        check_feature_string(
          data = data,
          feature = "N2",
          plots = "forest",
          func = is.integer,
          check_positive = T
        )
      )

      validate(need(error_msg == "", error_msg))

      if (change_ref == "N") {
        est <- calculate_rel_risk_bin(
          prop1 = prop1,
          prop2 = prop2,
          N1 = N1,
          N2 = N2
        )
      } else {
        est <- calculate_rel_risk_bin(
          prop1 = prop2,
          prop2 = prop1,
          N1 = N2,
          N2 = N1
        )
      }
      est <- est %>% dplyr::rename(diff = rr)
    }

    message(paste0(
      glue("[{format(Sys.time(),'%F %T')}] > "),
      glue("Prepare Forest plot data for relative risk")
    ))

    # generate forest plot data
    data.frame(
      treatment = data$Trt1,
      factor = data$Factor,
      type = data$Type,
      outcome = factor(data$Outcome,
                       levels =
                         rev(unique(data$Outcome))
      ),
      est,
      group = seq_len(length(prop1))
    )
  }

#' Forest plot data for odds ratio
#'
#' Prepare Forest plot data for odds ratio
#'
#' @param data (`data.frame`) Effects table
#' @param change_ref Change reference group ("Y" or "N)
#' @param ci_method (`character`) confidence interval method,
#'  either `Supplied` (taken from the effect table)
#'  or `Calculated` (calculated within the program).
#'
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#'
#' forest_plot_src <- subset(effects_table, !is.na(Prop1))
#'
#' forest_plot_data <- prepare_odds_ratio_data(
#'   forest_plot_src,
#'   "Y",
#'   "Calculated"
#' )
#' head(forest_plot_data)
prepare_odds_ratio_data <- function(data, change_ref = c("Y", "N"),
                                    ci_method = c("Supplied", "Calculated")) {
  change_ref <- match.arg(change_ref)
  ci_method <- match.arg(ci_method)

  error_msg <- paste0(
    check_feature_string(
      data = data, feature = "Prop1",
      plots = "forest", func = is.numeric,
      check_range = c(0, 1)
    ),
    check_feature_string(
      data = data, feature = "Prop2",
      plots = "forest", func = is.numeric,
      check_range = c(0, 1)
    )
  )


  validate(need(error_msg == "", error_msg))

  prop1 <- data$Prop1
  prop2 <- data$Prop2
  N1 <- data$N1
  N2 <- data$N2

  if (ci_method == "Supplied") {
    error_msg <- paste0(
      check_feature_string(
        data = data, feature = "OddsRatio_LowerCI",
        plots = "forest", func = is.numeric,
        add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
      ),
      check_feature_string(
        data = data, feature = "OddsRatio_UpperCI",
        plots = "forest", func = is.numeric,
        add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
      )
    )

    validate(need(error_msg == "", error_msg))

    if (change_ref == "N") {
      diff <- (prop1 * (1 - prop2)) /
        (prop2 * (1 - prop1))
      diff[!is.finite(diff)] <- NA

      est <- data.frame(
        diff = diff,
        lower = data$OddsRatio_LowerCI,
        upper = data$OddsRatio_UpperCI
      )
    } else {
      diff <- (prop2 * (1 - prop1)) /
        (prop1 * (1 - prop2))
      diff[!is.finite(diff)] <- NA

      est <- data.frame(
        diff = diff,
        lower = 1/data$OddsRatio_UpperCI,
        upper = 1/data$OddsRatio_LowerCI
      )
    }
  } else if (ci_method == "Calculated") {
    error_msg <- paste0(
      check_feature_string(
        data = data, feature = "N1",
        plots = "forest",
        func = is.integer, check_positive = T
      ),
      check_feature_string(
        data = data, feature = "N2",
        plots = "forest",
        func = is.integer, check_positive = T
      )
    )

    validate(need(error_msg == "", error_msg))
    if (change_ref == "N") {
      est <- calculate_odds_ratio_bin(
        prop1 = prop1,
        prop2 = prop2,
        N1 = N1,
        N2 = N2
      )
    } else {
      est <- calculate_odds_ratio_bin(
        prop1 = prop2,
        prop2 = prop1,
        N1 = N2,
        N2 = N1
      )
    }
    est <- est %>% dplyr::rename(diff = or)
  }

  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Prepare Forest plot data for odds ratio")
  ))

  # generate forest plot data
  data.frame(
    treatment = data$Trt1,
    factor = data$Factor,
    type = data$Type,
    outcome = factor(data$Outcome,
                     levels =
                       rev(unique(data$Outcome))
    ),
    est,
    group = seq_len(length(prop1))
  )
}


#' Function to trigger analysis based on type
#' @param do Type of analysis
#' @param x (`data.frame`) Effects table
#' @param y Change reference group
#' @param z Confidence interval calculation
#'
#' @return data frame for specified type of analysis
#'
#' @export
#'
#' @examples
#'
#' forest_plot_src <- subset(effects_table, !is.na(Prop1))
#'
#' forest_plot_data <- supplied_br_forest(
#'   "Absolute risk",
#'   forest_plot_src,
#'   "Y",
#'   "Calculated"
#' )
#' head(forest_plot_data)
supplied_br_forest <- function(do, x, y, z) {
  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("trigger analysis based on type")
  ))
  switch(do,
         "Absolute risk" = prepare_absolute_risk_data(x, y, z),
         "Relative risk" = prepare_relative_risk_data(x, y, z),
         "Odds ratio" = prepare_odds_ratio_data(x, y, z),
         "Exposure-adjusted rates (per 100 PYs)" =
           prepare_absolute_risk_exp_adj(x, y, z),
         "Nothing" = identity
  )
}

#' Forest plot data for continuous outcomes
#'
#' Prepare Forest plot data for continuous outcomes
#'
#' @param data (`data.frame`) Effects table
#' @param change_ref Change reference group ("Y" or "N)
#' @param ci_method (`character`) confidence interval method,
#'  either `Supplied` (taken from the effect table)
#'  or `Calculated` (calculated within the program).
#' @export
#'
#' @examples
#' forest_plot_src <- subset(effects_table, !is.na(Mean1))
#' forest_plot_data <- prepare_cont_benefits_data(
#'   forest_plot_src,
#'   "N",
#'   "Calculated"
#' )
#' head(forest_plot_data)
prepare_cont_benefits_data <- function(data,
                                       change_ref = c("Y", "N"),
                                       ci_method = c("Supplied", "Calculated")
) {
  change_ref <- match.arg(change_ref)
  ci_method <- match.arg(ci_method)

  error_msg <- paste0(
    check_feature_string(
      data = data,
      feature = "Mean1",
      plots = "forest",
      func = is.numeric
    ),
    check_feature_string(
      data = data,
      feature = "Mean2",
      plots = "forest",
      func = is.numeric
    )
  )

  validate(need(error_msg == "", error_msg))

  mean1_benefit <- data$Mean1
  mean2_benefit <- data$Mean2

  sd1_benefit <- data$Sd1
  sd2_benefit <- data$Sd2

  N1_benefit <- data$N1
  N2_benefit <- data$N2

  if (ci_method == "Supplied") {
    error_msg <- paste0(
      check_feature_string(
        data = data,
        feature = "Diff_LowerCI",
        plots = "forest",
        func = is.numeric,
        add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
      ),
      check_feature_string(
        data = data,
        feature = "Diff_UpperCI",
        plots = "forest",
        func = is.numeric,
        add_msg = "Consider switching the option for 'Use confidence intervals' to 'Calculated';"
      )
    )

    validate(need(error_msg == "", error_msg))

    if (change_ref == "N") {
      est <- data.frame(
        diff = mean1_benefit - mean2_benefit,
        lower = data$Diff_LowerCI,
        upper = data$Diff_UpperCI
      )
    } else {
      est <- data.frame(
        diff = mean2_benefit - mean1_benefit,
        lower = -data$Diff_UpperCI,
        upper = -data$Diff_LowerCI
      )
    }
  } else {
    error_msg <- paste0(
      check_feature_string(
        data = data,
        feature = "N1",
        plots = "forest",
        func = is.integer,
        check_positive = T
      ),
      check_feature_string(
        data = data,
        feature = "N2",
        plots = "forest",
        func = is.integer,
        check_positive = T
      ),
      check_feature_string(
        data = data,
        feature = "Sd1",
        plots = "forest",
        func = is.numeric,
        check_positive = T
      ),
      check_feature_string(
        data = data,
        feature = "Sd2",
        plots = "forest",
        func = is.numeric,
        check_positive = T
      )
    )

    validate(need(error_msg == "", error_msg))

    if (change_ref == "N") {
      est <- calculate_diff_con(
        mean1 = mean1_benefit,
        mean2 = mean2_benefit,
        sd1 = sd1_benefit,
        sd2 = sd2_benefit,
        N1 = N1_benefit,
        N2 = N2_benefit
      )
    } else {
      est <- calculate_diff_con(
        mean1 = mean2_benefit,
        mean2 = mean1_benefit,
        sd1 = sd2_benefit,
        sd2 = sd1_benefit,
        N1 = N2_benefit,
        N2 = N1_benefit
      )
    }
  }

  dot_data_benefit_c <- data.frame(
    rate = c(mean1_benefit, mean2_benefit),
    treatment = c(
      as.character(data$Trt1),
      as.character(data$Trt2)
    ),
    type = data$Type,
    factor = c(data$Factor, data$Factor),
    outcome = rep(factor(data$Outcome,
                         levels =
                           rev(
                             unique(data$Outcome)
                           )
    ), 2),
    group = seq_len(length(mean1_benefit))
  )

  forest_data_benefit_c <- data.frame(
    treatment = data$Trt1,
    type = data$Type,
    factor = data$Factor,
    outcome = factor(data$Outcome,
                     levels =
                       rev(unique(data$Outcome))
    ),
    est,
    group = seq_len(length(mean1_benefit))
  )

  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Prepare Forest plot data for continuous outcomes")
  ))

  return(
    list(
      "dot_data_benefit_c" = dot_data_benefit_c,
      "forest_data_benefit_c" = forest_data_benefit_c
    )
  )
}

#' @describeIn prepare_dot_forest_plot_data adds a variable in dataframe which
#'   identifies whether the dataframe is for Benefit or Risk analysis.
#' @param data (`data.frame`) dataset
#' @param typeval (`character`) Benefit or Risk
#' @param cod (`character`) identifier either c - continuous or b for binary
#' @export
#'
# Get continuous and binary data setup
pre_proc <- function(data, typeval, cod) {
  dfbr <- list()
  dfbr[[paste0("df_ben", cod)]] <- data[data$Type == typeval &
                                          data$Factor == "Benefit", ]
  dfbr[[paste0("df_risk", cod)]] <- data[data$Type == typeval &
                                           data$Factor == "Risk", ]
  return(dfbr)
}

#' Dot and Forest plots data
#'
#' Prepare data for Dot and Forest plots
#'
#' @param data (`data.frame`) dataset.
#' @param drug (`character`) selected drug.
#' @param benefit (`character`) selected benefit.
#' @param risk (`character`) selected risk.
#' @param filters (`character`) selected filter.
#' @param category (`character`) selected category.
#' @param type_risk (`character`) selected way to display risk outcomes
#'   (crude proportions, exposure-adjusted rates per 100 PYs
#' @param type_graph (`character`) selected way to display binary outcomes
#'   (absolute risk, relative risk, odds ratio).
#' @param ci_method (`character`) confidence interval method,
#'  either `Supplied` (taken from the effect table)
#'  or `Calculated` (calculated within the program).
#' @param space_btwn_out_yn  (`character`) control spacing between outcomes
#' @name prepare_dot_forest_plot_data
#' @import dplyr magrittr ggplot2
#' @export
#'
#' @examples
#' dot_plot_src <- subset(effects_table, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' fplot_data <- prepare_dot_forest_plot_data(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   ci_method = "Calculated",
#'   space_btwn_out_yn  = "N"
#' )
#'
#' dot_plot_src <- subset(brdata, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' fplot_data <- prepare_dot_forest_plot_data(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "Gender",
#'   category = c("Female","Male"),
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   ci_method = "Calculated"
#' )
#'
#' head(fplot_data$dot_plot_data)
#'
#' head(fplot_data$forest_plot_data)
prepare_dot_forest_plot_data <- function(data,
                                         drug,
                                         benefit,
                                         risk,
                                         filters,
                                         category,
                                         type_graph,
                                         type_risk,
                                         ci_method = c("Supplied", "Calculated"),
                                         space_btwn_out_yn = "Y"
) {
  ci_method <- match.arg(ci_method)


  error_msg <- paste0(
    check_feature_string(
      data = data,
      feature = "Outcome",
      plots = "forest",
      func = is.character,
      na_check = T,
      check_unique = c(
        "Factor",
        "Grouped_Outcome",
        "Statistics",
        "Outcome_Status",
        "Type"
      )
    ),
    check_feature_string(
      data = data,
      feature = "Filter",
      plots = "forest",
      func = is.character,
      na_check = T
    ),
    check_feature_string(
      data = data,
      feature = "Category",
      plots = "forest",
      func = is.character,
      na_check = T
    ),
    check_feature_string(
      data = data,
      feature = "Factor",
      plots = "forest",
      func = is.character,
      na_check = T,
      values = c("Benefit", "Risk")
    ),
    check_feature_string(
      data = data,
      feature = "Type",
      plots = "forest",
      func = is.character,
      na_check = T,
      values = c("Binary", "Continuous")
    ),
    check_feature_string(
      data = data,
      feature = "Trt1",
      plots = "forest",
      func = is.character,
      na_check = T
    ),
    check_feature_string(
      data = data,
      feature = "Trt2",
      plots = "forest",
      func = is.character,
      na_check = T,
      check_same = T
    )
  )

  validate(need(error_msg == "", error_msg))

  df_filter <-
    data[data$Outcome %in% c(benefit, risk) & data$Trt1 %in% drug, ]

  validate(need(nrow(df_filter) > 0, "filtered effects table is empty"))

  # set up color palette
  # my_colors <- RColorBrewer::brewer.pal(
  #   length(levels(as.factor(data$Trt1))) +
  #     length(levels(as.factor(data$Trt2))),
  #   "Set1"
  # )

  my_colors <- colfun()$fig6_colors

  names(my_colors) <- c(
    levels(as.factor(data$Trt2)),
    levels(as.factor(data$Trt1))
  )


  if (length(my_colors) == 3 & is.na(names(my_colors[3]))) {
    my_colors <- my_colors[1:2]
  }

  col_scale <- ggplot2::scale_color_manual(
    name = "grp",
    values = my_colors[c(df_filter$Trt2, df_filter$Trt1)]
  )

  col_scale2 <- ggplot2::scale_color_manual(
    name = "grp",
    values = my_colors[df_filter$Trt1]
  )


  # data for selected filter and category
  if (filters != "None") {
    df_filter <- df_filter[df_filter$Filter == filters &
                             df_filter$Category %in% category, ]
    df_filter$Outcome <- paste0(df_filter$Category," : ",df_filter$Outcome)
  } else {
    df_filter <- df_filter[df_filter$Filter == "None", ]
  }

  bin_br <- pre_proc(df_filter, "Binary", "_b")
  cnt_br <- pre_proc(df_filter, "Continuous", "_c")

  if (ci_method == "Supplied") {
    # Process for Benefits --------------------------------------------------
    if (nrow(bin_br[["df_ben_b"]]) != 0) {
      dot_data_benefit_b <- prepare_dot_data_b(bin_br[["df_ben_b"]])
      forest_data_benefit_b <- supplied_br_forest(
        type_graph, bin_br[["df_ben_b"]],
        "N",
        "Supplied"
      )
    }
    if (nrow(cnt_br[["df_ben_c"]]) != 0) {
      continuous_benefits_list <- prepare_cont_benefits_data(
        cnt_br[["df_ben_c"]],
        "N",
        "Supplied"
      )
      dot_data_benefit_c <-
        continuous_benefits_list[["dot_data_benefit_c"]]
      forest_data_benefit_c <-
        continuous_benefits_list[["forest_data_benefit_c"]]
    }
    # Process for Risks -----------------------------------------------------
    # for Risks we switch comparator to be Active

    if (nrow(bin_br[["df_risk_b"]]) != 0) {
      if (type_risk == "Crude proportions") {
        dot_data_risk_b <- prepare_dot_data_b(bin_br[["df_risk_b"]])
        forest_data_risk_b <- supplied_br_forest(
          type_graph,
          bin_br[["df_risk_b"]],
          "Y",
          "Supplied"
        )
      } else if (type_risk == "Exposure-adjusted rates (per 100 PYs)") {
        dot_data_risk_b <- prepare_dot_data_exp_adj(bin_br[["df_risk_b"]])
        forest_data_risk_b <- supplied_br_forest(
          "Exposure-adjusted rates (per 100 PYs)",
          bin_br[["df_risk_b"]],
          "Y",
          "Supplied"
        )
      }
    }
  } else if (ci_method == "Calculated") {
    # Process for Benefits --------------------------------------------------
    if (nrow(bin_br[["df_ben_b"]]) != 0) {
      dot_data_benefit_b <- prepare_dot_data_b(bin_br[["df_ben_b"]])
      forest_data_benefit_b <- supplied_br_forest(
        type_graph,
        bin_br[["df_ben_b"]],
        "N",
        "Calculated"
      )
    }

    if (nrow(cnt_br[["df_ben_c"]]) != 0) {
      continuous_benefits_list <- prepare_cont_benefits_data(
        cnt_br[["df_ben_c"]],
        "N",
        "Calculated"
      )
      dot_data_benefit_c <-
        continuous_benefits_list[["dot_data_benefit_c"]]
      forest_data_benefit_c <-
        continuous_benefits_list[["forest_data_benefit_c"]]
    }

    # Process for Risks -----------------------------------------------------
    # for Risks we switch comparator to be Active

    if (nrow(bin_br[["df_risk_b"]]) != 0) {
      if (type_risk == "Crude proportions") {
        dot_data_risk_b <- prepare_dot_data_b(bin_br[["df_risk_b"]])
        forest_data_risk_b <- supplied_br_forest(
          type_graph,
          bin_br[["df_risk_b"]],
          "Y",
          "Calculated"
        )
      } else if (type_risk == "Exposure-adjusted rates (per 100 PYs)") {
        dot_data_risk_b <- prepare_dot_data_exp_adj(bin_br[["df_risk_b"]])
        forest_data_risk_b <- supplied_br_forest(
          "Exposure-adjusted rates (per 100 PYs)",
          bin_br[["df_risk_b"]],
          "Y",
          "Calculated"
        )
      }
    }
  }

  dot_data <- rbind(
    if (exists("dot_data_benefit_c")) {
      dot_data_benefit_c
    },
    if (exists("dot_data_benefit_b")) {
      dot_data_benefit_b
    },
    if (exists("dot_data_risk_b")) {
      dot_data_risk_b
    }
  )

  dot_data$outcome <- as.character(dot_data$outcome)
  dot_data$factor <- as.character(dot_data$factor)

  # Create single label for each outcome ------------------------------------
  dot_plot_data <- create_order_label_der(
    dot_data,
    c("type", "factor", "outcome"),
    c("group"),
    dot_plot_data,
    space_btwn_out_yn
  )

  dot_plot_data$treatment <- factor(dot_plot_data$treatment,
                                    levels = c(
                                      levels(as.factor(data$Trt2)),
                                      levels(as.factor(data$Trt1))
                                    )
  )

  if (filters != "None") {
    dot_plot_data <-
      dot_plot_data %>%
      separate(.data$mylab, c("subgp", "outc"), sep = ":", remove = FALSE) %>%
      group_by(.data$type, .data$factor, .data$subgp) %>%
      dplyr::mutate(
        mylab = ifelse(
          .data$neword != max(.data$neword), .data$outc, .data$mylab
        )
      ) %>% replace_na(list(mylab = "", outc = "")) %>%
      ungroup()
  }

  dot_plot_data$mylab <- paste0("      ", dot_plot_data$mylab)

  # Get labels from dot_plot_data
  forest_data <- rbind(
    if (exists("forest_data_benefit_c")) {
      forest_data_benefit_c
    },
    if (exists("forest_data_benefit_b")) {
      forest_data_benefit_b
    },
    if (exists("forest_data_risk_b")) {
      forest_data_risk_b
    }
  )

  forest_plot_data <- dot_plot_data %>%
    dplyr::filter(.data[["treatment"]] != data$Trt2[1]) %>%
    dplyr::select(-c("rate")) %>%
    dplyr::right_join(forest_data,
                      by = c("treatment", "type", "factor", "outcome", "group")
    )

  forest_plot_data$treatment <- factor(forest_plot_data$treatment,
                                       levels = c(
                                         levels(as.factor(data$Trt2)),
                                         levels(as.factor(data$Trt1))
                                       )
  )


  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Prepare data for Dot and Forest plots")
  ))

  return(
    list(
      dot_plot_data = dot_plot_data,
      forest_plot_data = forest_plot_data,
      col_scale = col_scale,
      col_scale2 = col_scale2
    )
  )
}

#' Populated effects table
#'
#' Generate populated effects table with calculated CIs
#' @param data (`data.frame`) dataset
#'
#' @return Effects tble with calculated CIs
#' @export
#'
#' @examples
#' effects_table <- populated_effects_table(effects_table)
#' head(effects_table)
populated_effects_table <- function(data) {
  res_diff_bin <- calculate_diff_bin(
    prop1 = data$Prop1[data$Type == "Binary"],
    prop2 = data$Prop2[data$Type == "Binary"],
    N1 = data$N1[data$Type == "Binary"],
    N2 = data$N2[data$Type == "Binary"]
  )
  res_rel_risk_bin <- calculate_rel_risk_bin(
    prop1 = data$Prop1[data$Type == "Binary"],
    prop2 = data$Prop2[data$Type == "Binary"],
    N1 = data$N1[data$Type == "Binary"],
    N2 = data$N2[data$Type == "Binary"]
  )

  res_odds_ratio_bin <- calculate_odds_ratio_bin(
    prop1 = data$Prop1[data$Type == "Binary"],
    prop2 = data$Prop2[data$Type == "Binary"],
    N1 = data$N1[data$Type == "Binary"],
    N2 = data$N2[data$Type == "Binary"]
  )

  res_diff_con <- calculate_diff_con(
    mean1 = data$Mean1[data$Type == "Continuous"],
    mean2 = data$Mean2[data$Type == "Continuous"],
    sd1 = data$Sd1[data$Type == "Continuous"],
    sd2 = data$Sd2[data$Type == "Continuous"],
    N1 = data$N1[data$Type == "Continuous"],
    N2 = data$N2[data$Type == "Continuous"]
  )
  res_diff_event_rates <- calculate_diff_rates(
    rate1 = data$EventRate1[data$Factor == "Risk" &
                              data$Type == "Binary"],
    rate2 = data$EventRate2[data$Factor == "Risk" &
                              data$Type == "Binary"],
    py1 = data[data$Factor == "Risk" &
                 data$Type == "Binary", "100PEY1"],
    py2 = data[data$Factor == "Risk" &
                 data$Type == "Binary", "100PEY2"]
  )
  res_diff_inc_rates <- calculate_diff_rates(
    rate1 = data$IncRate1[data$Factor == "Risk" &
                            data$Type == "Binary"],
    rate2 = data$IncRate2[data$Factor == "Risk" &
                            data$Type == "Binary"],
    py1 = data[data$Factor == "Risk" &
                 data$Type == "Binary", "100PYAR1"],
    py2 = data[data$Factor == "Risk" &
                 data$Type == "Binary", "100PYAR2"]
  )

  data$Diff_LowerCI[data$Type == "Binary"] <- res_diff_bin$lower
  data$Diff_UpperCI[data$Type == "Binary"] <- res_diff_bin$upper

  data$Diff_LowerCI[data$Type == "Continuous"] <- res_diff_con$lower
  data$Diff_UpperCI[data$Type == "Continuous"] <- res_diff_con$upper

  data$RelRisk_LowerCI[data$Type == "Binary"] <- res_rel_risk_bin$lower
  data$RelRisk_UpperCI[data$Type == "Binary"] <- res_rel_risk_bin$upper

  data$OddsRatio_LowerCI[data$Type == "Binary"] <- res_odds_ratio_bin$lower
  data$OddsRatio_UpperCI[data$Type == "Binary"] <- res_odds_ratio_bin$upper

  data$Diff_EventRate_LowerCI[data$Factor == "Risk" & data$Type == "Binary"] <-
    res_diff_event_rates$lower
  data$Diff_EventRate_UpperCI[data$Factor == "Risk" & data$Type == "Binary"] <-
    res_diff_event_rates$upper

  data$Diff_IncRate_LowerCI[data$Factor == "Risk" & data$Type == "Binary"] <-
    res_diff_inc_rates$lower
  data$Diff_IncRate_UpperCI[data$Factor == "Risk" & data$Type == "Binary"] <-
    res_diff_inc_rates$upper

  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Generate populated effects table with calculated CIs")
  ))
  data
}

#' Dot plot
#'
#' Create Dot plot
#'
#' @param data  (`character`)\cr Data for figure
#' @param fact_subset  (`character`)\cr filter {data}
#' @param type_subset  (`character`)\cr Selected subset "Binary" or "Continuous"
#' @param type_scale (`character`) selected scale display type
#' @param x_scale_n1_p1 (`character`) fix x-axis scale between 0 and 1
#' @param xlabel  (`character`)\cr Label for x-axis
#' @param ylabel  (`character`)\cr Label for y-axis
#' @param select_nnx  (`character`)\cr show NNT/NNH
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' dot_plot_src <- subset(effects_table, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' fplot_data <- prepare_dot_forest_plot_data(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   ci_method = "Calculated"
#' )
#'
#' generate_fig_lft(fplot_data$dot_plot_data,
#'   fact_subset = "Benefit",
#'   type_subset = "Binary",
#'   xlabel = "Treatment Response\n\n ",
#'   select_nnx = "Yes"
#' )
#'
#' dot_plot_src <- subset(effects_table, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' fplot_data <- prepare_dot_forest_plot_data(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "Gender",
#'   category = c("Female","Male"),
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   ci_method = "Calculated"
#' )
#'
#' generate_fig_lft(fplot_data$dot_plot_data,
#'   fact_subset = "Benefit",
#'   type_subset = "Binary",
#'   xlabel = " Treatment Response\n\n ",
#'   select_nnx = "Yes"
#' )
generate_fig_lft <- function(data,
                             fact_subset = "Benefit",
                             type_subset = "Binary",
                             type_scale = "Fixed",
                             x_scale_n1_p1 = "N",
                             xlabel = NULL,
                             ylabel = NULL,
                             select_nnx) {
  sym_axis_data <- data[data$type == type_subset, ]

  data_max <- max(sym_axis_data$rate, na.rm = TRUE)
  data_min <- min(sym_axis_data$rate, na.rm = TRUE)

  x_min <- relmin(data_min, type_scale)
  x_max <- relmax(data_max, type_scale)

  sub_data <- data[data$factor == fact_subset & data$type == type_subset, ]
  if ("subgp" %in% colnames(sub_data)) {
    subc_data <- data[,c("mylab", "neword", "subgp", "outc")] %>%
      distinct(.keep_all = TRUE) %>%
      mutate(bold_var = ifelse(grepl(":", mylab, fixed = TRUE), 1, 0))
  }

  fig <- ggplot(data = sub_data) +
    geom_point(aes_string(x = "rate", y = "neword", color = "treatment"),
               size = 2, alpha = 1) +

    {if (!("subgp" %in%  colnames(sub_data)))
      scale_y_continuous(
        breaks = sub_data$neword,
        labels = sub_data$mylab)
    } +
    {if ("subgp" %in% colnames(sub_data))
      scale_y_continuous(
        breaks = subc_data$neword,
        labels = labs_bold(
          subc_data[["bold_var"]] , subc_data[["subgp"]], subc_data[["outc"]]
        )
      )
    } +
    scale_fill_brewer(palette = "Dark2") +
    xlab(xlabel) +
    ylab(ylabel) +
    theme_minimal() +
    guides(fill = guide_legend(nrow = 1)) +
    theme(
      plot.title = element_text(vjust = 2.12),
      legend.position = "bottom",
      legend.box.margin = margin(-42, 0, 0, 0),
      legend.title = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid = element_line(colour = "#808080"),
      strip.placement = "outside",
      strip.background = element_rect(
        fill = "grey80",
        size = 1, color = "grey80"
      ),
      plot.margin = unit(c(0, 30, 0, 0), "pt")
    ) +
    coord_cartesian(clip = "off")

  if (type_scale == "Fixed" &
      type_subset %in% c("Binary") & x_scale_n1_p1 == "Y") {
    fig <- fig + scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    )
  } else {
    fig <- fig +
      scale_x_continuous(
        limits = c(x_min, x_max),
        breaks = ggthemes::extended_range_breaks_(x_min, x_max)
      )
  }

  if (select_nnx == "Yes" & !purrr::every(data$rate, is.na)) {
    fig <- fig + scale_x_continuous(
      limits = c(0, 1),
      sec.axis = sec_axis(~ . * 1,
                          name = "",
                          breaks = min(data$rate),
                          labels = " "
      )
    )

    fig
  } else {
    fig
  }
  message(glue('[{format(Sys.time(),"%F %T")}] > Create Dot plot'))
  fig
}


#' Forest plot
#'
#' Create Forest plot
#'
#' @param data  (`character`)\cr Data for figure
#' @param fact_subset  (`character`)\cr filter {data}
#' @param type_subset  (`character`)\cr Selected subset "Binary" or "Continuous"
#' @param type_scale (`character`) selected scale display type
#' @param x_scale_n1_p1 (`character`) fix x-axis scale between -1 and 1
#' @param xlabel  (`character`)\cr Label for x-axis
#' @param ylabel  (`character`)\cr Label for y-axis
#' @param scale_x (`character`)\cr Type of scale for x-axis "Identity" or
#' "log10"
#' @param select_nnx  (`character`)\cr show NNT/NNH
#'
#' @import magrittr dplyr ggplot2
#'
#' @export
#' @examples
#' dot_plot_src <- subset(effects_table, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' fplot_data <- prepare_dot_forest_plot_data(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   ci_method = "Calculated"
#' )
#' forest_plot_data <- subset(
#'   fplot_data$forest_plot_data,
#'   factor == "Benefit" & type == "Binary"
#' )
#' generate_fig_rft(
#'   data = forest_plot_data,
#'   fact_subset = "Benefit",
#'   type_subset = "Binary",
#'   xlabel = "Treatment Difference with 95% CI\n",
#'   select_nnx = "No"
#' )
generate_fig_rft <- function(data,
                             xlabel = NULL,
                             ylabel = NULL,
                             fact_subset = "Benefit",
                             type_subset = "Binary",
                             type_scale = "Fixed",
                             scale_x = "Identity",
                             x_scale_n1_p1 = "N",
                             select_nnx) {
  sym_axis_data <- data[data$type == type_subset, ]
  sub_data <- data[data$factor == fact_subset & data$type == type_subset, ]

  if (tolower(scale_x) == "log10") {
    x_ref <- 1

    # For scale_log10, values of 0 are displayed on the left-most point
    # Set to NA instead
    sub_data$diff <- ifelse(sub_data$diff == 0, NA, sub_data$diff)
    sub_data$lower <- ifelse(sub_data$lower == 0, NA, sub_data$lower)
    sub_data$upper <- ifelse(sub_data$upper == 0, NA, sub_data$upper)

    # To get largest absolute difference from reference line
    all_vals <- abs(log10(
      c(sym_axis_data$lower, sym_axis_data$diff, sym_axis_data$upper)
    ))
    # Remove NA, Inf, and -Inf
    sub_vals <- all_vals[!(all_vals %in% c(NA, -Inf, Inf))]

    max_abs <- max(sub_vals)
    max_x <- 10**ceiling(max_abs)
    min_x <- 10**(-ceiling(max_abs))

    scale_x_code <- scale_x_log10(
      limits = c(min_x, max_x),
      expand = expansion(mult = 0.1),
      labels = scales::label_number(accuracy = 0.01)
    )
  } else {
    x_ref <- 0

    data_min <- min(sym_axis_data$lower, sym_axis_data$diff, na.rm = TRUE)
    data_max <- max(sym_axis_data$upper, sym_axis_data$diff, na.rm = TRUE)

    x_min <- relmin(data_min, type_scale)
    x_max <- relmax(data_max, type_scale)

    max_x <- max(abs(x_min), abs(x_max))

    scale_x_code <- scale_x_continuous(
      limits = c(-max_x, max_x),
      expand = expansion(mult = 0.1),
      labels = scales::label_number(accuracy = 0.01)
    )
  }

  fig <- ggplot2::ggplot(
    data = sub_data,
    aes_string(
      color = "treatment", x = "diff", xmin = "lower", xmax = "upper",
      y = "neword"
    )) +
    geom_point(size = 2, alpha = 1) +
    geom_linerange(size = 1, alpha = 1, show.legend = FALSE) +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(breaks = data$neword, labels = data$mylab) +
    geom_vline(xintercept = x_ref, lty = 2) +
    scale_x_code +
    xlab(paste0(
      glue(
        "\U2190 Favours {data$Trt2[1]}        Favours Active \U2192 \n \n "
      ),
      xlabel
    )) +
    ylab(ylabel) +
    ggtitle(" ") +
    theme_minimal() +
    guides(colour = guide_legend(nrow = 1)
    ) +
    theme(
      plot.title = element_text(vjust = 2.12),
      legend.position = "bottom",
      legend.box.margin = margin(-42, 0, 0, 0),
      legend.title = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid = element_line(colour = "#808080"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = unit(c(0, 0, 0, 30), "pt")
    )
  if (type_scale == "Fixed" &
      type_subset %in% c("Binary") & x_scale_n1_p1 == "Y") {

    if (tolower(scale_x) == "log10") {
      scale_x_code_fixed <- scale_x_log10(
        limits = c(0.1, 10),
        expand = expansion(mult = 0.1),
        labels = scales::label_number(accuracy = 0.01),
        breaks = breaks_log()
      )
    } else {
      scale_x_code_fixed <- scale_x_continuous(
        limits = c(-0.5, 0.5),
        expand = expansion(mult = 0.1),
        breaks = seq(-0.5, 0.5, 0.25),
        labels = scales::label_number(accuracy = 0.01)
      )
    }

    fig <- fig +
      scale_x_code_fixed
  }

  if (select_nnx == "Yes") {
    if (tolower(scale_x) != "log10") {
      scale_x_code_nnx <- scale_x_continuous(
        limits = c(-0.5, 0.5),
        expand = expansion(mult = 0.1),
        breaks = seq(-0.5, 0.5, 0.25),
        sec.axis = sec_axis(
          ~ . * 1,
          name = if (fact_subset == "Benefit") {
            "NNT"
          } else {
            "NNH"
          },
          labels = function(x) {
            ifelse(x != 0, round(abs(x)**(-1), 1), "\u221E")
          },
          breaks = c(-1, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 1)
        )
      )

    }

    fig <- fig +
      scale_x_code_nnx
  }
  message(glue('[{format(Sys.time(),"%F %T")}] > Create Forest plot'))
  fig
}

#' Dot and Forest plots
#'
#' Create Dot and Forest plots and associated data
#'
#' @param data (`data.frame`) dataset
#' @param drug (`character`) selected drug
#' @param benefit (`character`) selected benefit
#' @param risk (`character`) selected risk
#' @param filters (`character`) selected filter
#' @param category (`character`) selected category
#' @param type_risk (`character`) selected way to display risk outcomes
#' (crude proportions, exposure-adjusted rates (per 100 PYs))
#' @param type_graph (`character`) selected way to display binary outcomes
#' @param select_nnx  (`character`)\cr show NNT/NNH
#' @param x_scale_fixed_free (`character`)\cr free or fixed x-axis scale
#' @param ci_method (`character`) selected method to display
#' @param space_btwn_out_yn  (`character`) control spacing between outcomes
#' confidence intervals
#' (Supplied in effects table, Calculated within the program)
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' dot_plot_src <- subset(effects_table, !is.na(Prop1))
#' bdin <- subset(dot_plot_src, Factor == "Benefit")
#' rdin <- subset(dot_plot_src, Factor == "Risk")
#'
#' create_dot_forest_plot(
#'   data = dot_plot_src,
#'   drug = unique(dot_plot_src$Trt1),
#'   benefit = unique(bdin$Outcome),
#'   risk = unique(rdin$Outcome),
#'   filters = "None",
#'   category = "All",
#'   type_graph = "Absolute risk",
#'   type_risk = "Crude proportions",
#'   select_nnx = "Y",
#'   x_scale_fixed_free = "Fixed",
#'   ci_method = "Calculated",
#'   space_btwn_out_yn = "N"
#' )

create_dot_forest_plot <- function(data,
                                   drug,
                                   benefit,
                                   risk,
                                   filters,
                                   category,
                                   type_graph,
                                   type_risk,
                                   select_nnx,
                                   x_scale_fixed_free,
                                   ci_method,
                                   space_btwn_out_yn = "Y") {

  fplot_data <- prepare_dot_forest_plot_data(
    data,
    drug,
    benefit,
    risk,
    filters,
    category,
    type_graph,
    type_risk,
    ci_method,
    space_btwn_out_yn
  )

  # Read in data to plot dot and forest plots
  dot_plot_data <- fplot_data$dot_plot_data
  forest_plot_data <- fplot_data$forest_plot_data

  col_scale <- fplot_data$col_scale
  col_scale2 <- fplot_data$col_scale2

  lft0_dot_plot_data <-
    dot_plot_data[dot_plot_data$factor == "Benefit" &
                    dot_plot_data$type == "Continuous", ]

  if (nrow(lft0_dot_plot_data) != 0) {
    lft0 <- generate_fig_lft(
      dot_plot_data,
      fact_subset = "Benefit",
      type_subset = "Continuous",
      type_scale = x_scale_fixed_free,
      xlabel = "Treatment Response\n\n ",
      select_nnx = "No"
    ) +
      guides(colour = guide_legend(title = "Treatment"))+
      ggtitle("Benefit - Continuous Outcomes") +
      col_scale
  } else {
    lft0 <- NULL
  }

  forest_plot_data$Trt2 <- data$Trt2[1]

  rgt0_forest_plot_data <-
    forest_plot_data[forest_plot_data$factor == "Benefit" &
                       forest_plot_data$type == "Continuous", ]

  if (nrow(rgt0_forest_plot_data) != 0) {
    rgt0 <- generate_fig_rft(
      forest_plot_data,
      fact_subset = "Benefit",
      type_subset = "Continuous",
      scale_x = "Identity",
      type_scale = x_scale_fixed_free,
      xlabel = "Treatment Difference with 95% CI\n",
      select_nnx = "No"
    ) +
      col_scale2
  } else {
    rgt0 <- NULL
  }

  lft1_dot_plot_data <-
    dot_plot_data[dot_plot_data$factor == "Benefit" &
                    dot_plot_data$type == "Binary", ]

  rgt1_forest_plot_data <-
    forest_plot_data[forest_plot_data$factor == "Benefit" &
                       forest_plot_data$type == "Binary", ]

  if (type_graph == "Absolute risk") {
    if (nrow(lft1_dot_plot_data) != 0) {
      lft1 <- generate_fig_lft(
        lft1_dot_plot_data,
        fact_subset = "Benefit",
        type_subset = "Binary",
        type_scale = x_scale_fixed_free,
        x_scale_n1_p1 = "Y",
        xlabel = "Treatment Response\n\n ",
        select_nnx = select_nnx
      ) +
        ggtitle("Benefit - Binary Outcomes") +
        col_scale
    } else {
      lft1 <- NULL
    }

    if (nrow(rgt1_forest_plot_data) != 0) {
      rgt1 <- generate_fig_rft(
        rgt1_forest_plot_data,
        fact_subset = "Benefit",
        type_subset = "Binary",
        xlabel = "Treatment Difference with 95% CI\n",
        scale_x = "Identity",
        type_scale = x_scale_fixed_free,
        x_scale_n1_p1 = "Y",
        select_nnx = select_nnx
      ) +
        col_scale2
    } else {
      rgt1 <- NULL
    }
  } else if (type_graph == "Relative risk") {
    if (nrow(lft1_dot_plot_data) != 0) {
      lft1 <- generate_fig_lft(
        lft1_dot_plot_data,
        fact_subset = "Benefit",
        type_subset = "Binary",
        type_scale = x_scale_fixed_free,
        x_scale_n1_p1 = "Y",
        xlabel = "Treatment Response\n\n ",
        select_nnx = "No"
      ) +
        ggtitle("Benefit - Binary Outcomes") +
        col_scale
    } else {
      lft1 <- NULL
    }

    if (nrow(rgt1_forest_plot_data) != 0) {
      rgt1 <- generate_fig_rft(
        rgt1_forest_plot_data,
        fact_subset = "Benefit",
        type_subset = "Binary",
        scale_x = "log10",
        type_scale = x_scale_fixed_free,
        xlabel = "Relative risk with 95% CI\n",
        select_nnx = "No"
      ) +
        col_scale2
    } else {
      rgt1 <- NULL
    }
  } else if (type_graph == "Odds ratio") {
    if (nrow(lft1_dot_plot_data) != 0) {
      lft1 <- generate_fig_lft(
        lft1_dot_plot_data,
        fact_subset = "Benefit",
        type_subset = "Binary",
        type_scale = x_scale_fixed_free,
        x_scale_n1_p1 = "Y",
        xlabel = "Treatment Response\n\n ",
        select_nnx = "No"
      ) +
        ggtitle("Benefit - Binary Outcomes") +
        col_scale
    } else {
      lft1 <- NULL
    }

    if (nrow(rgt1_forest_plot_data) != 0) {
      rgt1 <- generate_fig_rft(
        rgt1_forest_plot_data,
        fact_subset = "Benefit",
        type_subset = "Binary",
        scale_x = "log10",
        type_scale = x_scale_fixed_free,
        xlabel = "Odds ratio with 95% CI\n",
        select_nnx = "No"
      ) +
        col_scale2
    } else {
      rgt1 <- NULL
    }
  }

  lft2_dot_plot_data <-
    dot_plot_data[dot_plot_data$factor == "Risk" &
                    dot_plot_data$type == "Binary", ]

  rgt2_forest_plot_data <-
    forest_plot_data[forest_plot_data$factor == "Risk" &
                       forest_plot_data$type == "Binary", ]

  if (type_risk == "Crude proportions") {
    if (type_graph == "Absolute risk") {
      if (nrow(lft2_dot_plot_data) != 0) {
        lft2 <- generate_fig_lft(
          lft2_dot_plot_data,
          fact_subset = "Risk",
          type_subset = "Binary",
          type_scale = x_scale_fixed_free,
          x_scale_n1_p1 = "Y",
          xlabel = "Treatment Response\n\n ",
          select_nnx = select_nnx
        ) +
          ggtitle("Risk - Binary Outcomes") +
          col_scale
      } else {
        lft2 <- NULL
      }

      if (nrow(rgt2_forest_plot_data) != 0) {
        rgt2 <- generate_fig_rft(
          rgt2_forest_plot_data,
          fact_subset = "Risk",
          type_subset = "Binary",
          xlabel = "Treatment Difference with 95% CI\n",
          scale_x = "Identity",
          type_scale = x_scale_fixed_free,
          x_scale_n1_p1 = "Y",
          select_nnx = select_nnx
        ) +
          col_scale2
      } else {
        rgt2 <- NULL
      }
    } else if (type_graph == "Relative risk") {
      if (nrow(lft2_dot_plot_data) != 0) {
        lft2 <- generate_fig_lft(
          lft2_dot_plot_data,
          fact_subset = "Risk",
          type_subset = "Binary",
          type_scale = x_scale_fixed_free,
          x_scale_n1_p1 = "Y",
          xlabel = "Treatment Response\n\n ",
          select_nnx = "No"
        ) +
          ggtitle("Risk - Binary Outcomes") +
          col_scale
      } else {
        lft2 <- NULL
      }

      if (nrow(rgt2_forest_plot_data) != 0) {
        rgt2 <- generate_fig_rft(
          rgt2_forest_plot_data,
          fact_subset = "Risk",
          type_subset = "Binary",
          type_scale = x_scale_fixed_free,
          scale_x = "log10",
          xlabel = "Relative risk with 95% CI\n",
          select_nnx = "No"
        ) +
          col_scale2
      } else {
        rgt2 <- NULL
      }

    } else if (type_graph == "Odds ratio") {
      if (nrow(lft2_dot_plot_data) != 0) {
        lft2 <- generate_fig_lft(
          lft2_dot_plot_data,
          fact_subset = "Risk",
          type_subset = "Binary",
          type_scale = x_scale_fixed_free,
          x_scale_n1_p1 = "Y",
          xlabel = "Treatment Response\n\n ",
          select_nnx = "No"
        ) +
          ggtitle("Risk - Binary Outcomes") +
          col_scale
      } else {
        lft2 <- NULL
      }

      if (nrow(rgt2_forest_plot_data) != 0) {
        rgt2 <- generate_fig_rft(
          rgt2_forest_plot_data,
          fact_subset = "Risk",
          type_subset = "Binary",
          scale_x = "log10",
          type_scale = x_scale_fixed_free,
          x_scale_n1_p1 = "N",
          xlabel = "Odds ratio with 95% CI\n",
          select_nnx = "No"
        ) +
          col_scale2
      } else {
        rgt2 <- NULL
      }
    }
  } else if (type_risk == "Exposure-adjusted rates (per 100 PYs)") {
    if (nrow(lft2_dot_plot_data) != 0) {
      lft2 <- generate_fig_lft(
        lft2_dot_plot_data,
        fact_subset = "Risk",
        type_subset = "Binary",
        type_scale = x_scale_fixed_free,
        xlabel = "Treatment Response\n\n ",
        select_nnx = "No"
      ) +
        ggtitle("Risk - Exposure-adjusted Rates per 100 PYs") +
        col_scale
    } else {
      lft2 <- NULL
    }

    if (nrow(rgt2_forest_plot_data) != 0) {
      rgt2 <- generate_fig_rft(
        rgt2_forest_plot_data,
        fact_subset = "Risk",
        type_subset = "Binary",
        scale_x = "Identity",
        type_scale = x_scale_fixed_free,
        xlabel = "Treatment Difference with 95% CI\n",
        select_nnx = "No"
      ) +
        col_scale2
    } else {
      rgt2 <- NULL
    }
  }

  if (nrow(lft2_dot_plot_data) != 0 & nrow(rgt2_forest_plot_data) != 0) {
    lft0 <- lft0 +
      theme(legend.position = "none")
    rgt0 <- rgt0 +
      theme(legend.position = "none")
    lft1 <- lft1 +
      theme(legend.position = "none")
    rgt1 <- rgt1 +
      theme(legend.position = "none")
  }

  if (nrow(lft1_dot_plot_data) != 0 & nrow(rgt1_forest_plot_data) != 0) {
    lft0 <- lft0 +
      theme(legend.position = "none")
    rgt0 <- rgt0 +
      theme(legend.position = "none")
  }

  message(paste0(
    glue("[{format(Sys.time(),'%F %T')}] > "),
    glue("Create Dot and Forest plots and associated data")
  ))
  return(
    list(
      myplot_lft0 = lft0,
      myplot_rgt0 = rgt0,
      myplot_lft1 = lft1,
      myplot_rgt1 = rgt1,
      myplot_lft2 = lft2,
      myplot_rgt2 = rgt2,
      myplotdata1 = dot_plot_data,
      myplotdata2 = forest_plot_data
    )
  )
}

