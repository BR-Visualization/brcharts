#' Create a correlogram from a given dataframe
#'
#' @param df A dataframe containing desired variables. Can be
#' inputted as continuous, binary, or ordinal variables.
#' Note: Binary variables must have a value of 0 or 1.
#' Note: Ordinal variables must be formatted as factors.
#' @param fig_colors Allows the user to change the colors of the figure
#' (defaults are provided). Must be vector of length 3, with color corresponding
#' to strength of correlation.
#' @param diagonal Allows user to choose to view the correlogram with diagonal
#' entries. Default is FALSE.
#' @param type_c Allows user to revise the display. Default is "lower".
#' @param method Allows user to modify the visualization method of the
#' correlogram. Default is "square".
#'
#' @return A correlogram.
#' @export
#' @import ltm
#' @import tibble
#' @import rcompanion
#' @import cowplot
#' @import ggcorrplot
#' @import stringr
#'
#' @details
#' Different correlation coefficients are calculated based on the nature of the
#' variables:
#' For two continuous variables, the Pearson correlation coefficient is used.
#' For two binary variables, the Phi correlation coefficient is implemented.
#' For one binary and one continuous variable, point biserial correlation is
#' utilized.
#' For two ordinal variables, Spearman rank correlation is utilized.
#' For one continuous and one ordinal variable, a modified Pearson correlation
#' combined with the nonparametric Spearman rank correlation is used.
#' For one binary and one ordinal variable, Glass rank biserial correlation
#' is implemented.
#'
#' @examples
#' create_correlogram(corr)
create_correlogram <- function(df,
                               diagonal = FALSE,
                               method = "square",
                               type_c = "lower",
                               fig_colors = colfun()$fig10_colors) {
  classes <- numeric()
  shortcs <- numeric()

  df <- as.data.frame(df)

  if (ncol(df) <= 1) {
    error_message <- "You must have more than one variable in your
                              dataframe."
    stop(error_message)
  }

  if (any(is.na(df))) {
    miss_vars <- colnames(df)[colSums(is.na(df) > 0)]
    warning(paste(
      "you have a missing value in row(s)",
      which(rowSums(is.na(df)) > 0), "and column(s)",
      which(colSums(is.na(df)) > 0)
    ))
    df[miss_vars] <- lapply(df[miss_vars], function(x) {
      ifelse(is.na(x), NA, x)
    })
  }

  for (i in seq_along(df)) {
    column_dat <- df[[i]][!is.na(df[[i]])]
    ifelse(
      all(column_dat %in% c(0, 1)),
      c(classes[i] <- "binary", shortcs[i] <- "b"),
      ifelse(
        all(is.numeric(column_dat)),
        c(classes[i] <- "continuous", shortcs[i] <- "c"),
        ifelse(all(is.factor(column_dat)), c(
          classes[i] <- "ordinal",
          shortcs[i] <- "o"
        ),
        stop("Please review your dataframe inputs to ensure correct
                formatting.")
        )
      )
    )
  }

  df_attribs <- data.frame(
    names = c(colnames(df)),
    category = c(classes),
    shortc = c(shortcs)
  )

  mat <- data.frame(matrix(NA, nrow = ncol(df), ncol = ncol(df)))
  dimnames(mat) <- list(names(df), names(df))

  for (i in seq(1, ncol(df))) {
    for (j in seq(1, ncol(df))) {
      xattr <-
        df_attribs[df_attribs$names %in% names(df)[i], ][["shortc"]]
      yattr <-
        df_attribs[df_attribs$names %in% names(df)[j], ][["shortc"]]

      type <- paste0(xattr, yattr)
      # correlation calculations
      ifelse(
        type == "cc",
        # calculates Pearson correlation with two continuous variables
        mat[i, j] <- cor(df[, i], df[, j]),
        ifelse(
          type == "bb",
          # calculates Phi correlation coefficient between two binary variables.
          mat[i, j] <- rcompanion::phi(df[, i], df[, j]),
          ifelse(
            type == "cb",
            # calculates point biserial correlation with a
            # continuous variable as the x attribute followed by a binary variable
            # as the y attribute.
            mat[i, j] <- biserial.cor(df[, i], df[, j]),
            ifelse(
              type %in% c("bc"),
              # calculates point biserial correlation with a binary variable as
              # the x attribute followed by a continuous variable as the y
              # attribute.
              mat[i, j] <-
                biserial.cor(df[, j], df[, i]),
              ifelse(
                type == "oo",
                # calculates Spearman rank correlation with two ordinal variables.
                mat[i, j] <- cor(rank(df[, i]), rank(df[, j])),
                ifelse(
                  type == "co",
                  # calculates modified Pearson correlation with nonparametric
                  # Spearman rank correlation, considering a continuous variable
                  # as the x attribute and ordinal variable as the y attribute.
                  mat[i, j] <- cor(df[, i], rank(df[, j])),
                  ifelse(
                    type == "oc",
                    # calculates modified Pearson correlation with nonparametric
                    # Spearman rank correlation, considering an ordinal variable
                    # as the x attribute and continuous variable as the y
                    # attribute.
                    mat[i, j] <- cor(rank(df[, i]), df[, j]),
                    ifelse(type == "ob",
                      # calculates glass rank biserial correlation with an ordinal
                      # variable as the x attribute and binary variable as the y
                      # attribute.
                      mat[i, j] <- enframe(wilcoxonRG(table(
                        df[, j], df[, i]
                      )))[1, 2],
                      ifelse(type == "bo",
                        # calculates glass rank biserial correlation with a binary
                        # variable as the x attribute and an ordinal variable as
                        # the y attribute.
                        mat[i, j] <- enframe(wilcoxonRG(table(
                          df[, i], df[, j]
                        )))[1, 2]
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  fig <-
    ggcorrplot(
      mat,
      type = type_c,
      outline.color = "grey",
      show.diag = diagonal,
      method = method,
      colors = fig_colors,
      ggtheme = br_charts_theme(),
      tl.cex = 9
    )

  build1 <- ggplot_build(fig)
  labels1 <- build1$layout$panel_params[[1]]$x$get_labels()
  labels2 <- build1$layout$panel_params[[1]]$y$get_labels()

  fig <- fig + scale_x_discrete(
    labels = str_wrap(labels1, width = 7)
  ) +
    scale_y_discrete(
      labels = str_wrap(labels2, width = 7)
    ) +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        size = rel(1.2),
        color = "black"
      ),
      axis.text.y = element_text(
        angle = 0,
        hjust = 0.5,
        size = rel(1.2),
        color = "black"
      ),
      plot.margin = margin(0, 0, 0, 0, unit = "cm"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(
        size = rel(1.2),
        margin = margin(t = 7), color = "black"
      ),
      legend.key.width = unit(1, "null"),
      legend.key.height = unit(0.35, "cm"),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
    )
  fig
}
