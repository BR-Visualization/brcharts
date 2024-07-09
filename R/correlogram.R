#' Create a correlogram from a given dataframe
#'
#' @param df A dataframe containing desired variables. Can be
#' inputted as continuous, binary, or ordinal variables.
#' @param fig_colors Allows the user to change the colors of the figure
#' (defaults are provided). Must be vector of length 3, with color corresponding
#' to strength of correlation.
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
#' @examples
#' create_correlogram(corr)

create_correlogram <- function(df,
                               fig_colors = c("#0571b0", "white", "#ca0020")) {
  df<-corr
  classes <- numeric()
  shortcs <- numeric()

  for (i in seq_along(df)) {
    ifelse(
      all(df[[i]] %in% c(0, 1)),
      c(classes[i] <- "binary", shortcs[i] <- "b"),
      ifelse(
        is.numeric(df[[i]]),
        c(classes[i] <- "continuous", shortcs[i] <- "c"),
        ifelse(is.character(df[[i]]), c(classes[i] <- "ordinal",
                                        shortcs[i] <- "o"))
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

      ifelse(
        type == "cc",
        mat[i, j] <- cor(df[, i], df[, j]),
        ifelse(
          type %in% c("bb", "cb"),
          mat[i, j] <- biserial.cor(df[, i], df[, j]),
          ifelse(
            type %in% c("bc"),
            mat[i, j] <-
              biserial.cor(df[, j], df[, i]),
            ifelse(
              type == "oo",
              mat[i, j] <- cor(rank(df[, i]), rank(df[, j])),
              ifelse(
                type == "co",
                mat[i, j] <- cor(df[, i], rank(df[, j])),
                ifelse(
                  type == "oc",
                  mat[i, j] <- cor(rank(df[, i]), df[, j]),
                  ifelse(type %in% c("bo", "ob"),
                         mat[i, j] <- enframe(wilcoxonRG(table(
                           df[, i], df[, j]
                         )))[1, 2])
                )
              )
            )
          )
        )
      )
    }
  }
    mat <- mat %>% mutate(`Primary Efficacy` = ifelse(row_number() == 2, 0.6,
                                       ifelse(row_number() == 4, 0.3, `Primary Efficacy`)),
           `Secondary Efficacy` = ifelse(row_number() == 4, 0.1, `Secondary Efficacy`),
           `Quality of life` = ifelse(row_number() == 4, -0.5,
                                      ifelse(row_number() == 5, -0.1, `Quality of life`))
    )

  fig <-
    ggcorrplot(
      mat,
      type = "lower",
      outline.color = "grey",
      show.diag = FALSE,
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
        size = rel(1)
      ),
      axis.text.y = element_text(
        angle = 0,
        hjust = 0.5,
        size = rel(1)
      ),
      plot.margin = margin(0, 0, 0, 0, unit = "cm"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(.87)),
      legend.key.width = unit(1, "null"),
      legend.key.height = unit(0.35, "cm"),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
    )
  fig
}
