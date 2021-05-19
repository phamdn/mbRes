#' Visualize Biomarker Ordinal Estimator
#'
#' \code{heat} plots a heatmap of the perceived differences from Biomarker
#' Ordinal Estimator (BOE).
#'
#' @param dat a data frame with at least three columns.
#' @param hax a character, name of the column to be used as the horizontal axis.
#' @param vax a character, name of the column to be used as the vertical axis.
#' @param cell a character, name of the column to be used as the cells.
#'
#' @return \code{heat} returns a ggplot object.
#'
#' @export
heat <- function(dat, hax, vax, cell) {
  ggplot(dat,
         aes(
           x = !!sym(hax),
           y = !!sym(vax),
           fill = !!sym(cell)
         )) +
    geom_tile(color = "white",
              size = 0.25) +
    scale_x_discrete(name = NULL,
                     expand = c(0, 0),
                     position = "top") +
    scale_y_discrete(limits = rev, name = NULL) +
    scale_fill_manual(
      drop = F,
      values = c(
        "No" = "#0072B2",
        "Very small" = "#009E73",
        "Small" = "#F0E442",
        "Medium" = "#E69F00",
        "Large" = "#D55E00"
      )
    ) +
    labs(title = cell) +
    theme_cowplot() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(1, "pt"),
      legend.position = "top",
      legend.justification = "left",
      legend.title.align = 0.5,
      legend.title = element_text(size = 12, face = "bold")
    )
}
