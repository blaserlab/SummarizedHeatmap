#' Plot the assay as a ggplot heatmap
#'
#' The returned object is an ordinary ggplot and can be modified with standard
#' ggplot2 layers and scales. The display follows the stored row and column
#' orders.
#'
#' @param x A SummarizedHeatmap object.
#' @param tileColor Tile border colour.
#' @param high,mid,low Colours for high, midpoint, and low assay values.
#' @param flip If `TRUE`, transpose the visual orientation.
#' @return A ggplot object.
#' @examples
#' mat <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
#' plotHeatmapMain(SummarizedHeatmap(mat))
#' @export
plotHeatmapMain <- function(x, tileColor = "white", high = "red3",
                            mid = "white", low = "blue4", flip = FALSE) {
    .checkSummarizedHeatmap(x)
    row_ids <- .axis_ids(x, 1L)[rowOrder(x)]
    col_ids <- .axis_ids(x, 2L)[colOrder(x)]
    values <- SummarizedExperiment::assay(x)[rowOrder(x), colOrder(x), drop = FALSE]
    data <- data.frame(
        row = factor(rep(row_ids, times = length(col_ids)), levels = row_ids),
        column = factor(rep(col_ids, each = length(row_ids)), levels = col_ids),
        value = as.vector(values)
    )
    if (!flip) {
        ggplot2::ggplot(data, ggplot2::aes(x = column, y = row, fill = value)) +
            ggplot2::geom_tile(colour = tileColor) +
            ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high) +
            ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0)) +
            ggplot2::scale_y_discrete(position = "right", expand = ggplot2::expansion(add = 0)) +
            ggplot2::labs(x = NULL, y = NULL, fill = "Expression") +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.text = ggplot2::element_text(colour = "black"),
                panel.grid = ggplot2::element_blank(),
                panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
                plot.margin = ggplot2::margin(2, 2, 2, 2)
            )
    } else {
        ggplot2::ggplot(data, ggplot2::aes(y = column, x = row, fill = value)) +
            ggplot2::geom_tile(colour = tileColor) +
            ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high) +
            ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0)) +
            ggplot2::scale_y_discrete(position = "right", expand = ggplot2::expansion(add = 0)) +
            ggplot2::labs(x = NULL, y = NULL, fill = "Expression") +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.text = ggplot2::element_text(colour = "black"),
                panel.grid = ggplot2::element_blank(),
                panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
                plot.margin = ggplot2::margin(2, 2, 2, 2)
            )
    }
}
