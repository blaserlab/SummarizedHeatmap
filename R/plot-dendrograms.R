#' Plot the stored row dendrogram
#'
#' @param x A SummarizedHeatmap object.
#' @param side Side on which the dendrogram is oriented. The default places
#'   leaves toward the heatmap when shown to its left.
#' @param linewidth Dendrogram line width.
#' @return A ggplot object.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' plotRowDendro(SummarizedHeatmap(mat))
#' @export
plotRowDendro <- function(x, side = "left", linewidth = 0.5) {
    .checkSummarizedHeatmap(x)
    side <- match.arg(side, c("left", "right", "top", "bottom"))
    dendro <- rowDendro(x)
    if (is.null(dendro)) stop("no row dendrogram is stored", call. = FALSE)
    .plot_dendrogram(dendro, side, linewidth)
}

#' Plot the stored column dendrogram
#'
#' @inheritParams plotRowDendro
#' @return A ggplot object.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' plotColDendro(SummarizedHeatmap(mat))
#' @export
plotColDendro <- function(x, side = "top", linewidth = 0.5) {
    .checkSummarizedHeatmap(x)
    side <- match.arg(side, c("top", "bottom", "left", "right"))
    dendro <- colDendro(x)
    if (is.null(dendro)) stop("no column dendrogram is stored", call. = FALSE)
    .plot_dendrogram(dendro, side, linewidth)
}

.plot_dendrogram <- function(dendro, side, linewidth) {
    segments <- ggdendro::segment(ggdendro::dendro_data(dendro, type = "rectangle"))
    if (side %in% c("top", "bottom")) {
        p <- ggplot2::ggplot(segments) +
            ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend), linewidth = linewidth)
        if (side == "bottom") p <- p + ggplot2::scale_y_reverse()
    } else {
        p <- ggplot2::ggplot(segments) +
            ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend), linewidth = linewidth) +
            ggplot2::coord_flip()
        if (side == "left") p <- p + ggplot2::scale_y_reverse()
    }
    p + ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}
