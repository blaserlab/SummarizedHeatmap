#' Plot row annotations
#'
#' Each selected row annotation is represented by an independent ggplot,
#' labelled with its variable name as axis text next to the annotation tiles
#' (in addition to its legend title). A single variable returns a ggplot;
#' multiple variables return an (unwrapped) patchwork stacking the
#' independent plots, so the result stays aligned with sibling panels and
#' its guides can still be collected by an enclosing `plot_layout(guides =
#' "collect")` -- see `plotHeatmap()`.
#'
#' Combining a multiple-variable result directly with another plot using
#' patchwork operators (`+`, `/`, `|`) can silently flatten its panel list
#' into the parent's, which may break a `plot_layout(heights = ...)`/`
#' plot_layout(design = ...)` built around an expected panel count. Use
#' `patchwork::wrap_plots(list(annotation, otherPlot), ...)` instead of an
#' operator to combine them safely (this is how `plotHeatmap()` does it
#' internally).
#'
#' @param x A SummarizedHeatmap object.
#' @param vars Character vector of `rowData` names. A named vector uses its
#'   names as displayed labels.
#' @param side Annotation placement: `"right"`, `"left"`, `"top"`, or `"bottom"`.
#' @param tileColor Tile border colour.
#' @param palette Optional named values passed to `scale_fill_manual()`.
#' @param showNames Whether to display row or column names on the annotation axis.
#' @return A ggplot for one variable, a patchwork for multiple variables, or a
#'   spacer when no row annotations are present.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' rd <- S4Vectors::DataFrame(kind = rep(c("gene", "control"), 2), row.names = letters[1:4])
#' plotRowData(SummarizedHeatmap(mat, rowData = rd))
#' @export
plotRowData <- function(x, vars = NULL, side = "right", tileColor = "white", palette = NULL,
                        showNames = FALSE) {
    .checkSummarizedHeatmap(x)
    side <- match.arg(side, c("right", "left", "top", "bottom"))
    .annotation_plots(x, 1L, vars, side, tileColor, palette, showNames)
}

#' Plot column annotations
#'
#' Each selected column annotation is represented by an independent ggplot,
#' labelled with its variable name as axis text next to the annotation
#' tiles. A single variable returns a ggplot; multiple variables return an
#' (unwrapped) patchwork stacking the independent plots (see
#' [plotRowData()] for alignment and safe-composition details).
#'
#' @inheritParams plotRowData
#' @return A ggplot for one variable, a patchwork for multiple variables, or a
#'   spacer when no column annotations are present.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' cd <- S4Vectors::DataFrame(group = c("A", "B", "A"), row.names = LETTERS[1:3])
#' plotColData(SummarizedHeatmap(mat, colData = cd))
#' @export
plotColData <- function(x, vars = NULL, side = "top", tileColor = "white", palette = NULL,
                        showNames = FALSE) {
    .checkSummarizedHeatmap(x)
    side <- match.arg(side, c("top", "bottom", "right", "left"))
    .annotation_plots(x, 2L, vars, side, tileColor, palette, showNames)
}
