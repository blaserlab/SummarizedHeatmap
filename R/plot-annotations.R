#' Plot row annotations
#'
#' Each selected row annotation is represented by an independent ggplot. A
#' single variable returns a ggplot; multiple variables return a patchwork of
#' those plots.
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
#' Each selected column annotation is represented by an independent ggplot. A
#' single variable returns a ggplot; multiple variables return a patchwork.
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
