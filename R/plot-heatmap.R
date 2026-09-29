#' Compose a complete heatmap with patchwork
#'
#' The component plots remain available through their individual plotting
#' functions. This helper arranges them in a fixed layout and collects guides
#' in grid traversal order: column dendrogram, column annotations, row
#' dendrogram, row annotations, then the main heatmap. Column annotation plots
#' are stacked vertically; the row dendrogram and row annotations sit to the
#' left of the heatmap in that order. Pass named variable
#' vectors to `rowVars` or `colVars` to control annotation strip labels.
#'
#' Each annotation strip's variable-name axis text is wrapped with
#' `patchwork::free(type = "space")` so it packs tightly against the heatmap
#' instead of forcing extra column/row width (see `plotColData()`/
#' `plotRowData()` for the axis text itself). Because `"space"` reserves no
#' room for that text, it can get clipped at the edge of the plotting device
#' if there isn't enough surrounding space to draw it -- most likely with the
#' non-default `rowAnnotationSide = "left"` or `colAnnotationSide = "bottom"`,
#' where the (possibly rotated) label competes for space with the heatmap's
#' own row/column identifier labels. Increase the figure's height or width if
#' labels are clipped in that configuration.
#'
#' @param x A SummarizedHeatmap object.
#' @param rowVars,colVars Optional annotation variables to display. `NULL`
#'   includes all available variables.
#' @param showRowDendro,showColDendro Whether to display stored dendrograms.
#' @param rowDendroSide,colDendroSide Orientation sides passed to the
#'   dendrogram component plotters.
#' @param rowAnnotationSide,colAnnotationSide Placement sides passed to the
#'   annotation component plotters. `"left"` (rows) and `"bottom"` (columns)
#'   place the annotation's variable-name label where it can be clipped if
#'   the figure is too small; see Details.
#' @param collectGuides Collect guides into a shared guide area.
#' @param guideWidth Relative width of the guide area column, on the same
#'   scale as the dendrogram/annotation (`1`) and heatmap body (`8`) column
#'   widths. Increase this when stacked legends (e.g. several annotation
#'   variables) overflow their column and collide with the heatmap's row
#'   labels or with each other.
#' @param ... Additional arguments passed to `plotHeatmapMain()`.
#' @return A patchwork object.
#' @examples
#' mat <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
#' x <- SummarizedHeatmap(mat)
#' plotHeatmap(x)
#' @export
plotHeatmap <- function(x, rowVars = NULL, colVars = NULL,
                        showRowDendro = TRUE, showColDendro = TRUE,
                        rowDendroSide = "left", colDendroSide = "top",
                        rowAnnotationSide = "right", colAnnotationSide = "top",
                        collectGuides = TRUE, guideWidth = 3, ...) {
    .checkSummarizedHeatmap(x)
    if (!is.numeric(guideWidth) || length(guideWidth) != 1L || is.na(guideWidth) || guideWidth <= 0) {
        stop("'guideWidth' must be a single positive number", call. = FALSE)
    }
    colAnnotationSide <- match.arg(colAnnotationSide, c("top", "bottom", "right", "left"))
    rowAnnotationSide <- match.arg(rowAnnotationSide, c("right", "left", "top", "bottom"))
    spacer <- patchwork::plot_spacer()
    col_dendro <- if (showColDendro && !is.null(colDendro(x))) plotColDendro(x, side = colDendroSide) else spacer
    # `free(type = "space")` lets each annotation strip's variable-name axis
    # text occupy space without reserving room for it in the shared grid, so
    # annotations with different label lengths still pack tightly against
    # the heatmap instead of forcing uneven column/row widths.
    col_data <- if (length(colVars) || (is.null(colVars) && ncol(SummarizedExperiment::colData(x)))) {
        patchwork::free(plotColData(x, vars = colVars, side = colAnnotationSide), type = "space")
    } else {
        spacer
    }
    row_data <- if (length(rowVars) || (is.null(rowVars) && ncol(SummarizedExperiment::rowData(x)))) {
        patchwork::free(plotRowData(x, vars = rowVars, side = rowAnnotationSide), type = "space")
    } else {
        spacer
    }
    row_dendro <- if (showRowDendro && !is.null(rowDendro(x))) plotRowDendro(x, side = rowDendroSide) else spacer
    main <- plotHeatmapMain(x, ...)
    guide <- patchwork::guide_area()
    panels <- list(col_dendro, col_data, row_dendro, row_data, main, guide)
    design <- "##A#\n##B#\nCDEM"
    patchwork::wrap_plots(panels, design = design) +
        patchwork::plot_layout(
            widths = c(1, 1, 8, guideWidth), heights = c(1, 1, 8),
            guides = if (collectGuides) "collect" else "keep"
        )
}
