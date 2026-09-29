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
#' @param x A SummarizedHeatmap object.
#' @param rowVars,colVars Optional annotation variables to display. `NULL`
#'   includes all available variables.
#' @param showRowDendro,showColDendro Whether to display stored dendrograms.
#' @param rowDendroSide,colDendroSide Orientation sides passed to the
#'   dendrogram component plotters.
#' @param rowAnnotationSide,colAnnotationSide Placement sides passed to the
#'   annotation component plotters.
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
    # Build annotation strips with `.annotation_plots(..., wrap = FALSE)`
    # rather than the exported `plotColData()`/`plotRowData()`. Those wrap
    # multi-variable results with `wrap_elements()` so they compose safely
    # with patchwork operators, but a wrapped composite can no longer be
    # aligned to the axis positions of sibling panels. Composing the raw,
    # unwrapped composite through this function's list + `design` layout is
    # already safe from the same flattening operators would cause, so the
    # unwrapped form keeps annotation strips pixel-aligned with the heatmap.
    col_data <- if (length(colVars) || (is.null(colVars) && ncol(SummarizedExperiment::colData(x)))) {
        .annotation_plots(x, 2L, colVars, colAnnotationSide, tileColor = "white", palette = NULL, showNames = FALSE, wrap = FALSE)
    } else {
        spacer
    }
    row_data <- if (length(rowVars) || (is.null(rowVars) && ncol(SummarizedExperiment::rowData(x)))) {
        .annotation_plots(x, 1L, rowVars, rowAnnotationSide, tileColor = "white", palette = NULL, showNames = FALSE, wrap = FALSE)
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
