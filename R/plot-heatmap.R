#' Compose a complete heatmap with patchwork
#'
#' The component plots remain available through their individual plotting
#' functions. This helper arranges them around the main heatmap panel and
#' collects guides in grid traversal order: column dendrogram, column
#' annotations, row dendrogram, row annotations, then the main heatmap.
#' Column annotation plots are stacked vertically. The row dendrogram and row
#' annotation panels move to whichever side of the heatmap `rowDendroSide`/
#' `rowAnnotationSide` request (`"left"` or `"right"`); the column dendrogram
#' and column annotation panels likewise move with `colDendroSide`/
#' `colAnnotationSide` (`"top"` or `"bottom"`). When a dendrogram and its
#' matching annotation share a side, the annotation sits adjacent to the
#' heatmap and the dendrogram sits further out. Pass named variable vectors
#' to `rowVars` or `colVars` to control annotation strip labels.
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
#' @param rowDendroSide `"left"` or `"right"`: which side of the heatmap the
#'   row dendrogram panel is placed on (and oriented toward).
#' @param colDendroSide `"top"` or `"bottom"`: which side of the heatmap the
#'   column dendrogram panel is placed on (and oriented toward).
#' @param rowAnnotationSide,colAnnotationSide Placement sides passed to the
#'   annotation component plotters, restricted to `"left"`/`"right"` (rows)
#'   or `"top"`/`"bottom"` (columns) so the panel is placed on that side of
#'   the heatmap. `"left"` (rows) and `"bottom"` (columns) place the
#'   annotation's variable-name label where it can be clipped if the figure
#'   is too small; see Details.
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
    # Row dendrogram/annotation panels sit beside the heatmap body, so they
    # may only move between its left and right; column dendrogram/annotation
    # panels sit above/below it, so they may only move between top and
    # bottom (see `.heatmap_design()`).
    rowDendroSide <- match.arg(rowDendroSide, c("left", "right"))
    rowAnnotationSide <- match.arg(rowAnnotationSide, c("left", "right"))
    colDendroSide <- match.arg(colDendroSide, c("top", "bottom"))
    colAnnotationSide <- match.arg(colAnnotationSide, c("top", "bottom"))
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
    layout <- .heatmap_design(rowDendroSide, rowAnnotationSide, colDendroSide, colAnnotationSide, guideWidth)
    panels <- list(A = col_dendro, B = col_data, C = row_dendro, D = row_data, E = main, M = guide)
    patchwork::wrap_plots(panels, design = layout$design) +
        patchwork::plot_layout(
            widths = layout$widths, heights = layout$heights,
            guides = if (collectGuides) "collect" else "keep"
        )
}

# Build the patchwork grid `design` (plus matching `widths`/`heights`) that
# places each component panel on its requested side of the main heatmap
# panel ("E"). Row-axis components (row dendrogram "C", row annotation "D")
# are ordered left-to-right; column-axis components (column dendrogram "A",
# column annotation "B") are ordered top-to-bottom. When two components share
# a side, the annotation sits adjacent to the main panel and the dendrogram
# sits further out, matching the package's default layout. The guide column
# ("M") is always last and only occupies the main panel's row.
.heatmap_design <- function(rowDendroSide, rowAnnotationSide, colDendroSide, colAnnotationSide, guideWidth) {
    row_left <- c(
        if (rowDendroSide == "left") "dendro",
        if (rowAnnotationSide == "left") "annotation"
    )
    row_right <- c(
        if (rowAnnotationSide == "right") "annotation",
        if (rowDendroSide == "right") "dendro"
    )
    horiz_order <- c(row_left, "main", row_right)

    col_top <- c(
        if (colDendroSide == "top") "dendro",
        if (colAnnotationSide == "top") "annotation"
    )
    col_bottom <- c(
        if (colAnnotationSide == "bottom") "annotation",
        if (colDendroSide == "bottom") "dendro"
    )
    vert_order <- c(col_top, "main", col_bottom)

    row_letters <- c(dendro = "C", annotation = "D")
    col_letters <- c(dendro = "A", annotation = "B")
    row_widths <- c(dendro = 1, annotation = 1, main = 8)
    col_heights <- c(dendro = 1, annotation = 1, main = 8)

    design_rows <- vapply(vert_order, function(vk) {
        cells <- vapply(horiz_order, function(hk) {
            if (vk == "main" && hk == "main") "E"
            else if (vk == "main") row_letters[[hk]]
            else if (hk == "main") col_letters[[vk]]
            else "#"
        }, character(1))
        paste0(paste(cells, collapse = ""), if (vk == "main") "M" else "#")
    }, character(1))

    list(
        design = paste(design_rows, collapse = "\n"),
        widths = c(unname(row_widths[horiz_order]), guideWidth),
        heights = unname(col_heights[vert_order])
    )
}
