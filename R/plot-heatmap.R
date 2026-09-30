#' Compose a complete heatmap with patchwork
#'
#' The component plots remain available through their individual plotting
#' functions. This helper arranges them around the main heatmap panel and
#' collects guides in grid traversal order: column dendrogram, column
#' annotations, row dendrogram, row annotations, then the main heatmap.
#' Column annotation plots are stacked vertically. The row dendrogram and row
#' annotation panels move to whichever side of the heatmap `rowDendroSide`/
#' `rowAnnotationSide` request (`"left"` or `"right"`, both defaulting to
#' `"left"`); the column dendrogram and column annotation panels likewise
#' move with `colDendroSide`/`colAnnotationSide` (`"top"` or `"bottom"`, both
#' defaulting to `"top"`). When a dendrogram and its matching annotation
#' share a side, the annotation sits adjacent to the heatmap and the
#' dendrogram sits further out; by default this puts both on the same side
#' for each axis. Pass named variable vectors to `rowVars` or `colVars` to
#' control annotation strip labels; pass `NULL` to `rowAnnotationSide`/
#' `colAnnotationSide` to omit that annotation panel entirely. Any omitted
#' panel -- an annotation set to `NULL`, a dendrogram hidden with
#' `showRowDendro`/`showColDendro = FALSE`, or a dendrogram that was never
#' stored -- is dropped from the grid rather than left as a blank but
#' still-sized slot, so the remaining panels use the reclaimed space.
#'
#' Each annotation strip's variable-name axis text is wrapped with
#' `patchwork::free(type = "space")` so it packs tightly against the heatmap
#' instead of forcing extra column/row width (see `plotColData()`/
#' `plotRowData()` for the axis text itself). Because `"space"` reserves no
#' room for that text, it can get clipped at the edge of the plotting device
#' if there isn't enough surrounding space to draw it -- most likely with the
#' non-default `rowAnnotationSide = "right"` or `colAnnotationSide =
#' "bottom"`, where the (possibly rotated) label competes for space with the
#' heatmap's own row/column identifier labels. Increase the figure's height
#' or width if labels are clipped in that configuration.
#'
#' @param x A SummarizedHeatmap object.
#' @param rowVars,colVars Optional annotation variables to display. `NULL`
#'   includes all available variables.
#' @param showRowDendro,showColDendro Whether to display stored dendrograms.
#' @param rowDendroSide `"left"` or `"right"`: which side of the heatmap the
#'   row dendrogram panel is placed on (and oriented toward).
#' @param colDendroSide `"top"` or `"bottom"`: which side of the heatmap the
#'   column dendrogram panel is placed on (and oriented toward).
#' @param rowAnnotationSide,colAnnotationSide Which side of the heatmap the
#'   annotation panel is placed on, restricted to `"left"`/`"right"` (rows)
#'   or `"top"`/`"bottom"` (columns). `"right"` (rows) and `"bottom"`
#'   (columns) place the annotation's variable-name label where it can be
#'   clipped if the figure is too small; see Details. Set to `NULL` to omit
#'   the annotation panel entirely.
#' @param collectGuides Collect guides into a shared guide area.
#' @param guideWidth Relative width of the guide area column, on the same
#'   scale as the dendrogram/annotation (`1`) and heatmap body (`8`) column
#'   widths. Increase this when stacked legends (e.g. several annotation
#'   variables) overflow their column and collide with the heatmap's row
#'   labels or with each other.
#' @param ... Additional arguments passed to `plotHeatmapMain()`, such as
#'   `low`/`mid`/`high`/`midpoint` (fill scale) or `fillTitle` (legend
#'   title). `flip` is not accepted here -- flipping only the main panel
#'   would misalign it against the annotation and dendrogram panels arranged
#'   around it; call `plotHeatmapMain()` directly for a flipped main panel on
#'   its own.
#' @return A patchwork object.
#' @examples
#' mat <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
#' x <- SummarizedHeatmap(mat)
#' plotHeatmap(x)
#' @export
plotHeatmap <- function(x, rowVars = NULL, colVars = NULL,
                        showRowDendro = TRUE, showColDendro = TRUE,
                        rowDendroSide = "left", colDendroSide = "top",
                        rowAnnotationSide = "left", colAnnotationSide = "top",
                        collectGuides = TRUE, guideWidth = 3, ...) {
    .checkSummarizedHeatmap(x)
    if (!is.numeric(guideWidth) || length(guideWidth) != 1L || is.na(guideWidth) || guideWidth <= 0) {
        stop("'guideWidth' must be a single positive number", call. = FALSE)
    }
    dots <- list(...)
    if ("flip" %in% names(dots)) {
        stop(
            "'flip' is only supported by plotHeatmapMain(), not plotHeatmap(). ",
            "Flipping just the main panel would misalign it against the ",
            "annotation and dendrogram panels built around it; call ",
            "plotHeatmapMain() directly if you want a flipped main panel on its own.",
            call. = FALSE
        )
    }
    # Row dendrogram/annotation panels sit beside the heatmap body, so they
    # may only move between its left and right; column dendrogram/annotation
    # panels sit above/below it, so they may only move between top and
    # bottom (see `.heatmap_design()`). `NULL` skips validation for the
    # annotation sides: it means "omit this annotation panel" rather than
    # naming a side.
    rowDendroSide <- match.arg(rowDendroSide, c("left", "right"))
    colDendroSide <- match.arg(colDendroSide, c("top", "bottom"))
    if (!is.null(rowAnnotationSide)) rowAnnotationSide <- match.arg(rowAnnotationSide, c("left", "right"))
    if (!is.null(colAnnotationSide)) colAnnotationSide <- match.arg(colAnnotationSide, c("top", "bottom"))
    # `free(type = "space")` lets each annotation strip's variable-name axis
    # text occupy space without reserving room for it in the shared grid, so
    # annotations with different label lengths still pack tightly against
    # the heatmap instead of forcing uneven column/row widths.
    col_data <- if (!is.null(colAnnotationSide) &&
        (length(colVars) || (is.null(colVars) && ncol(SummarizedExperiment::colData(x))))) {
        patchwork::free(plotColData(x, vars = colVars, side = colAnnotationSide), type = "space")
    } else {
        NULL
    }
    row_data <- if (!is.null(rowAnnotationSide) &&
        (length(rowVars) || (is.null(rowVars) && ncol(SummarizedExperiment::rowData(x))))) {
        patchwork::free(plotRowData(x, vars = rowVars, side = rowAnnotationSide), type = "space")
    } else {
        NULL
    }
    col_dendro <- if (showColDendro && !is.null(colDendro(x))) plotColDendro(x, side = colDendroSide) else NULL
    row_dendro <- if (showRowDendro && !is.null(rowDendro(x))) plotRowDendro(x, side = rowDendroSide) else NULL
    main <- plotHeatmapMain(x, ...)
    guide <- patchwork::guide_area()
    # A hidden/omitted panel's *Side is reset to NULL here so `.heatmap_design()`
    # drops its letter from the grid entirely, rather than passing through a
    # side that no longer has a matching panel. `patchwork::wrap_plots()`
    # would otherwise still reserve a phantom slot for an unused
    # `plot_spacer()` sitting in `panels` even when its letter never appears
    # in `design`, leaving a gap where the omitted panel used to be.
    layout <- .heatmap_design(
        rowDendroSide = if (is.null(row_dendro)) NULL else rowDendroSide,
        rowAnnotationSide = if (is.null(row_data)) NULL else rowAnnotationSide,
        colDendroSide = if (is.null(col_dendro)) NULL else colDendroSide,
        colAnnotationSide = if (is.null(col_data)) NULL else colAnnotationSide,
        guideWidth = guideWidth
    )
    panels <- list(A = col_dendro, B = col_data, C = row_dendro, D = row_data, E = main, M = guide)
    panels <- panels[!vapply(panels, is.null, logical(1))]
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
# ("M") is always last and spans the full height of the grid, so stacked
# legends have the whole plot height to lay out in rather than being
# squeezed into the main panel's row alone.
# Any of the four side arguments may be `NULL`, meaning that component's
# panel is omitted entirely: it is dropped from the grid instead of
# reserving a blank slot for it. `identical()` (rather than `==`) is used
# throughout so a `NULL` side simply never matches "left"/"right"/"top"/
# "bottom", instead of raising a length-zero comparison error.
.heatmap_design <- function(rowDendroSide, rowAnnotationSide, colDendroSide, colAnnotationSide, guideWidth) {
    row_left <- c(
        if (identical(rowDendroSide, "left")) "dendro",
        if (identical(rowAnnotationSide, "left")) "annotation"
    )
    row_right <- c(
        if (identical(rowAnnotationSide, "right")) "annotation",
        if (identical(rowDendroSide, "right")) "dendro"
    )
    horiz_order <- c(row_left, "main", row_right)

    col_top <- c(
        if (identical(colDendroSide, "top")) "dendro",
        if (identical(colAnnotationSide, "top")) "annotation"
    )
    col_bottom <- c(
        if (identical(colAnnotationSide, "bottom")) "annotation",
        if (identical(colDendroSide, "bottom")) "dendro"
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
        paste0(paste(cells, collapse = ""), "M")
    }, character(1))

    list(
        design = paste(design_rows, collapse = "\n"),
        widths = c(unname(row_widths[horiz_order]), guideWidth),
        heights = unname(col_heights[vert_order])
    )
}
