test_that("component plots have the documented classes", {
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))
    expect_s3_class(plotHeatmapMain(x), "ggplot")
    expect_s3_class(plotRowDendro(x), "ggplot")
    expect_s3_class(plotColDendro(x), "ggplot")
    expect_s3_class(plotRowData(x, vars = "feature_type"), "ggplot")
    expect_s3_class(plotColData(x, vars = "condition"), "ggplot")
    expect_s3_class(plotRowData(x, vars = c("Feature" = "feature_type", "Again" = "feature_type2")), "patchwork")
    expect_s3_class(plotColData(x, vars = c("Condition" = "condition", "Batch" = "batch")), "patchwork")
    expect_s3_class(plotRowDendro(x)$coordinates, "CoordFlip")
    expect_s3_class(plotHeatmap(x), "patchwork")
})

test_that("annotation tiles label their variable with axis text matching the main heatmap's theme", {
    x <- make_heatmap()
    main_theme <- plotHeatmapMain(x)$theme
    expect_equal(main_theme$axis.text$colour, "black")
    expect_equal(main_theme$axis.text.y$hjust, 0)

    # Column annotations (side = "top"/"bottom"): the variable-name axis is
    # `y`, always drawn on the right like the main heatmap's row labels, so
    # it should be justified and coloured the same way.
    col_plot <- plotColData(x, vars = "condition")
    expect_s3_class(col_plot$theme$axis.text.y, "element_text")
    expect_equal(col_plot$theme$axis.text.y$colour, "black")
    expect_equal(col_plot$theme$axis.text.y$hjust, 0)

    # Row annotations (side = "left"/"right"): the variable-name axis is the
    # (narrow) `x` axis, so the label is rotated vertical to fit.
    row_plot <- plotRowData(x, vars = "feature_type", side = "right")
    expect_s3_class(row_plot$theme$axis.text.x, "element_text")
    expect_equal(row_plot$theme$axis.text.x$colour, "black")
    expect_equal(row_plot$theme$axis.text.x$angle, 90)
})

test_that("plotRowData()'s variable-name label sits at the side with free space by default", {
    # In `plotHeatmap()`'s composite grid, a row-axis panel's neighbour
    # above it is always blank regardless of whether the panel is placed to
    # the "left" or "right" of the heatmap, so anchoring the label at "top"
    # keeps it compact on either side; "right" (the more clipping-prone
    # option per the docs) anchors it at "bottom" instead.
    x <- make_heatmap()
    left_plot <- plotRowData(x, vars = "feature_type", side = "left")
    right_plot <- plotRowData(x, vars = "feature_type", side = "right")
    expect_equal(left_plot$scales$get_scales("x")$position, "top")
    expect_equal(right_plot$scales$get_scales("x")$position, "bottom")
})

test_that("multi-variable annotation results compose safely via wrap_plots(list(...))", {
    # Combining a multi-variable annotation patchwork directly with another
    # plot via `/`/`+`/`|` can flatten both objects' panel lists together
    # (see `.annotation_plots()`). `wrap_plots(list(...))` keeps each list
    # element as a single area instead, which is the pattern this test (and
    # `plotHeatmap()`) relies on.
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))
    col_ann <- plotColData(x, vars = c("condition", "batch"))
    main <- plotHeatmapMain(x)
    combined <- patchwork::wrap_plots(
        list(col_ann, main),
        ncol = 1, heights = c(1, 5), guides = "collect"
    )
    expect_s3_class(combined, "patchwork")
    expect_equal(length(combined$patches$plots) + 1L, 2L)
})

test_that("annotation variables and missing dendrograms are validated", {
    x <- make_heatmap()
    expect_error(plotRowData(x, vars = "missing"), "unknown row annotation")
    manual <- SummarizedHeatmap(SummarizedExperiment::assay(x), rowOrder = seq_len(nrow(x)))
    expect_error(plotRowDendro(manual), "no row dendrogram")
    expect_error(plotHeatmapMain("not a heatmap"), "must be a SummarizedHeatmap")
})

test_that("plotRowData()/plotColData() reject sides that don't apply to their axis", {
    # Row annotation tiles always place row identifiers on the vertical
    # axis, so only left/right placement makes sense; top/bottom previously
    # silently produced a mis-oriented, column-style plot instead of
    # erroring.
    x <- make_heatmap()
    expect_error(plotRowData(x, vars = "feature_type", side = "top"), "'arg' should be one of")
    expect_error(plotRowData(x, vars = "feature_type", side = "bottom"), "'arg' should be one of")
    # Column annotation tiles always place column identifiers on the
    # horizontal axis, so only top/bottom placement makes sense; left/right
    # previously silently produced a mis-oriented, row-style plot instead of
    # erroring.
    expect_error(plotColData(x, vars = "condition", side = "left"), "'arg' should be one of")
    expect_error(plotColData(x, vars = "condition", side = "right"), "'arg' should be one of")
})

test_that("plotRowData()/plotColData() lay tiles out along their matching axis", {
    x <- make_heatmap()
    # Row annotation: one tile per row, so the (discrete) position scale is
    # on y and there are as many break positions as rows.
    row_plot <- plotRowData(x, vars = "feature_type")
    expect_true("y" %in% names(row_plot$mapping))
    expect_equal(length(ggplot2::layer_scales(row_plot)$y$get_labels()), nrow(x))
    # Column annotation: one tile per column, so the (discrete) position
    # scale is on x and there are as many break positions as columns.
    col_plot <- plotColData(x, vars = "condition")
    expect_true("x" %in% names(col_plot$mapping))
    expect_equal(length(ggplot2::layer_scales(col_plot)$x$get_labels()), ncol(x))
})

test_that("plotHeatmap()'s guideWidth controls the guide column width", {
    x <- make_heatmap()
    expect_equal(plotHeatmap(x)$patches$layout$widths[[4]], 3)
    expect_equal(plotHeatmap(x, guideWidth = 5)$patches$layout$widths[[4]], 5)
    expect_error(plotHeatmap(x, guideWidth = 0), "'guideWidth' must be a single positive number")
    expect_error(plotHeatmap(x, guideWidth = c(1, 2)), "'guideWidth' must be a single positive number")
    expect_error(plotHeatmap(x, guideWidth = NA_real_), "'guideWidth' must be a single positive number")
    expect_error(plotHeatmap(x, colAnnotationSide = "nowhere"), "'arg' should be one of")
    expect_error(plotHeatmap(x, rowAnnotationSide = "nowhere"), "'arg' should be one of")
})

test_that("plotHeatmap() keeps multi-variable annotation strips aligned with the heatmap", {
    # Annotation results stay unwrapped (see `.annotation_plots()`), so
    # `plotHeatmap()`'s list + `design` composition keeps its panels
    # aligned with the main heatmap's axis positions and its guides
    # collectible into the shared guide area.
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))
    classes <- vapply(plotHeatmap(x)$patches$plots, function(p) class(p)[[1]], character(1))
    expect_true("patchwork" %in% classes)
    expect_false("wrapped_patch" %in% classes)
})

test_that("plotHeatmap() packs annotation label text with patchwork::free(type = \"space\")", {
    x <- make_heatmap()
    classes <- lapply(plotHeatmap(x)$patches$plots, class)
    expect_true(any(vapply(classes, function(cl) "free_plot" %in% cl, logical(1))))
})

# `plotHeatmap()`'s panels are composed in a fixed A-M order (column
# dendrogram, column annotation, row dendrogram, row annotation, main,
# guide), but a panel is dropped from `$patches$layout$design` entirely
# when its corresponding `*Side` argument is `NULL` (see `.heatmap_design()`).
# `letters` lets a test declare which panels are actually present so the
# lookup index stays correct; it defaults to all six.
panel_rect <- function(p, letter, letters = c("A", "B", "C", "D", "E", "M")) {
    idx <- match(letter, letters)
    design <- p$patches$layout$design
    list(t = design$t[idx], l = design$l[idx], b = design$b[idx], r = design$r[idx])
}

test_that("plotHeatmap()'s *Side arguments move panels, not just orient them", {
    x <- make_heatmap()

    # Defaults: row dendrogram and row annotation both left of the heatmap
    # (dendrogram further out, annotation adjacent); column dendrogram and
    # column annotation both above it (dendrogram further out, annotation
    # adjacent).
    p <- plotHeatmap(x)
    main <- panel_rect(p, "E")
    expect_lt(panel_rect(p, "C")$l, panel_rect(p, "D")$l)
    expect_lt(panel_rect(p, "D")$l, main$l)
    expect_lt(panel_rect(p, "A")$t, panel_rect(p, "B")$t)
    expect_lt(panel_rect(p, "B")$t, main$t)

    # rowDendroSide = "right" moves the row dendrogram panel to the right of
    # the heatmap, to the opposite side of the (still default, "left") row
    # annotation panel (previously it only mirrored the dendrogram's
    # orientation while leaving the panel on the left).
    p_dendro_right <- plotHeatmap(x, rowDendroSide = "right")
    main_dr <- panel_rect(p_dendro_right, "E")
    expect_gt(panel_rect(p_dendro_right, "C")$l, main_dr$l)
    expect_lt(panel_rect(p_dendro_right, "D")$l, main_dr$l)

    # rowAnnotationSide = "right" moves the row annotation panel to the
    # right of the heatmap, to the opposite side of the (still default,
    # "left") row dendrogram panel (previously changing this argument had no
    # visible effect on the composed layout).
    p_ann_right <- plotHeatmap(x, rowAnnotationSide = "right")
    main_ar <- panel_rect(p_ann_right, "E")
    expect_gt(panel_rect(p_ann_right, "D")$l, main_ar$l)
    expect_lt(panel_rect(p_ann_right, "C")$l, main_ar$l)

    # With both the dendrogram and annotation moved to the right, the
    # annotation stays adjacent to the heatmap and the dendrogram sits
    # further out (mirroring the default "both left" ordering).
    p_both_right <- plotHeatmap(x, rowDendroSide = "right", rowAnnotationSide = "right")
    main_br <- panel_rect(p_both_right, "E")
    expect_lt(main_br$l, panel_rect(p_both_right, "D")$l)
    expect_lt(panel_rect(p_both_right, "D")$l, panel_rect(p_both_right, "C")$l)

    # colDendroSide = "bottom" moves the column dendrogram panel below the
    # heatmap.
    p_col_bottom <- plotHeatmap(x, colDendroSide = "bottom")
    main_cb <- panel_rect(p_col_bottom, "E")
    expect_gt(panel_rect(p_col_bottom, "A")$t, main_cb$t)
})

test_that("plotHeatmap()'s *AnnotationSide arguments accept NULL to omit that panel", {
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))

    # `colAnnotationSide = NULL` drops the column annotation panel from the
    # layout entirely (rather than leaving a reserved but blank slot, as
    # `showColDendro = FALSE` also does for the dendrogram -- see the test
    # below).
    p_no_col_ann <- plotHeatmap(x, colAnnotationSide = NULL)
    present <- c("A", "C", "D", "E", "M")
    expect_equal(length(p_no_col_ann$patches$layout$design$t), length(present))
    main_nca <- panel_rect(p_no_col_ann, "E", letters = present)
    expect_lt(panel_rect(p_no_col_ann, "A", letters = present)$t, main_nca$t)

    # `rowAnnotationSide = NULL` likewise drops the row annotation panel.
    p_no_row_ann <- plotHeatmap(x, rowAnnotationSide = NULL)
    present <- c("A", "B", "C", "E", "M")
    expect_equal(length(p_no_row_ann$patches$layout$design$t), length(present))
    main_nra <- panel_rect(p_no_row_ann, "E", letters = present)
    expect_lt(panel_rect(p_no_row_ann, "C", letters = present)$l, main_nra$l)
})

test_that("omitted panels don't leave an unreferenced plot_spacer() behind", {
    # `patchwork::wrap_plots()` silently retains an unused `plot_spacer()` in
    # `$patches$plots` even when its letter never appears in `design` --
    # unlike an unused ordinary ggplot, which is dropped entirely. That
    # leftover spacer visibly distorted the rendered layout (a gap where the
    # omitted panel used to be) despite `$patches$layout$design` looking
    # correct, so `plotHeatmap()` must exclude hidden panels from the list
    # passed to `wrap_plots()` rather than substituting a spacer for them.
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))

    baseline <- length(plotHeatmap(x)$patches$plots)
    expect_equal(length(plotHeatmap(x, rowAnnotationSide = NULL)$patches$plots), baseline - 1L)
    expect_equal(length(plotHeatmap(x, colAnnotationSide = NULL)$patches$plots), baseline - 1L)
    expect_equal(length(plotHeatmap(x, showRowDendro = FALSE)$patches$plots), baseline - 1L)
    expect_equal(length(plotHeatmap(x, showColDendro = FALSE)$patches$plots), baseline - 1L)
    expect_equal(
        length(plotHeatmap(x, showRowDendro = FALSE, showColDendro = FALSE, rowAnnotationSide = NULL)$patches$plots),
        baseline - 3L
    )
})

test_that("showRowDendro/showColDendro = FALSE drop the dendrogram's reserved grid slot", {
    x <- make_heatmap()

    # With the row dendrogram hidden, the row annotation panel (still
    # present, at its default "left" side) should be adjacent to the main
    # heatmap panel, not separated by a leftover blank column.
    p_no_row_dendro <- plotHeatmap(x, showRowDendro = FALSE)
    present <- c("A", "B", "D", "E", "M")
    expect_equal(length(p_no_row_dendro$patches$layout$design$t), length(present))
    main_nrd <- panel_rect(p_no_row_dendro, "E", letters = present)
    expect_lt(panel_rect(p_no_row_dendro, "D", letters = present)$l, main_nrd$l)

    # Likewise for the column dendrogram.
    p_no_col_dendro <- plotHeatmap(x, showColDendro = FALSE)
    present <- c("B", "C", "D", "E", "M")
    expect_equal(length(p_no_col_dendro$patches$layout$design$t), length(present))
    main_ncd <- panel_rect(p_no_col_dendro, "E", letters = present)
    expect_lt(panel_rect(p_no_col_dendro, "B", letters = present)$t, main_ncd$t)
})

test_that("plotHeatmap() rejects sides that don't apply to a panel's axis", {
    x <- make_heatmap()
    # A column dendrogram/annotation spans columns, so it can only be placed
    # above or below the heatmap, never to its left or right.
    expect_error(plotHeatmap(x, colDendroSide = "right"), "'arg' should be one of")
    expect_error(plotHeatmap(x, colDendroSide = "left"), "'arg' should be one of")
    expect_error(plotHeatmap(x, colAnnotationSide = "right"), "'arg' should be one of")
    expect_error(plotHeatmap(x, colAnnotationSide = "left"), "'arg' should be one of")
    # A row dendrogram/annotation spans rows, so it can only be placed left
    # or right of the heatmap, never above or below.
    expect_error(plotHeatmap(x, rowDendroSide = "top"), "'arg' should be one of")
    expect_error(plotHeatmap(x, rowDendroSide = "bottom"), "'arg' should be one of")
    expect_error(plotHeatmap(x, rowAnnotationSide = "top"), "'arg' should be one of")
    expect_error(plotHeatmap(x, rowAnnotationSide = "bottom"), "'arg' should be one of")
})
