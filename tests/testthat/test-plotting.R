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

# `plotHeatmap()`'s panels are always composed in a fixed A-F order (column
# dendrogram, column annotation, row dendrogram, row annotation, main,
# guide); this helper reads a panel's grid position back out regardless of
# where `*Side` arguments have placed it.
panel_rect <- function(p, letter) {
    idx <- match(letter, c("A", "B", "C", "D", "E", "M"))
    design <- p$patches$layout$design
    list(t = design$t[idx], l = design$l[idx], b = design$b[idx], r = design$r[idx])
}

test_that("plotHeatmap()'s *Side arguments move panels, not just orient them", {
    x <- make_heatmap()

    # Defaults: row dendrogram left of the heatmap, row annotation right of
    # it; column dendrogram above the column annotation, both above the
    # heatmap.
    p <- plotHeatmap(x)
    main <- panel_rect(p, "E")
    expect_lt(panel_rect(p, "C")$l, main$l)
    expect_gt(panel_rect(p, "D")$l, main$l)
    expect_lt(panel_rect(p, "A")$t, panel_rect(p, "B")$t)
    expect_lt(panel_rect(p, "B")$t, main$t)

    # rowDendroSide = "right" moves the row dendrogram panel to the right of
    # the heatmap (previously it only mirrored the dendrogram's orientation
    # while leaving the panel on the left).
    p_dendro_right <- plotHeatmap(x, rowDendroSide = "right")
    main_dr <- panel_rect(p_dendro_right, "E")
    expect_gt(panel_rect(p_dendro_right, "C")$l, main_dr$l)
    # With both the dendrogram and (default) annotation on the right, the
    # annotation stays adjacent to the heatmap and the dendrogram sits
    # further out.
    expect_lt(panel_rect(p_dendro_right, "D")$l, panel_rect(p_dendro_right, "C")$l)

    # rowAnnotationSide = "left" moves the row annotation panel to the left
    # of the heatmap (previously changing this argument had no visible
    # effect on the composed layout).
    p_ann_left <- plotHeatmap(x, rowAnnotationSide = "left")
    main_al <- panel_rect(p_ann_left, "E")
    expect_lt(panel_rect(p_ann_left, "D")$l, main_al$l)
    expect_lt(panel_rect(p_ann_left, "C")$l, panel_rect(p_ann_left, "D")$l)

    # colDendroSide = "bottom" moves the column dendrogram panel below the
    # heatmap.
    p_col_bottom <- plotHeatmap(x, colDendroSide = "bottom")
    main_cb <- panel_rect(p_col_bottom, "E")
    expect_gt(panel_rect(p_col_bottom, "A")$t, main_cb$t)
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
