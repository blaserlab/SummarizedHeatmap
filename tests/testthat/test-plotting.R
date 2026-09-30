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

test_that("plotHeatmapMain() uses midpoint and fillTitle in the fill scale", {
    x <- make_heatmap()

    # The midpoint isn't stored as a plain field on the scale; rescaling the
    # midpoint value itself should land at 0.5 regardless of its range.
    default_rescaler <- plotHeatmapMain(x)$scales$get_scales("fill")$rescaler
    expect_equal(default_rescaler(0, from = c(-4, 4)), 0.5)
    expect_equal(plotHeatmapMain(x)$labels$fill, "Value")

    custom_main <- plotHeatmapMain(x, midpoint = 5, fillTitle = "Count")
    custom_rescaler <- custom_main$scales$get_scales("fill")$rescaler
    expect_equal(custom_rescaler(5, from = c(0, 10)), 0.5)
    expect_equal(custom_main$labels$fill, "Count")
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
    # `length()`/`as.list()` are patchwork's own generic-based way of
    # inspecting a composition's top-level elements, so checking two remain
    # here confirms `col_ann`'s internal panels weren't flattened into
    # `combined`'s own list alongside `main`.
    expect_equal(length(combined), 2L)
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

test_that("plotHeatmap()'s guideWidth is validated and accepted", {
    # `.heatmap_design()`'s own tests cover how `guideWidth` maps onto the
    # guide column's width; this just checks `plotHeatmap()` validates and
    # forwards it without erroring.
    x <- make_heatmap()
    expect_s3_class(plotHeatmap(x, guideWidth = 5), "patchwork")
    expect_error(plotHeatmap(x, guideWidth = 0), "'guideWidth' must be a single positive number")
    expect_error(plotHeatmap(x, guideWidth = c(1, 2)), "'guideWidth' must be a single positive number")
    expect_error(plotHeatmap(x, guideWidth = NA_real_), "'guideWidth' must be a single positive number")
    expect_error(plotHeatmap(x, colAnnotationSide = "nowhere"), "'arg' should be one of")
    expect_error(plotHeatmap(x, rowAnnotationSide = "nowhere"), "'arg' should be one of")
})

test_that("plotHeatmap() omits the guide-area panel and column when collectGuides = FALSE", {
    x <- make_heatmap()
    collected <- plotHeatmap(x, collectGuides = TRUE, guideWidth = 3)
    kept <- plotHeatmap(x, collectGuides = FALSE, guideWidth = 3)

    # One fewer top-level panel (no `guide_area()`), and no trailing
    # `guideWidth` entry in the widths vector.
    expect_equal(length(kept), length(collected) - 1L)
    expect_equal(kept$patches$layout$widths, collected$patches$layout$widths[-length(collected$patches$layout$widths)])

    collected_classes <- vapply(as.list(collected), function(p) class(p)[[1]], character(1))
    kept_classes <- vapply(as.list(kept), function(p) class(p)[[1]], character(1))
    expect_true("guide_area" %in% collected_classes)
    expect_false("guide_area" %in% kept_classes)

    # Normal per-panel legends are still drawn (multiple "guide-box" grobs)
    # rather than none at all, i.e. `guides = "keep"` still applies.
    gt <- patchwork::patchworkGrob(kept)
    guide_boxes <- gt$grobs[vapply(gt$grobs, function(g) identical(g$name, "guide-box"), logical(1))]
    expect_gt(length(guide_boxes), 0L)
})

test_that("plotHeatmap() keeps multi-variable annotation strips aligned with the heatmap", {
    # Annotation results stay unwrapped (see `.annotation_plots()`), so
    # `plotHeatmap()`'s list + `design` composition keeps its panels
    # aligned with the main heatmap's axis positions and its guides
    # collectible into the shared guide area.
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))
    # `as.list()` is patchwork's own generic-based way of inspecting a
    # composition's top-level elements.
    classes <- vapply(as.list(plotHeatmap(x)), function(p) class(p)[[1]], character(1))
    expect_true("patchwork" %in% classes)
    expect_false("wrapped_patch" %in% classes)
})

test_that("plotHeatmap() packs annotation label text with patchwork::free(type = \"space\")", {
    x <- make_heatmap()
    classes <- lapply(as.list(plotHeatmap(x)), class)
    expect_true(any(vapply(classes, function(cl) "free_plot" %in% cl, logical(1))))
})

test_that("plotHeatmap()'s *Side arguments and panel-omitting options don't error", {
    # Detailed panel placement/omission coverage (which side each panel
    # lands on, which grid slot gets dropped, and how widths/heights adjust)
    # lives in `.heatmap_design()`'s own tests, since `plotHeatmap()` just
    # forwards these arguments to it unchanged; this only checks that
    # `plotHeatmap()` builds a valid patchwork for each supported
    # combination.
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))

    expect_s3_class(plotHeatmap(x, rowDendroSide = "right"), "patchwork")
    expect_s3_class(plotHeatmap(x, rowAnnotationSide = "right"), "patchwork")
    expect_s3_class(plotHeatmap(x, rowDendroSide = "right", rowAnnotationSide = "right"), "patchwork")
    expect_s3_class(plotHeatmap(x, colDendroSide = "bottom"), "patchwork")
    expect_s3_class(plotHeatmap(x, colAnnotationSide = "bottom"), "patchwork")
    expect_s3_class(plotHeatmap(x, colAnnotationSide = NULL), "patchwork")
    expect_s3_class(plotHeatmap(x, rowAnnotationSide = NULL), "patchwork")
    expect_s3_class(plotHeatmap(x, showRowDendro = FALSE), "patchwork")
    expect_s3_class(plotHeatmap(x, showColDendro = FALSE), "patchwork")
    expect_s3_class(
        plotHeatmap(x, showRowDendro = FALSE, showColDendro = FALSE, rowAnnotationSide = NULL, colAnnotationSide = NULL),
        "patchwork"
    )
})

test_that("omitted panels don't leave an unreferenced plot_spacer() behind", {
    # `plotHeatmap()` excludes hidden panels from the list passed to
    # `wrap_plots()` rather than substituting a `plot_spacer()` for them (see
    # the comment above `panels <- panels[...]` in `plotHeatmap()`), so the
    # composition's top-level element count (via patchwork's own `length()`
    # method) should drop by exactly one per omitted panel.
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))

    baseline <- length(plotHeatmap(x))
    expect_equal(length(plotHeatmap(x, rowAnnotationSide = NULL)), baseline - 1L)
    expect_equal(length(plotHeatmap(x, colAnnotationSide = NULL)), baseline - 1L)
    expect_equal(length(plotHeatmap(x, showRowDendro = FALSE)), baseline - 1L)
    expect_equal(length(plotHeatmap(x, showColDendro = FALSE)), baseline - 1L)
    expect_equal(
        length(plotHeatmap(x, showRowDendro = FALSE, showColDendro = FALSE, rowAnnotationSide = NULL)),
        baseline - 3L
    )
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

test_that("plotHeatmap() forwards midpoint and fillTitle to the main panel via '...'", {
    x <- make_heatmap()
    combined <- as.list(plotHeatmap(x, midpoint = 2, fillTitle = "Count"))
    fill_labels <- vapply(combined, function(p) {
        if (is.null(p$labels$fill)) NA_character_ else p$labels$fill
    }, character(1))
    expect_true("Count" %in% fill_labels)

    main <- combined[[which(fill_labels == "Count")]]
    expect_equal(main$scales$get_scales("fill")$rescaler(2, from = c(0, 4)), 0.5)
})

test_that("plotHeatmap() rejects 'flip' while plotHeatmapMain() still accepts it", {
    x <- make_heatmap()
    # Flipping only the main panel would misalign it against the annotation
    # and dendrogram panels `plotHeatmap()` arranges around it, so it must
    # reject 'flip' rather than pass it through via `...`.
    expect_error(plotHeatmap(x, flip = TRUE), "plotHeatmapMain")
    expect_s3_class(plotHeatmapMain(x, flip = TRUE), "ggplot")
})
