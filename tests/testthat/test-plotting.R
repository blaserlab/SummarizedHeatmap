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

test_that("annotation tiles always label their variable as axis text", {
    x <- make_heatmap()
    col_plot <- plotColData(x, vars = "condition")
    expect_s3_class(col_plot$theme$axis.text.y, "element_text")
    row_plot <- plotRowData(x, vars = "feature_type", side = "right")
    expect_s3_class(row_plot$theme$axis.text.x, "element_text")
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
