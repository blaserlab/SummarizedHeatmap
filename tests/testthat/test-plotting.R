test_that("component plots have the documented classes", {
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))
    expect_s3_class(plotHeatmapMain(x), "ggplot")
    expect_s3_class(plotRowDendro(x), "ggplot")
    expect_s3_class(plotColDendro(x), "ggplot")
    expect_s3_class(plotRowData(x, vars = "feature_type"), "ggplot")
    expect_s3_class(plotColData(x, vars = "condition"), "ggplot")
    expect_s3_class(plotRowData(x, vars = c("Feature" = "feature_type", "Again" = "feature_type2")), "patch")
    expect_s3_class(plotColData(x, vars = c("Condition" = "condition", "Batch" = "batch")), "patch")
    expect_s3_class(plotRowDendro(x)$coordinates, "CoordFlip")
    expect_s3_class(plotHeatmap(x), "patchwork")
})

test_that("multi-variable annotation plots compose safely with patchwork operators", {
    # `plotColData()`/`plotRowData()` wrap multi-variable results with
    # `wrap_elements()` so their internal panels stay fixed and don't get
    # flattened into a parent layout built with patchwork operators.
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))
    col_ann <- plotColData(x, vars = c("condition", "batch"))
    main <- plotHeatmapMain(x)
    combined <- (col_ann / main) +
        patchwork::plot_layout(heights = c(1, 5), guides = "collect")
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

test_that("plotHeatmap() keeps multi-variable annotation strips unwrapped for alignment", {
    # Unlike `plotColData()`/`plotRowData()`, which wrap multi-variable
    # results with `wrap_elements()` for safe composition, `plotHeatmap()`
    # must use the raw composite so patchwork can align its panels with the
    # main heatmap's actual axis positions (see `.annotation_plots()`).
    x <- make_heatmap()
    SummarizedExperiment::colData(x)$batch <- rep(c("one", "two"), length.out = ncol(x))
    classes <- vapply(plotHeatmap(x)$patches$plots, function(p) class(p)[[1]], character(1))
    expect_true("patchwork" %in% classes)
    expect_false("wrapped_patch" %in% classes)
})
