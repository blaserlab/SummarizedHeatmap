test_that("component plots have the documented classes", {
    x <- make_heatmap()
    expect_s3_class(plotHeatmapMain(x), "ggplot")
    expect_s3_class(plotRowDendro(x), "ggplot")
    expect_s3_class(plotColDendro(x), "ggplot")
    expect_s3_class(plotRowData(x, vars = "feature_type"), "ggplot")
    expect_s3_class(plotColData(x, vars = "condition"), "ggplot")
    expect_s3_class(plotRowData(x, vars = c("Feature" = "feature_type", "Again" = "feature_type2")), "patchwork")
    expect_s3_class(plotHeatmap(x), "patchwork")
})

test_that("annotation variables and missing dendrograms are validated", {
    x <- make_heatmap()
    expect_error(plotRowData(x, vars = "missing"), "unknown row annotation")
    manual <- SummarizedHeatmap(SummarizedExperiment::assay(x), rowOrder = seq_len(nrow(x)))
    expect_error(plotRowDendro(manual), "no row dendrogram")
    expect_error(plotHeatmapMain("not a heatmap"), "must be a SummarizedHeatmap")
})
