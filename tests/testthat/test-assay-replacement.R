test_that("assay<- invalidates row and column clustering when dendrograms are stored", {
    x <- clusterHeatmap(make_heatmap())
    expect_s3_class(rowDendro(x), "dendrogram")
    expect_s3_class(colDendro(x), "dendrogram")

    new_values <- SummarizedExperiment::assay(x) * -1
    assay(x) <- new_values

    expect_identical(unname(SummarizedExperiment::assay(x)), unname(new_values))
    expect_null(rowDendro(x))
    expect_null(colDendro(x))
    expect_identical(rowOrder(x), seq_len(nrow(x)))
    expect_identical(colOrder(x), seq_len(ncol(x)))
    expect_silent(validObject(x))
})

test_that("assay<- preserves manual row/column order when no dendrogram is stored", {
    x <- make_heatmap()
    manual_rows <- rev(seq_len(nrow(x)))
    manual_cols <- rev(seq_len(ncol(x)))
    x <- .setRowClustering(x, NULL, manual_rows)
    x <- .setColClustering(x, NULL, manual_cols)

    assay(x) <- SummarizedExperiment::assay(x) + 1

    expect_null(rowDendro(x))
    expect_null(colDendro(x))
    expect_identical(rowOrder(x), as.integer(manual_rows))
    expect_identical(colOrder(x), as.integer(manual_cols))
    expect_silent(validObject(x))
})

test_that("assay<- applies the invalidation rule independently per axis", {
    x <- clusterHeatmap(make_heatmap())
    x <- .clearColClustering(x)
    expect_s3_class(rowDendro(x), "dendrogram")
    expect_null(colDendro(x))
    manual_cols <- colOrder(x)

    assay(x) <- SummarizedExperiment::assay(x) * 2

    expect_null(rowDendro(x))
    expect_identical(rowOrder(x), seq_len(nrow(x)))
    expect_null(colDendro(x))
    expect_identical(colOrder(x), manual_cols)
})

test_that("assay<- never automatically reclusters", {
    x <- clusterHeatmap(make_heatmap())
    old_row_dendro <- rowDendro(x)

    new_values <- SummarizedExperiment::assay(x)
    new_values[] <- rev(new_values)
    assay(x) <- new_values

    # A fresh clustering on these (very different) values would not match the
    # cleared/natural-order state left behind by assay<-.
    expect_null(rowDendro(x))
    expect_false(identical(rowOrder(x), as.integer(order.dendrogram(old_row_dendro))))
    expect_identical(rowOrder(x), seq_len(nrow(x)))
})

test_that("assay<- with index/name argument (assay(x, i) <-) also invalidates clustering", {
    x <- clusterHeatmap(make_heatmap())
    assay(x, 1) <- SummarizedExperiment::assay(x) * 3
    expect_null(rowDendro(x))
    expect_null(colDendro(x))

    y <- clusterHeatmap(make_heatmap())
    assay(y, "matrix") <- SummarizedExperiment::assay(y) * 3
    expect_null(rowDendro(y))
    expect_null(colDendro(y))
})

test_that("assays<- invalidates clustering and preserves manual order", {
    x <- clusterHeatmap(make_heatmap())
    assays(x) <- list(matrix = SummarizedExperiment::assay(x) * 5)
    expect_null(rowDendro(x))
    expect_null(colDendro(x))

    y <- make_heatmap()
    y <- .setRowClustering(y, NULL, rev(seq_len(nrow(y))))
    assays(y) <- S4Vectors::SimpleList(matrix = SummarizedExperiment::assay(y) + 1)
    expect_null(rowDendro(y))
    expect_identical(rowOrder(y), as.integer(rev(seq_len(nrow(y)))))
})

test_that("assay<- preserves SummarizedExperiment dimension/dimname validation", {
    x <- make_heatmap()
    bad_values <- SummarizedExperiment::assay(x)[1:3, ]
    expect_error(assay(x) <- bad_values)
})

test_that("plotHeatmap() omits dendrogram panels once assay<- invalidates clustering", {
    # `make_heatmap()` clusters both axes by default; a stored dendrogram
    # normally adds a row and a column dendrogram panel to the composed
    # patchwork (see `plotHeatmap()`'s `!is.null(rowDendro(x))`/
    # `colDendro(x)` guards). After `assay<-` invalidates both dendrograms,
    # those two panels should disappear rather than plotting stale
    # dendrograms or erroring -- matching the panel count you'd get by
    # passing `showRowDendro = showColDendro = FALSE` explicitly.
    x <- make_heatmap()
    expect_s3_class(rowDendro(x), "dendrogram")
    expect_s3_class(colDendro(x), "dendrogram")
    baseline <- length(plotHeatmap(x))

    assay(x) <- SummarizedExperiment::assay(x) * -1
    expect_null(rowDendro(x))
    expect_null(colDendro(x))

    p <- plotHeatmap(x)
    expect_s3_class(p, "patchwork")
    expect_equal(length(p), baseline - 2L)
    expect_equal(length(p), length(plotHeatmap(make_heatmap(), showRowDendro = FALSE, showColDendro = FALSE)))

    # Calling the dendrogram plotting functions directly still fails
    # clearly, rather than silently drawing a dendrogram computed from the
    # old assay values.
    expect_error(plotRowDendro(x), "no row dendrogram is stored")
    expect_error(plotColDendro(x), "no column dendrogram is stored")
})
