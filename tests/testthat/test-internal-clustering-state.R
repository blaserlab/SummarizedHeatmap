test_that(".setRowClustering()/.setColClustering() sync dendrogram and order", {
    x <- make_heatmap()
    result <- .cluster_axis(x, 1L, "euclidean", "average")

    y <- .setRowClustering(x, result$dendrogram, result$order)
    expect_identical(rowDendro(y), result$dendrogram)
    expect_identical(rowOrder(y), as.integer(result$order))
    # Untouched axis is unaffected.
    expect_identical(colDendro(y), colDendro(x))
    expect_identical(colOrder(y), colOrder(x))
    expect_silent(validObject(y))

    result_col <- .cluster_axis(x, 2L, "euclidean", "average")
    z <- .setColClustering(x, result_col$dendrogram, result_col$order)
    expect_identical(colDendro(z), result_col$dendrogram)
    expect_identical(colOrder(z), as.integer(result_col$order))
    expect_identical(rowDendro(z), rowDendro(x))
    expect_silent(validObject(z))
})

test_that(".setRowClustering()/.setColClustering() accept a NULL dendrogram with a manual order", {
    x <- make_heatmap()

    y <- .setRowClustering(x, NULL, rev(seq_len(nrow(x))))
    expect_null(rowDendro(y))
    expect_identical(rowOrder(y), as.integer(rev(seq_len(nrow(x)))))
    expect_silent(validObject(y))

    z <- .setColClustering(x, NULL, rev(seq_len(ncol(x))))
    expect_null(colDendro(z))
    expect_identical(colOrder(z), as.integer(rev(seq_len(ncol(x)))))
    expect_silent(validObject(z))
})

test_that(".clearRowClustering()/.clearColClustering() reset to natural order", {
    x <- clusterHeatmap(make_heatmap())
    expect_s3_class(rowDendro(x), "dendrogram")
    expect_s3_class(colDendro(x), "dendrogram")

    no_rows <- .clearRowClustering(x)
    expect_null(rowDendro(no_rows))
    expect_identical(rowOrder(no_rows), seq_len(nrow(x)))
    # Column clustering is untouched.
    expect_identical(colDendro(no_rows), colDendro(x))
    expect_silent(validObject(no_rows))

    no_cols <- .clearColClustering(x)
    expect_null(colDendro(no_cols))
    expect_identical(colOrder(no_cols), seq_len(ncol(x)))
    expect_identical(rowDendro(no_cols), rowDendro(x))
    expect_silent(validObject(no_cols))
})

test_that(".clearRowClustering()/.clearColClustering() handle zero-length axes", {
    # A single remaining row/column can't carry a dendrogram, exercising the
    # same "too few elements to cluster" path as an empty axis without
    # requiring a zero-row/zero-column SummarizedHeatmap to be constructed.
    x <- make_heatmap(nrow = 1L, ncol = 1L)
    expect_null(rowDendro(x))
    expect_null(colDendro(x))

    cleared_rows <- .clearRowClustering(x)
    expect_null(rowDendro(cleared_rows))
    expect_identical(rowOrder(cleared_rows), 1L)
    expect_silent(validObject(cleared_rows))

    cleared_cols <- .clearColClustering(x)
    expect_null(colDendro(cleared_cols))
    expect_identical(colOrder(cleared_cols), 1L)
    expect_silent(validObject(cleared_cols))
})

test_that(".setRowClustering()/.setColClustering() handle a zero-length axis", {
    mat <- matrix(numeric(0), nrow = 0L, ncol = 3L, dimnames = list(character(0), LETTERS[1:3]))
    x <- SummarizedHeatmap(mat, clusterRows = FALSE)
    expect_identical(nrow(x), 0L)
    expect_identical(rowOrder(x), integer())

    y <- .setRowClustering(x, NULL, integer())
    expect_null(rowDendro(y))
    expect_identical(rowOrder(y), integer())
    expect_silent(validObject(y))

    cleared <- .clearRowClustering(x)
    expect_null(rowDendro(cleared))
    expect_identical(rowOrder(cleared), integer())
    expect_silent(validObject(cleared))

    mat2 <- matrix(numeric(0), nrow = 3L, ncol = 0L, dimnames = list(LETTERS[1:3], character(0)))
    x2 <- SummarizedHeatmap(mat2, clusterCols = FALSE)
    expect_identical(ncol(x2), 0L)
    expect_identical(colOrder(x2), integer())

    z <- .setColClustering(x2, NULL, integer())
    expect_null(colDendro(z))
    expect_identical(colOrder(z), integer())
    expect_silent(validObject(z))

    cleared_cols <- .clearColClustering(x2)
    expect_null(colDendro(cleared_cols))
    expect_identical(colOrder(cleared_cols), integer())
    expect_silent(validObject(cleared_cols))
})
