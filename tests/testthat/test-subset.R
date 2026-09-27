test_that("row subsetting invalidates only row clustering", {
    x <- make_heatmap()
    y <- x[1:4, ]
    expect_s4_class(y, "SummarizedHeatmap")
    expect_equal(dim(y), c(4L, ncol(x)))
    expect_null(rowDendro(y))
    expect_identical(rowOrder(y), seq_len(4L))
    expect_s3_class(colDendro(y), "dendrogram")
    expect_identical(colOrder(y), colOrder(x))
    expect_silent(validObject(y))
})

test_that("column logical and character subsetting invalidates only columns", {
    x <- make_heatmap()
    by_logical <- x[, c(TRUE, FALSE, TRUE, FALSE)]
    expect_null(colDendro(by_logical))
    expect_identical(colOrder(by_logical), 1:2)
    expect_s3_class(rowDendro(by_logical), "dendrogram")

    by_name <- x[, c("sample4", "sample2")]
    expect_equal(colnames(by_name), c("sample4", "sample2"))
    expect_null(colDendro(by_name))
    expect_silent(validObject(by_name))
})

test_that("unchanged full dimensions preserve clustering; empty subsets remain valid", {
    x <- make_heatmap()
    y <- x[seq_len(nrow(x)), ]
    expect_s3_class(rowDendro(y), "dendrogram")
    expect_s3_class(colDendro(y), "dendrogram")
    empty <- x[integer(), , drop = FALSE]
    expect_equal(dim(empty), c(0L, ncol(x)))
    expect_null(rowDendro(empty))
    expect_silent(validObject(empty))
})
