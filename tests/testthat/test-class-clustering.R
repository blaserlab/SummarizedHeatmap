test_that("constructor and accessors preserve dimensions and clustering", {
    x <- make_heatmap()
    expect_s4_class(x, "SummarizedHeatmap")
    expect_s4_class(x, "SummarizedExperiment")
    expect_identical(length(rowOrder(x)), nrow(x))
    expect_identical(length(colOrder(x)), ncol(x))
    expect_true(all(sort(rowOrder(x)) == seq_len(nrow(x))))
    expect_true(all(sort(colOrder(x)) == seq_len(ncol(x))))
    expect_s3_class(rowDendro(x), "dendrogram")
    expect_s3_class(colDendro(x), "dendrogram")
    expect_silent(validObject(x))
})

test_that("manual orders and invalid constructor inputs are checked", {
    x <- SummarizedHeatmap(matrix(1:12, 4, dimnames = list(letters[1:4], LETTERS[1:3])),
        rowOrder = c("d", "c", "b", "a")
    )
    expect_identical(rowOrder(x), 4:1)
    expect_null(rowDendro(x))
    expect_error(SummarizedHeatmap(1:5), "numeric matrix")
    expect_error(SummarizedHeatmap(matrix(1:4, 2), rowOrder = c(1, 1)), "permutation")
})

test_that("clustering helpers update the requested axes", {
    x <- make_heatmap()
    row_only <- clusterRows(x, distMethod = "manhattan", hclustMethod = "complete")
    expect_s3_class(rowDendro(row_only), "dendrogram")
    expect_identical(colOrder(row_only), colOrder(x))
    both <- clusterHeatmap(x)
    expect_silent(validObject(both))
    expect_length(rowOrder(both), nrow(both))
    expect_length(colOrder(both), ncol(both))
})

test_that("single and empty dimensions are valid and are not clustered", {
    one_row <- SummarizedHeatmap(matrix(1:3, 1, dimnames = list("f", paste0("s", 1:3))))
    expect_null(rowDendro(one_row))
    expect_identical(rowOrder(one_row), 1L)
    empty <- SummarizedHeatmap(matrix(numeric(),
        nrow = 0, ncol = 2,
        dimnames = list(character(), c("s1", "s2"))
    ))
    expect_silent(validObject(empty))
    expect_length(rowOrder(empty), 0L)
    expect_null(colDendro(empty))
    one_column <- SummarizedHeatmap(matrix(1:3,
        ncol = 1,
        dimnames = list(paste0("f", 1:3), "s")
    ))
    expect_null(colDendro(one_column))
    empty_columns <- SummarizedHeatmap(matrix(numeric(),
        nrow = 2, ncol = 0,
        dimnames = list(c("f1", "f2"), character())
    ))
    expect_silent(validObject(empty_columns))
    expect_length(colOrder(empty_columns), 0L)
})

test_that("clusterRows/clusterCols control dendrogram computation", {
    mat <- matrix(1:12, 4, dimnames = list(letters[1:4], LETTERS[1:3]))

    both <- SummarizedHeatmap(mat)
    expect_s3_class(rowDendro(both), "dendrogram")
    expect_s3_class(colDendro(both), "dendrogram")
    expect_silent(validObject(both))

    neither <- SummarizedHeatmap(mat, clusterRows = FALSE, clusterCols = FALSE)
    expect_null(rowDendro(neither))
    expect_null(colDendro(neither))
    expect_identical(rowOrder(neither), seq_len(nrow(mat)))
    expect_identical(colOrder(neither), seq_len(ncol(mat)))
    expect_silent(validObject(neither))

    rows_only <- SummarizedHeatmap(mat, clusterCols = FALSE)
    expect_s3_class(rowDendro(rows_only), "dendrogram")
    expect_null(colDendro(rows_only))
    expect_identical(colOrder(rows_only), seq_len(ncol(mat)))
    expect_silent(validObject(rows_only))

    cols_only <- SummarizedHeatmap(mat, clusterRows = FALSE)
    expect_null(rowDendro(cols_only))
    expect_s3_class(colDendro(cols_only), "dendrogram")
    expect_identical(rowOrder(cols_only), seq_len(nrow(mat)))
    expect_silent(validObject(cols_only))
})

test_that("explicit orders are honored without a dendrogram even when clustering is requested", {
    mat <- matrix(1:12, 4, dimnames = list(letters[1:4], LETTERS[1:3]))

    x <- SummarizedHeatmap(mat,
        rowOrder = c("d", "c", "b", "a"), colOrder = c("C", "B", "A"),
        clusterRows = TRUE, clusterCols = TRUE
    )
    expect_identical(rowOrder(x), 4:1)
    expect_identical(colOrder(x), 3:1)
    expect_null(rowDendro(x))
    expect_null(colDendro(x))
    expect_silent(validObject(x))
})

test_that("validity rejects corrupt order state", {
    x <- make_heatmap()
    x@rowOrder <- rep(1L, nrow(x))
    expect_error(validObject(x), "row order")
})
