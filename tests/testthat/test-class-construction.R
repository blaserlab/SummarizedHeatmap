test_that("matrix input behaves as before", {
    mat <- matrix(1:12, 4, dimnames = list(letters[1:4], LETTERS[1:3]))
    x <- SummarizedHeatmap(mat)
    expect_s4_class(x, "SummarizedHeatmap")
    expect_identical(SummarizedExperiment::assay(x), mat)
    expect_length(SummarizedExperiment::assays(x), 1L)
})

test_that("a SummarizedExperiment with a single assay is used automatically", {
    mat <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
    se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat))
    x <- SummarizedHeatmap(se)
    expect_s4_class(x, "SummarizedHeatmap")
    expect_identical(SummarizedExperiment::assay(x), mat)
    expect_length(SummarizedExperiment::assays(x), 1L)
    expect_silent(validObject(x))
})

test_that("a multi-assay SummarizedExperiment requires 'assay' by name or index", {
    mat1 <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
    mat2 <- mat1 * 2
    se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat1, scaled = mat2))

    expect_error(SummarizedHeatmap(se), "multiple assays")

    by_name <- SummarizedHeatmap(se, assay = "scaled")
    expect_identical(SummarizedExperiment::assay(by_name), mat2)

    by_index <- SummarizedHeatmap(se, assay = 2L)
    expect_identical(SummarizedExperiment::assay(by_index), mat2)
})

test_that("invalid 'assay' selections are rejected with clear errors", {
    mat1 <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
    mat2 <- mat1 * 2
    se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat1, scaled = mat2))

    expect_error(SummarizedHeatmap(se, assay = "bogus"), "must be one of")
    expect_error(SummarizedHeatmap(se, assay = 5L), "between 1 and 2")
    expect_error(SummarizedHeatmap(se, assay = c(1L, 2L)), "single")
    expect_error(SummarizedHeatmap(se, assay = TRUE), "single assay name or index")
})

test_that("rowData, colData, and metadata are preserved from a SummarizedExperiment", {
    mat <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
    rd <- S4Vectors::DataFrame(type = rep(c("gene", "control"), 3), row.names = rownames(mat))
    cd <- S4Vectors::DataFrame(group = rep(c("A", "B"), 2), row.names = colnames(mat))
    se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = mat), rowData = rd, colData = cd, metadata = list(note = "hi")
    )

    x <- SummarizedHeatmap(se)
    expect_identical(as.data.frame(SummarizedExperiment::rowData(x)), as.data.frame(rd))
    expect_identical(as.data.frame(SummarizedExperiment::colData(x)), as.data.frame(cd))
    expect_identical(S4Vectors::metadata(x), list(note = "hi"))
})

test_that("validity rejects a SummarizedHeatmap constructed with more than one assay", {
    mat1 <- matrix(rnorm(24), 6, dimnames = list(paste0("f", 1:6), paste0("s", 1:4)))
    mat2 <- mat1 * 2
    se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat1, scaled = mat2))

    expect_error(
        methods::new(
            "SummarizedHeatmap", se,
            rowDendro = NULL, colDendro = NULL,
            rowOrder = 1:6, colOrder = 1:4
        ),
        "exactly one assay"
    )
})
