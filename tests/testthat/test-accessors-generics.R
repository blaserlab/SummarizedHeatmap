test_that("accessors are S4 generics with a SummarizedHeatmap method", {
    for (name in c("rowOrder", "colOrder", "rowDendro", "colDendro")) {
        expect_true(isGeneric(name))
        expect_true(existsMethod(name, "SummarizedHeatmap"))
    }
})

test_that("accessor generics still reject non-SummarizedHeatmap input", {
    expect_error(rowOrder(1:4), "unable to find an inherited method")
    expect_error(colOrder(1:4), "unable to find an inherited method")
    expect_error(rowDendro(1:4), "unable to find an inherited method")
    expect_error(colDendro(1:4), "unable to find an inherited method")
})
