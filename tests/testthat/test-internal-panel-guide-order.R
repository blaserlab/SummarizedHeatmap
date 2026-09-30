# `.orderPanelsForGuides()` is the single place that decides the order
# `plotHeatmap()`'s components are handed to `patchwork::wrap_plots()`,
# which in turn determines collected-guide order (see the comment above
# `.PANEL_GUIDE_ORDER` in R/plot-heatmap.R). These tests exercise our own
# ordering helper directly rather than patchwork's undocumented guide-
# collection internals.

test_that(".orderPanelsForGuides() sorts a scrambled list into the canonical order", {
    panels <- list(D = "row_dendro", B = "col_dendro", M = "guide", A = "main", E = "row_data", C = "col_data")
    ordered <- .orderPanelsForGuides(panels)
    expect_identical(names(ordered), c("A", "B", "C", "D", "E", "M"))
    expect_identical(unname(unlist(ordered)), c("main", "col_dendro", "col_data", "row_dendro", "row_data", "guide"))
})

test_that(".orderPanelsForGuides() drops NULL (omitted) panels while preserving order of the rest", {
    panels <- list(A = "main", B = NULL, C = "col_data", D = NULL, E = "row_data", M = "guide")
    ordered <- .orderPanelsForGuides(panels)
    expect_identical(names(ordered), c("A", "C", "E", "M"))
})

test_that(".orderPanelsForGuides() is already-in-order input identity, and handles a single panel", {
    panels <- list(A = "main", B = "col_dendro", C = "col_data", D = "row_dendro", E = "row_data", M = "guide")
    expect_identical(.orderPanelsForGuides(panels), panels)

    expect_identical(.orderPanelsForGuides(list(A = "main")), list(A = "main"))
})

test_that(".orderPanelsForGuides() rejects names outside the recognised panel letters", {
    expect_error(.orderPanelsForGuides(list(Z = "unexpected")))
})
