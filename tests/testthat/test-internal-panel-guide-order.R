# `.orderPanelsForGuides()` is the single place that decides the order
# `plotHeatmap()`'s components are handed to `patchwork::wrap_plots()`,
# which in turn determines collected-guide order (see the comment above
# `.PANEL_GUIDE_ORDER` in R/plot-heatmap.R). These tests exercise our own
# ordering helper directly rather than patchwork's undocumented guide-
# collection internals.

test_that(".orderPanelsForGuides() sorts a scrambled list into the canonical order", {
    panels <- list(D = "row_data", B = "col_data", M = "guide", A = "col_dendro", E = "main", C = "row_dendro")
    ordered <- .orderPanelsForGuides(panels)
    expect_identical(names(ordered), c("A", "B", "C", "D", "E", "M"))
    expect_identical(unname(unlist(ordered)), c("col_dendro", "col_data", "row_dendro", "row_data", "main", "guide"))
})

test_that(".orderPanelsForGuides() drops NULL (omitted) panels while preserving order of the rest", {
    panels <- list(A = NULL, B = "col_data", C = NULL, D = "row_data", E = "main", M = "guide")
    ordered <- .orderPanelsForGuides(panels)
    expect_identical(names(ordered), c("B", "D", "E", "M"))
})

test_that(".orderPanelsForGuides() is already-in-order input identity, and handles a single panel", {
    panels <- list(A = "col_dendro", B = "col_data", C = "row_dendro", D = "row_data", E = "main", M = "guide")
    expect_identical(.orderPanelsForGuides(panels), panels)

    expect_identical(.orderPanelsForGuides(list(E = "main")), list(E = "main"))
})

test_that(".orderPanelsForGuides() rejects names outside the recognised panel letters", {
    expect_error(.orderPanelsForGuides(list(Z = "unexpected")))
})
