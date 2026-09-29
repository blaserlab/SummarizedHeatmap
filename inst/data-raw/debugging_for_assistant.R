devtools::load_all()
make_test_hm <- function() {
  set.seed(2026)
  mat <- matrix(rnorm(48),
                nrow = 8,
                dimnames = list(paste0("gene", seq_len(8)), paste0("sample", seq_len(6))))
  row_info <- S4Vectors::DataFrame(feature_class = rep(c("marker", "background"), each = 4),
                                   row.names = rownames(mat))
  column_info <- S4Vectors::DataFrame(
    condition = rep(c("control", "treated"), each = 3),
    batch = rep(c("batch 1", "batch 2"), 3),
    row.names = colnames(mat)
  )

  hm <- SummarizedHeatmap(mat, rowData = row_info, colData = column_info)
  hm
}
hm <- make_test_hm()

plotHeatmap(hm)# not working, by default the annotations should be on the left
plotHeatmap(hm, rowDendroSide = "right")# works, though the underlying issue with the rowAnnotation is still there
plotHeatmap(hm, rowAnnotationSide = "right")# works
plotHeatmap(hm, colDendroSide = "right")# errs appropriately
plotHeatmap(hm, colDendroSide = "bottom")#
plotHeatmap(hm, colAnnotationSide = "bottom")#
plotHeatmap(hm, colAnnotationSide = NULL)# should delete the column annotation
plotHeatmap(hm, rowAnnotationSide = NULL)# somehow this puts the row annotation on the left and moves the axis text to the bottom
