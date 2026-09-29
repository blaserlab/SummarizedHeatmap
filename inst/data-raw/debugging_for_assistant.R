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

plotHeatmap(hm)# close:  the axis title should be on the top instead of teh bottom for maximum compactness
plotHeatmap(hm, rowDendroSide = "right")# works
plotHeatmap(hm, rowAnnotationSide = "right")# works
plotHeatmap(hm, colDendroSide = "right")# errs appropriately
plotHeatmap(hm, colDendroSide = "bottom")# works
plotHeatmap(hm, colAnnotationSide = "bottom")# works
plotHeatmap(hm, colAnnotationSide = "bottom", colDendroSide = "bottom")# works
plotHeatmap(hm, colAnnotationSide = NULL)# the grid is off:  annotations are much too big relative to thhe main body
plotHeatmap(hm, rowAnnotationSide = NULL)# the grid is off:  there is a big gap between the dendrogram and the main body
plotHeatmap(hm, showRowDendro = FALSE)# should drop the reserved grid slot
