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

plotHeatmap(hm, rowDendroSide = "right")# not working:  flips row dendrogram instead of moving it
plotHeatmap(hm, rowAnnotationSide = "right")# not working: doesn't appear to do anything
plotHeatmap(hm, colDendroSide = "right")# should err:  colDendro should never be on the right or left
plotHeatmap(hm, colDendroSide = "bottom")# not working, similar to row dendro arg
