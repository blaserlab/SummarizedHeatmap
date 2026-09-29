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

plotHeatmap(hm)# expand guide area to the full height of the plot

plot_hm <- function() {

  design <-
    "
  ##AG
  ##BG
  CDEG
  "
  pColDendro <- plotColDendro(hm)
  pColData <- plotColData(hm)
  pRowDendro <- plotRowDendro(hm)
  pRowData <- free(plotRowData(hm, side = "left"), type = "space")
  pMain <- plotHeatmapMain(hm)

  wrap_plots(A = pColDendro,
             B = pColData,
             C = pRowDendro,
             D = pRowData,
             E = pMain,
             G = guide_area(),
             design = design,
             heights = c(1, 1, 8),
             widths = c(1, 1, 8, 2),
             guides = "collect"
             )
}
plot_hm()# add another block to the vignette demonstrating more of the versatility of the approach used here.  In addition to the plot_hm function, take advantage of the palette parameters available in the annotation data plotters.
