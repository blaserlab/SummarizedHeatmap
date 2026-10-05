# SummarizedHeatmap

`SummarizedHeatmap` represents a clustered, annotated assay as a
`SummarizedExperiment` subclass. It keeps assay values, `rowData`, `colData`,
display order, and clustering together.  Plotting functions return ordinary 
ggplot2 graphics that users can compose and customize with patchwork.

The workflow is: **SummarizedExperiment integration → persistent clustering
and annotation state → ordinary ggplot components → patchwork composition.**

Install the Bioconductor release with `BiocManager::install("SummarizedHeatmap")`.

```r
library(SummarizedHeatmap)

mat <- matrix(rnorm(40), nrow = 8,
  dimnames = list(paste0("gene", 1:8), paste0("sample", 1:5)))
x <- SummarizedHeatmap(mat)
SummarizedExperiment::colData(x) <- S4Vectors::DataFrame(
  condition = rep(c("control", "treated"), length.out = 5),
  row.names = colnames(mat)
)

main <- plotHeatmapMain(x)
annotation <- plotColData(x, vars = "condition")
plotHeatmap(x)
```

Use `plotRowData()`, `plotRowDendro()`, and `plotColDendro()` for individual
components. Each component can be extended with ggplot2 and assembled with
patchwork; `clusterRows()`, `clusterCols()`, and `clusterHeatmap()` explicitly
refresh stored clustering after assay changes.

See the package vignette for custom layouts, subsetting behavior, and the
relationship to other heatmap approaches.
