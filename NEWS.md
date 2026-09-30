# SummarizedHeatmap 0.99.0

* Initial Bioconductor development release.
* Introduces the `SummarizedExperiment`-derived heatmap representation,
  explicit clustering helpers, composable ggplot2 components, and patchwork
  assembly.
* `assay<-`/`assays<-` on a `SummarizedHeatmap` now invalidate stale
  cluster-derived state: a stored row/column dendrogram is cleared and that
  axis's order reset to natural order, while axes without a stored
  dendrogram (e.g. a manual order) are left untouched. Assay replacement
  never reclusters automatically.
* AI coding assistants (Posit Assistant, OpenAI Codex) were used during
  development to help with refactoring, testing, and documentation; all
  code was reviewed and is maintained by the package author.
