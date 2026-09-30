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
* `SummarizedHeatmap()` now preserves the original assay name when
  constructing from a `SummarizedExperiment`, instead of always renaming it
  to `"matrix"`. An explicit `assay` argument is now validated even for a
  single-assay `SummarizedExperiment`, rather than being silently ignored.
* `plotHeatmap(collectGuides = FALSE)` no longer reserves an empty guide-area
  column: the `guide_area()` panel and its `guideWidth` column are now
  omitted from the layout entirely instead of being left blank.
* The `SummarizedHeatmap` validity method now requires the sole assay to be
  a numeric, non-complex matrix, matching the constructor's own check. This
  means `assay<-`/`assays<-` reject replacements that would leave a
  non-numeric, complex, or non-matrix assay.
* AI coding assistants (Posit Assistant, OpenAI Codex) were used during
  development to help with refactoring, testing, and documentation; all
  code was reviewed and is maintained by the package author.
