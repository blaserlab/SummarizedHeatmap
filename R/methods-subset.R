#' Subset a SummarizedHeatmap
#'
#' Subsetting follows SummarizedExperiment semantics. If row membership or
#' order changes, the row dendrogram is cleared and row order resets to the
#' natural order of the subset. The same rule applies independently to
#' columns. Clustering for an unchanged dimension is preserved. Subsetting
#' never reclusters implicitly; call `clusterRows()`, `clusterCols()`, or
#' `clusterHeatmap()` when new clustering is desired.
#'
#' @param x A SummarizedHeatmap object.
#' @param i Row indices, names, or logical selector.
#' @param j Column indices, names, or logical selector.
#' @param ... Additional subsetting arguments.
#' @param drop Whether to drop dimensions. Defaults to `FALSE` to preserve the
#'   SummarizedHeatmap class.
#' @return A valid SummarizedHeatmap object.
#' @examples
#' mat <- matrix(rnorm(20), 5, dimnames = list(letters[1:5], LETTERS[1:4]))
#' x <- SummarizedHeatmap(mat)
#' x[1:3, 1:2]
#' @export
methods::setMethod("[", "SummarizedHeatmap", function(x, i, j, ..., drop = FALSE) {
    old_rows <- rownames(x)
    old_cols <- colnames(x)
    y <- methods::callNextMethod()
    if (!identical(rownames(y), old_rows)) {
        y@rowDendro <- NULL
        y@rowOrder <- seq_len(nrow(y))
    }
    if (!identical(colnames(y), old_cols)) {
        y@colDendro <- NULL
        y@colOrder <- seq_len(ncol(y))
    }
    methods::validObject(y)
    y
})
