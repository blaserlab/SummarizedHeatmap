#' Replace the assay of a SummarizedHeatmap
#'
#' Assay replacement follows `SummarizedExperiment` semantics: dimensions and
#' dimnames must be preserved. Because a stored dendrogram and its implied
#' order are only valid for the assay values they were computed from,
#' replacing the assay never silently reclusters. Instead, whenever a
#' dendrogram is currently stored for an axis, it is cleared and that axis's
#' order resets to its natural order; when no dendrogram is stored (e.g. a
#' manually-supplied or default order), the existing order is left
#' untouched. The same rule is applied independently to rows and columns.
#' Call `clusterRows()`, `clusterCols()`, or `clusterHeatmap()` afterwards to
#' recompute clustering against the new values.
#'
#' @param x A SummarizedHeatmap object.
#' @param i Optional name or index of the assay to replace. Since a
#'   `SummarizedHeatmap` always holds exactly one assay, this must (when
#'   supplied) identify that single assay.
#' @param withDimnames A logical value indicating whether `value`'s
#'   dimnames must be respected. Passed through to the
#'   `SummarizedExperiment` method.
#' @param ... Additional arguments passed to the `SummarizedExperiment`
#'   method.
#' @param value A numeric matrix (or other object accepted by the
#'   `SummarizedExperiment` method) with the same dimensions and dimnames as
#'   the current assay.
#' @return An updated, valid SummarizedHeatmap object.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' x <- SummarizedHeatmap(mat)
#' assay(x) <- mat * 2
#' rowDendro(x)
#' @importFrom SummarizedExperiment assay assay<-
#' @name assay-set-SummarizedHeatmap
NULL

#' @rdname assay-set-SummarizedHeatmap
#' @export
methods::setMethod(
    "assay<-", signature(x = "SummarizedHeatmap", i = "missing"),
    function(x, i, ..., value) {
        y <- methods::callNextMethod()
        .invalidateStaleClustering(y)
    }
)

#' @rdname assay-set-SummarizedHeatmap
#' @export
methods::setMethod(
    "assay<-", signature(x = "SummarizedHeatmap", i = "numeric"),
    function(x, i, ..., value) {
        y <- methods::callNextMethod()
        .invalidateStaleClustering(y)
    }
)

#' @rdname assay-set-SummarizedHeatmap
#' @export
methods::setMethod(
    "assay<-", signature(x = "SummarizedHeatmap", i = "character"),
    function(x, i, ..., value) {
        y <- methods::callNextMethod()
        .invalidateStaleClustering(y)
    }
)

#' Replace the assays of a SummarizedHeatmap
#'
#' @inherit assay-set-SummarizedHeatmap description
#' @param x A SummarizedHeatmap object.
#' @param withDimnames A logical value indicating whether `value`'s
#'   dimnames must be respected. Passed through to the
#'   `SummarizedExperiment` method.
#' @param ... Additional arguments passed to the `SummarizedExperiment`
#'   method.
#' @param value A list or `SimpleList` of exactly one assay, with the same
#'   dimensions and dimnames as the current assay.
#' @return An updated, valid SummarizedHeatmap object.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' x <- SummarizedHeatmap(mat)
#' assays(x) <- list(matrix = mat * 2)
#' rowDendro(x)
#' @importFrom SummarizedExperiment assays assays<-
#' @importClassesFrom S4Vectors SimpleList
#' @name assays-set-SummarizedHeatmap
NULL

#' @rdname assays-set-SummarizedHeatmap
#' @export
methods::setMethod(
    "assays<-", signature(x = "SummarizedHeatmap", value = "list"),
    function(x, ..., value) {
        y <- methods::callNextMethod()
        .invalidateStaleClustering(y)
    }
)

#' @rdname assays-set-SummarizedHeatmap
#' @export
methods::setMethod(
    "assays<-", signature(x = "SummarizedHeatmap", value = "SimpleList"),
    function(x, ..., value) {
        y <- methods::callNextMethod()
        .invalidateStaleClustering(y)
    }
)

#' Invalidate cluster-derived state after an assay replacement
#'
#' Clears the row/column dendrogram and resets the corresponding order to
#' natural order whenever a dendrogram is currently stored, since a
#' dendrogram (and the order it implies) is only valid for the assay values
#' it was computed from. When no dendrogram is stored, the existing order
#' (e.g. manually supplied) is preserved, since it does not depend on assay
#' values. Never reclusters.
#'
#' @param x A SummarizedHeatmap object, already updated with new assay
#'   values.
#' @return `x` with stale cluster-derived state cleared.
#' @noRd
.invalidateStaleClustering <- function(x) {
    if (!is.null(rowDendro(x))) {
        x <- .clearRowClustering(x)
    }
    if (!is.null(colDendro(x))) {
        x <- .clearColClustering(x)
    }
    methods::validObject(x)
    x
}
