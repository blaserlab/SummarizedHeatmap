#' Create a SummarizedHeatmap
#'
#' Create a heatmap representation from a numeric matrix. Rows and columns are
#' clustered by default when their sizes permit; stored orders are integer
#' indices into the assay dimensions. Missing dimension names are filled with
#' stable sequential identifiers.
#'
#' @param mat A numeric matrix.
#' @param rowOrder Optional manual row order, as integer indices or row names.
#'   Supplying it leaves the row dendrogram unset.
#' @param colOrder Optional manual column order, as integer indices or column
#'   names. Supplying it leaves the column dendrogram unset.
#' @param distMethod Distance method passed to [stats::dist()].
#' @param hclustMethod Linkage method passed to [stats::hclust()]. Defaults to
#'   average linkage, matching the original constructor.
#' @param ... Additional arguments passed to
#'   [SummarizedExperiment::SummarizedExperiment()], such as `rowData` and
#'   `colData`.
#' @return A valid `SummarizedHeatmap` object.
#' @examples
#' mat <- matrix(rnorm(24),
#'     nrow = 6,
#'     dimnames = list(paste0("feature", 1:6), paste0("sample", 1:4))
#' )
#' x <- SummarizedHeatmap(mat)
#' validObject(x)
#' @export
SummarizedHeatmap <- function(mat, rowOrder = NULL, colOrder = NULL,
                              distMethod = "euclidean", hclustMethod = "average", ...) {
    if (!is.matrix(mat) || !is.numeric(mat) || is.complex(mat)) {
        stop("'mat' must be a numeric matrix", call. = FALSE)
    }
    if (is.null(rownames(mat))) {
        rownames(mat) <- if (nrow(mat)) paste0("row", seq_len(nrow(mat))) else character()
    }
    if (is.null(colnames(mat))) {
        colnames(mat) <- if (ncol(mat)) paste0("column", seq_len(ncol(mat))) else character()
    }
    if (anyNA(rownames(mat)) || any(!nzchar(rownames(mat))) || anyDuplicated(rownames(mat))) {
        stop("matrix row names must be unique, non-missing, and non-empty", call. = FALSE)
    }
    if (anyNA(colnames(mat)) || any(!nzchar(colnames(mat))) || anyDuplicated(colnames(mat))) {
        stop("matrix column names must be unique, non-missing, and non-empty", call. = FALSE)
    }

    se <- SummarizedExperiment::SummarizedExperiment(assays = list(matrix = mat), ...)
    object <- methods::new(
        "SummarizedHeatmap", se,
        rowDendro = NULL, colDendro = NULL,
        rowOrder = seq_len(nrow(mat)), colOrder = seq_len(ncol(mat))
    )
    if (is.null(rowOrder)) {
        object <- clusterRows(object, distMethod = distMethod, hclustMethod = hclustMethod)
    } else {
        object@rowOrder <- .normalize_order(object, rowOrder, 1L, "rowOrder")
    }
    if (is.null(colOrder)) {
        object <- clusterCols(object, distMethod = distMethod, hclustMethod = hclustMethod)
    } else {
        object@colOrder <- .normalize_order(object, colOrder, 2L, "colOrder")
    }
    methods::validObject(object)
    object
}
