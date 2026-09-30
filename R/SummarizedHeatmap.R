#' Create a SummarizedHeatmap
#'
#' Create a heatmap representation from a numeric matrix or from a single
#' assay of a [SummarizedExperiment-class] object. Rows and columns are
#' clustered by default when their sizes permit; stored orders are integer
#' indices into the assay dimensions. Missing dimension names are filled with
#' stable sequential identifiers when `x` is a matrix.
#'
#' A `SummarizedHeatmap` always holds exactly one assay. When `x` is a
#' `SummarizedExperiment` with a single assay, that assay is used
#' automatically. When it has multiple assays, `assay` must be supplied to
#' select one by name or position. Either way, the selected assay's original
#' name is preserved in the resulting `SummarizedHeatmap`; `rowData`,
#' `colData`, and `metadata` are carried over from `x` unchanged.
#'
#' @param x A numeric matrix, or a [SummarizedExperiment-class] object.
#' @param assay When `x` is a `SummarizedExperiment`, the name or integer
#'   index of the assay to use. Required when `x` has more than one assay;
#'   optional but still validated when `x` has exactly one. Unused when `x`
#'   is a matrix. The selected assay's original name is preserved in the
#'   resulting `SummarizedHeatmap` (falling back to `"matrix"` when the
#'   source assay is unnamed).
#' @param rowOrder Optional manual row order, as integer indices or row names.
#'   Supplying it leaves the row dendrogram unset and takes precedence over
#'   `clusterRows`.
#' @param colOrder Optional manual column order, as integer indices or column
#'   names. Supplying it leaves the column dendrogram unset and takes
#'   precedence over `clusterCols`.
#' @param clusterRows Whether to compute a row dendrogram and clustering
#'   order. Ignored (with no dendrogram computed) when `rowOrder` is
#'   supplied. When `FALSE` and `rowOrder` is not supplied, rows keep their
#'   natural order and no dendrogram is stored.
#' @param clusterCols Whether to compute a column dendrogram and clustering
#'   order. Ignored (with no dendrogram computed) when `colOrder` is
#'   supplied. When `FALSE` and `colOrder` is not supplied, columns keep
#'   their natural order and no dendrogram is stored.
#' @param distMethod Distance method passed to [stats::dist()].
#' @param hclustMethod Linkage method passed to [stats::hclust()]. Defaults to
#'   average linkage, matching the original constructor.
#' @param ... Additional arguments passed to
#'   [SummarizedExperiment::SummarizedExperiment()], such as `rowData` and
#'   `colData`. Only used when `x` is a matrix; ignored when `x` is a
#'   `SummarizedExperiment`, whose own annotations are preserved instead.
#' @return A valid `SummarizedHeatmap` object.
#' @examples
#' mat <- matrix(rnorm(24),
#'     nrow = 6,
#'     dimnames = list(paste0("feature", 1:6), paste0("sample", 1:4))
#' )
#' x <- SummarizedHeatmap(mat)
#' validObject(x)
#'
#' se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat))
#' y <- SummarizedHeatmap(se)
#' validObject(y)
#' SummarizedExperiment::assayNames(y) # "counts", preserved from se
#' @export
SummarizedHeatmap <- function(x, assay = NULL, rowOrder = NULL, colOrder = NULL,
                              clusterRows = TRUE, clusterCols = TRUE,
                              distMethod = "euclidean", hclustMethod = "average", ...) {
    if (methods::is(x, "SummarizedExperiment")) {
        assay_name <- .select_assay_name(x, assay)
        mat <- .validate_heatmap_matrix(SummarizedExperiment::assay(x, assay_name),
            what = "the selected assay", fillDefaults = FALSE
        )
        se <- SummarizedExperiment::SummarizedExperiment(
            assays = stats::setNames(list(mat), assay_name),
            rowData = SummarizedExperiment::rowData(x),
            colData = SummarizedExperiment::colData(x),
            metadata = S4Vectors::metadata(x)
        )
    } else {
        mat <- .validate_heatmap_matrix(x)
        se <- SummarizedExperiment::SummarizedExperiment(assays = list(matrix = mat), ...)
    }

    object <- methods::new(
        "SummarizedHeatmap", se,
        rowDendro = NULL, colDendro = NULL,
        rowOrder = seq_len(nrow(mat)), colOrder = seq_len(ncol(mat))
    )
    if (!is.null(rowOrder)) {
        # No dendrogram implied by a manually-supplied order.
        object <- .setRowClustering(object, NULL, .normalize_order(object, rowOrder, 1L, "rowOrder"))
    } else if (isTRUE(clusterRows)) {
        object <- SummarizedHeatmap::clusterRows(object, distMethod = distMethod, hclustMethod = hclustMethod)
    }
    if (!is.null(colOrder)) {
        object <- .setColClustering(object, NULL, .normalize_order(object, colOrder, 2L, "colOrder"))
    } else if (isTRUE(clusterCols)) {
        object <- SummarizedHeatmap::clusterCols(object, distMethod = distMethod, hclustMethod = hclustMethod)
    }
    methods::validObject(object)
    object
}

#' Validate and normalize a matrix intended for a SummarizedHeatmap assay
#'
#' @param mat A candidate matrix.
#' @param what Label for the argument being validated, used in error messages.
#' @param fillDefaults Whether to fill missing dimnames with stable sequential
#'   identifiers. Used for matrix input; left `FALSE` for assays extracted
#'   from a `SummarizedExperiment`, whose dimnames (including `NULL`) come
#'   from that object and must stay in sync with its `rowData`/`colData`.
#' @return The validated matrix, with dimnames filled in when requested.
#' @noRd
.validate_heatmap_matrix <- function(mat, what = "'x'", fillDefaults = TRUE) {
    if (!is.matrix(mat) || !is.numeric(mat) || is.complex(mat)) {
        stop(sprintf("%s must be a numeric matrix", what), call. = FALSE)
    }
    if (fillDefaults) {
        if (is.null(rownames(mat))) {
            rownames(mat) <- if (nrow(mat)) paste0("row", seq_len(nrow(mat))) else character()
        }
        if (is.null(colnames(mat))) {
            colnames(mat) <- if (ncol(mat)) paste0("column", seq_len(ncol(mat))) else character()
        }
    }
    if (!is.null(rownames(mat)) &&
        (anyNA(rownames(mat)) || any(!nzchar(rownames(mat))) || anyDuplicated(rownames(mat)))) {
        stop("matrix row names must be unique, non-missing, and non-empty", call. = FALSE)
    }
    if (!is.null(colnames(mat)) &&
        (anyNA(colnames(mat)) || any(!nzchar(colnames(mat))) || anyDuplicated(colnames(mat)))) {
        stop("matrix column names must be unique, non-missing, and non-empty", call. = FALSE)
    }
    mat
}

#' Resolve the name of the assay to use from a SummarizedExperiment
#'
#' Validates `assay` (a name, an index, or `NULL`) against the assays of `se`
#' and resolves it to a single assay name, which is then used to name the
#' resulting `SummarizedHeatmap`'s assay. When `se` has a single assay,
#' `assay` is optional but, if supplied, is still validated against that
#' assay rather than ignored.
#'
#' @param se A `SummarizedExperiment` object.
#' @param assay `NULL`, or the name/index of the assay to select.
#' @return A single string: the name of the selected assay. Unnamed assays
#'   (where `assayNames()` reports `NA` or `""`) resolve to `"matrix"`, the
#'   same default used for matrix input.
#' @noRd
.select_assay_name <- function(se, assay) {
    n_assays <- length(SummarizedExperiment::assays(se))
    if (n_assays == 0L) {
        stop("'x' must contain at least one assay", call. = FALSE)
    }
    assay_names <- SummarizedExperiment::assayNames(se)
    if (n_assays == 1L) {
        if (!is.null(assay)) {
            .validate_assay_selector(assay, assay_names, n_assays)
        }
        return(.default_assay_name(assay_names, 1L))
    }
    if (is.null(assay)) {
        stop("'x' has multiple assays; specify 'assay' by name or index", call. = FALSE)
    }
    idx <- .validate_assay_selector(assay, assay_names, n_assays)
    .default_assay_name(assay_names, idx)
}

#' Validate an 'assay' selector and resolve it to a positional index
#'
#' @param assay The name or index supplied by the caller.
#' @param assay_names The result of [SummarizedExperiment::assayNames()] on
#'   the source object.
#' @param n_assays The number of assays in the source object.
#' @return The integer position of the selected assay.
#' @noRd
.validate_assay_selector <- function(assay, assay_names, n_assays) {
    if (is.character(assay)) {
        if (length(assay) != 1L || anyNA(assay) || !nzchar(assay)) {
            stop("'assay' must be a single, non-empty assay name", call. = FALSE)
        }
        if (!(assay %in% assay_names)) {
            stop(sprintf(
                "'assay' must be one of the available assay names: %s",
                paste(assay_names, collapse = ", ")
            ), call. = FALSE)
        }
        return(match(assay, assay_names))
    }
    if (is.numeric(assay)) {
        if (length(assay) != 1L || is.na(assay) || assay != as.integer(assay) ||
            assay < 1L || assay > n_assays) {
            stop(sprintf("'assay' must be a single integer between 1 and %d", n_assays), call. = FALSE)
        }
        return(as.integer(assay))
    }
    stop("'assay' must be a single assay name or index", call. = FALSE)
}

#' Default an assay's name when it is missing, matching matrix-input behavior
#'
#' @param assay_names The result of [SummarizedExperiment::assayNames()], or
#'   `NULL` when the source object has no assay names at all.
#' @param idx The position of the assay whose name is needed.
#' @return The assay's name, or `"matrix"` when that name is `NA` or empty.
#' @noRd
.default_assay_name <- function(assay_names, idx) {
    name <- if (is.null(assay_names)) NA_character_ else assay_names[idx]
    if (is.na(name) || !nzchar(name)) "matrix" else name
}
