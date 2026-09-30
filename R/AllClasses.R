methods::setOldClass("dendrogram")
methods::setClassUnion("dendrogramOrNULL", c("dendrogram", "NULL"))

#' SummarizedHeatmap class
#'
#' A single assay and its row and column annotations, display orders, and
#' optional hierarchical clusterings. The class extends
#' [SummarizedExperiment-class] but is restricted to exactly one assay; use
#' [SummarizedHeatmap()] to build one from a matrix or from a
#' `SummarizedExperiment` (selecting an assay when it holds more than one).
#'
#' @slot rowDendro Row dendrogram, or `NULL` when rows are not clustered.
#' @slot colDendro Column dendrogram, or `NULL` when columns are not clustered.
#' @slot rowOrder Integer indices giving row display order.
#' @slot colOrder Integer indices giving column display order.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' show(SummarizedHeatmap(mat))
#' @importFrom S4Vectors setValidity2
#' @exportClass SummarizedHeatmap
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
methods::setClass(
    "SummarizedHeatmap",
    contains = "SummarizedExperiment",
    slots = c(
        rowDendro = "dendrogramOrNULL",
        colDendro = "dendrogramOrNULL",
        rowOrder = "integer",
        colOrder = "integer"
    ),
    prototype = list(
        rowDendro = NULL,
        colDendro = NULL,
        rowOrder = integer(),
        colOrder = integer()
    )
)

S4Vectors::setValidity2("SummarizedHeatmap", function(object) {
    errors <- new.env(parent = emptyenv())
    errors$messages <- character()
    nr <- nrow(object)
    nc <- ncol(object)

    check_order <- function(order, n, dimension) {
        if (length(order) != n) {
            errors$messages <- c(errors$messages, sprintf("%s order length must equal %s", dimension, n))
            return(invisible(NULL))
        }
        if (!is.integer(order) || anyNA(order) || anyDuplicated(order) ||
            !identical(sort(order), seq_len(n))) {
            errors$messages <- c(errors$messages, sprintf("%s order must be a permutation of 1:%s", dimension, n))
        }
    }

    check_names <- function(nms, n, dimension) {
        if (is.null(nms)) {
            # A zero-length dimension always reports NULL names in base R,
            # even when explicitly assigned character(0); only require names
            # to be present when there is at least one row/column to name.
            if (n > 0L) {
                errors$messages <- c(errors$messages, sprintf("%s names must not be NULL", dimension))
            }
            return(invisible(NULL))
        }
        if (anyNA(nms)) {
            errors$messages <- c(errors$messages, sprintf("%s names must not contain NA", dimension))
        }
        if (any(!nzchar(nms))) {
            errors$messages <- c(errors$messages, sprintf("%s names must not contain empty strings", dimension))
        }
        if (anyDuplicated(nms)) {
            errors$messages <- c(errors$messages, sprintf("%s names must not contain duplicates", dimension))
        }
    }

    check_dendrogram <- function(dendro, order, n, dimension, ids) {
        if (is.null(dendro)) {
            return(invisible(NULL))
        }
        if (n < 2L) {
            errors$messages <- c(errors$messages, sprintf("a %s dendrogram requires at least two elements", dimension))
            return(invisible(NULL))
        }
        dendro_order <- tryCatch(as.integer(order.dendrogram(dendro)), error = function(e) integer())
        if (length(dendro_order) != n || anyNA(dendro_order) ||
            anyDuplicated(dendro_order) || !identical(sort(dendro_order), seq_len(n))) {
            errors$messages <- c(errors$messages, sprintf("the %s dendrogram is incompatible with the current dimensions", dimension))
            return(invisible(NULL))
        }
        labels <- as.character(base::labels(dendro))
        expected <- ids[order]
        if (length(labels) != n || !identical(labels, expected)) {
            errors$messages <- c(errors$messages, sprintf("the %s dendrogram labels do not match the stored order", dimension))
        }
    }

    n_assays <- length(SummarizedExperiment::assays(object))
    if (n_assays != 1L) {
        errors$messages <- c(errors$messages, sprintf("a SummarizedHeatmap must contain exactly one assay, not %d", n_assays))
    }

    check_order(object@rowOrder, nr, "row")
    check_order(object@colOrder, nc, "column")
    row_ids <- rownames(object)
    col_ids <- colnames(object)
    check_names(row_ids, nr, "row")
    check_names(col_ids, nc, "column")
    if (is.null(row_ids)) row_ids <- as.character(seq_len(nr))
    if (is.null(col_ids)) col_ids <- as.character(seq_len(nc))
    check_dendrogram(object@rowDendro, object@rowOrder, nr, "row", row_ids)
    check_dendrogram(object@colDendro, object@colOrder, nc, "column", col_ids)

    if (length(errors$messages)) errors$messages else TRUE
})

#' Display a concise summary of a SummarizedHeatmap
#'
#' @param object A SummarizedHeatmap object.
#' @return The method displays a summary and returns invisibly.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' show(SummarizedHeatmap(mat))
methods::setMethod("show", "SummarizedHeatmap", function(object) {
    cat(sprintf("SummarizedHeatmap: %d rows x %d columns\n", nrow(object), ncol(object)))
    cat(sprintf("  row clustering: %s\n", if (is.null(rowDendro(object))) "not stored" else "stored"))
    cat(sprintf("  column clustering: %s\n", if (is.null(colDendro(object))) "not stored" else "stored"))
    invisible(NULL)
})
