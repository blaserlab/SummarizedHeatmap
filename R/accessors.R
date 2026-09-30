#' Access stored row dendrogram
#' @param x A SummarizedHeatmap object.
#' @return A dendrogram, or `NULL` if rows are not clustered.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' rowDendro(SummarizedHeatmap(mat))
#' @export
setGeneric("rowDendro", function(x) standardGeneric("rowDendro"))

#' @rdname rowDendro
setMethod("rowDendro", "SummarizedHeatmap", function(x) {
    x@rowDendro
})

#' Access stored column dendrogram
#' @param x A SummarizedHeatmap object.
#' @return A dendrogram, or `NULL` if columns are not clustered.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' colDendro(SummarizedHeatmap(mat))
#' @export
setGeneric("colDendro", function(x) standardGeneric("colDendro"))

#' @rdname colDendro
setMethod("colDendro", "SummarizedHeatmap", function(x) {
    x@colDendro
})

#' Access row display order
#' @param x A SummarizedHeatmap object.
#' @return Integer row indices in display order.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' rowOrder(SummarizedHeatmap(mat))
#' @export
setGeneric("rowOrder", function(x) standardGeneric("rowOrder"))

#' @rdname rowOrder
setMethod("rowOrder", "SummarizedHeatmap", function(x) {
    x@rowOrder
})

#' Access column display order
#' @param x A SummarizedHeatmap object.
#' @return Integer column indices in display order.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' colOrder(SummarizedHeatmap(mat))
#' @export
setGeneric("colOrder", function(x) standardGeneric("colOrder"))

#' @rdname colOrder
setMethod("colOrder", "SummarizedHeatmap", function(x) {
    x@colOrder
})
