#' Access stored row dendrogram
#' @param x A SummarizedHeatmap object.
#' @return A dendrogram, or `NULL` if rows are not clustered.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' rowDendro(SummarizedHeatmap(mat))
#' @export
rowDendro <- function(x) {
    .checkSummarizedHeatmap(x)
    x@rowDendro
}

#' Access stored column dendrogram
#' @param x A SummarizedHeatmap object.
#' @return A dendrogram, or `NULL` if columns are not clustered.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' colDendro(SummarizedHeatmap(mat))
#' @export
colDendro <- function(x) {
    .checkSummarizedHeatmap(x)
    x@colDendro
}

#' Access row display order
#' @param x A SummarizedHeatmap object.
#' @return Integer row indices in display order.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' rowOrder(SummarizedHeatmap(mat))
#' @export
rowOrder <- function(x) {
    .checkSummarizedHeatmap(x)
    x@rowOrder
}

#' Access column display order
#' @param x A SummarizedHeatmap object.
#' @return Integer column indices in display order.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' colOrder(SummarizedHeatmap(mat))
#' @export
colOrder <- function(x) {
    .checkSummarizedHeatmap(x)
    x@colOrder
}
