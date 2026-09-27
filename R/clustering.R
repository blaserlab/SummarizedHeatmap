#' Cluster rows
#'
#' Compute a hierarchical clustering across assay rows and update the stored
#' row dendrogram and display order. Clustering is deterministic for a fixed
#' assay and set of methods.
#'
#' @param x A SummarizedHeatmap object.
#' @param distMethod Distance method passed to [stats::dist()].
#' @param hclustMethod Linkage method passed to [stats::hclust()].
#' @return The updated SummarizedHeatmap object.
#' @importFrom stats dist hclust
#' @importFrom stats as.dendrogram order.dendrogram
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' clusterRows(SummarizedHeatmap(mat))
#' @export
clusterRows <- function(x, distMethod = "euclidean", hclustMethod = "average") {
    .checkSummarizedHeatmap(x)
    result <- .cluster_axis(x, 1L, distMethod, hclustMethod)
    x@rowDendro <- result$dendrogram
    x@rowOrder <- as.integer(result$order)
    methods::validObject(x)
    x
}

#' Cluster columns
#'
#' Compute a hierarchical clustering across assay columns and update the
#' stored column dendrogram and display order.
#'
#' @inheritParams clusterRows
#' @return The updated SummarizedHeatmap object.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' clusterCols(SummarizedHeatmap(mat))
#' @export
clusterCols <- function(x, distMethod = "euclidean", hclustMethod = "average") {
    .checkSummarizedHeatmap(x)
    result <- .cluster_axis(x, 2L, distMethod, hclustMethod)
    x@colDendro <- result$dendrogram
    x@colOrder <- as.integer(result$order)
    methods::validObject(x)
    x
}

#' Cluster rows and columns
#'
#' @inheritParams clusterRows
#' @return The updated SummarizedHeatmap object.
#' @examples
#' mat <- matrix(rnorm(12), 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' clusterHeatmap(SummarizedHeatmap(mat))
#' @export
clusterHeatmap <- function(x, distMethod = "euclidean", hclustMethod = "average") {
    clusterCols(clusterRows(x, distMethod, hclustMethod), distMethod, hclustMethod)
}
