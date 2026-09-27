make_heatmap <- function(nrow = 6L, ncol = 4L) {
    mat <- matrix(seq_len(nrow * ncol),
        nrow = nrow,
        dimnames = list(paste0("feature", seq_len(nrow)), paste0("sample", seq_len(ncol)))
    )
    x <- SummarizedHeatmap(mat)
    if (nrow) {
        SummarizedExperiment::rowData(x)$feature_type <- rep(c("gene", "control"), length.out = nrow)
        SummarizedExperiment::rowData(x)$feature_type2 <- rep(c("one", "two"), length.out = nrow)
    }
    if (ncol) {
        SummarizedExperiment::colData(x)$condition <- rep(c("A", "B"), length.out = ncol)
    }
    x
}
