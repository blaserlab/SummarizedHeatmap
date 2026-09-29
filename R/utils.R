utils::globalVariables(c("position", "label", "x", "y", "xend", "yend", "column", "value"))

.checkSummarizedHeatmap <- function(x) {
    if (!methods::is(x, "SummarizedHeatmap")) {
        stop("'x' must be a SummarizedHeatmap object", call. = FALSE)
    }
    invisible(x)
}

.axis_ids <- function(x, margin) {
    ids <- if (margin == 1L) rownames(x) else colnames(x)
    n <- if (margin == 1L) nrow(x) else ncol(x)
    if (is.null(ids)) as.character(seq_len(n)) else ids
}

.normalize_order <- function(x, order, margin, argument) {
    n <- if (margin == 1L) nrow(x) else ncol(x)
    ids <- .axis_ids(x, margin)
    if (is.character(order)) {
        if (anyNA(order) || anyDuplicated(order) || any(!order %in% ids)) {
            stop(sprintf("'%s' must contain unique valid dimension names", argument), call. = FALSE)
        }
        order <- match(order, ids)
    }
    if (!is.numeric(order) || length(order) != n || anyNA(order) ||
        any(order != as.integer(order)) || anyDuplicated(order) ||
        !identical(sort(as.integer(order)), seq_len(n))) {
        stop(sprintf("'%s' must be a permutation of 1:%d or the corresponding names", argument, n), call. = FALSE)
    }
    as.integer(order)
}

.cluster_axis <- function(x, margin, distMethod, hclustMethod) {
    n <- if (margin == 1L) nrow(x) else ncol(x)
    if (n < 2L || (margin == 1L && ncol(x) == 0L) ||
        (margin == 2L && nrow(x) == 0L)) {
        return(list(dendrogram = NULL, order = seq_len(n)))
    }
    values <- SummarizedExperiment::assay(x)
    if (margin == 2L) values <- t(values)
    hc <- hclust(dist(values, method = distMethod), method = hclustMethod)
    dendrogram <- as.dendrogram(hc)
    list(dendrogram = dendrogram, order = as.integer(order.dendrogram(dendrogram)))
}

.annotation_plots <- function(x, margin, vars, side, tileColor, palette, showNames, wrap = TRUE) {
    metadata <- if (margin == 1L) SummarizedExperiment::rowData(x) else SummarizedExperiment::colData(x)
    data <- as.data.frame(metadata)
    ids <- .axis_ids(x, margin)
    order <- if (margin == 1L) rowOrder(x) else colOrder(x)
    axis_name <- if (margin == 1L) "row" else "column"
    if (is.null(vars)) vars <- names(data)
    if (!is.character(vars) || anyNA(vars) || anyDuplicated(unname(vars))) {
        stop("'vars' must be a character vector of unique annotation names", call. = FALSE)
    }
    labels <- names(vars)
    if (is.null(labels)) labels <- unname(vars)
    missing <- setdiff(unname(vars), names(data))
    if (length(missing)) {
        stop(sprintf("unknown %s annotation variable(s): %s", axis_name, paste(missing, collapse = ", ")), call. = FALSE)
    }
    if (!length(vars)) {
        return(patchwork::plot_spacer())
    }

    data <- data[order, , drop = FALSE]
    axis_ids <- ids[order]
    plots <- lapply(seq_along(vars), function(i) {
        .annotation_plot(
            value = data[[unname(vars)[i]]],
            axis_ids = axis_ids,
            label = labels[i],
            side = side,
            tileColor = tileColor,
            palette = palette,
            showNames = showNames
        )
    })
    if (length(plots) == 1L) {
        return(plots[[1L]])
    }
    composite <- if (margin == 2L && side %in% c("top", "bottom")) {
        patchwork::wrap_plots(plots, ncol = 1L, guides = "auto")
    } else {
        patchwork::wrap_plots(plots, guides = "auto")
    }
    if (!wrap) {
        return(composite)
    }
    # `wrap_elements()` fixes the composite's internal panels into a single
    # opaque patch. Without it, combining the multi-variable result with
    # another plot via patchwork operators (`+`, `/`, `|`) would silently
    # flatten its plot list into the parent's, changing the effective panel
    # count and breaking layouts built with `plot_layout(heights = ...)` or
    # `plot_layout(design = ...)`. The trade-off is that a wrapped composite
    # can no longer be aligned to sibling panels' actual axis positions, so
    # `plotHeatmap()` calls `.annotation_plots()` directly with `wrap = FALSE`
    # to keep its column/row annotation strips pixel-aligned with the main
    # heatmap panel.
    patchwork::wrap_elements(panel = composite)
}

.annotation_plot <- function(value, axis_ids, label, side, tileColor, palette, showNames) {
    if (is.list(value) && !is.factor(value)) {
        stop("annotation values must be atomic vectors", call. = FALSE)
    }
    plot_data <- data.frame(
        position = factor(axis_ids, levels = axis_ids),
        value = value,
        label = label,
        stringsAsFactors = FALSE
    )
    if (side %in% c("left", "right")) {
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = position, x = label, fill = value)) +
            ggplot2::geom_tile(colour = tileColor) +
            ggplot2::scale_x_discrete(
                position = if (side == "left") "bottom" else "top",
                expand = ggplot2::expansion(add = 0)
            ) +
            ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0))
    } else {
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = position, y = label, fill = value)) +
            ggplot2::geom_tile(colour = tileColor) +
            ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0)) +
            ggplot2::scale_y_discrete(position = "right", expand = ggplot2::expansion(add = 0))
    }
    p <- p + ggplot2::labs(x = NULL, y = NULL, fill = label) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text.x = if (showNames) ggplot2::element_text() else ggplot2::element_blank(),
            axis.text.y = if (showNames) ggplot2::element_text() else ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.25),
            plot.margin = ggplot2::margin(2, 2, 2, 2)
        )
    if (!is.null(palette)) p <- p + ggplot2::scale_fill_manual(values = palette)
    p
}
