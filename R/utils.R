utils::globalVariables(
    c("position", "label", "x", "y", "xend", "yend", "column", "value")
)

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
            stop(
                sprintf(
                    "'%s' must contain unique valid dimension names", argument
                ),
                call. = FALSE
            )
        }
        order <- match(order, ids)
    }
    if (!is.numeric(order) || length(order) != n || anyNA(order) ||
        any(order != as.integer(order)) || anyDuplicated(order) ||
        !identical(sort(as.integer(order)), seq_len(n))) {
        stop(
            sprintf(
                "'%s' must be a permutation of 1:%d or the corresponding names",
                argument, n
            ),
            call. = FALSE
        )
    }
    as.integer(order)
}

#' Set row clustering state
#'
#' Internal setter that keeps `rowDendro` and `rowOrder` in sync; a
#' dendrogram is only ever stored alongside the order it implies (or
#' `NULL` alongside a manually-supplied order).
#'
#' @param x A SummarizedHeatmap object.
#' @param dendrogram A dendrogram, or `NULL`.
#' @param order Integer row display order.
#' @return `x` with `rowDendro`/`rowOrder` updated.
#' @noRd
.setRowClustering <- function(x, dendrogram, order) {
    x@rowDendro <- dendrogram
    x@rowOrder <- as.integer(order)
    x
}

#' Set column clustering state
#'
#' @inherit .setRowClustering description return
#' @param x A SummarizedHeatmap object.
#' @param dendrogram A dendrogram, or `NULL`.
#' @param order Integer column display order.
#' @noRd
.setColClustering <- function(x, dendrogram, order) {
    x@colDendro <- dendrogram
    x@colOrder <- as.integer(order)
    x
}

#' Clear row clustering state
#'
#' Drops any stored row dendrogram and resets the row order to the
#' natural (identity) order, e.g. after subsetting changes row membership
#' or order.
#'
#' @param x A SummarizedHeatmap object.
#' @return `x` with `rowDendro` set to `NULL` and `rowOrder` reset.
#' @noRd
.clearRowClustering <- function(x) {
    .setRowClustering(x, NULL, seq_len(nrow(x)))
}

#' Clear column clustering state
#'
#' @inherit .clearRowClustering description return
#' @param x A SummarizedHeatmap object.
#' @noRd
.clearColClustering <- function(x) {
    .setColClustering(x, NULL, seq_len(ncol(x)))
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
    list(
        dendrogram = dendrogram,
        order = as.integer(order.dendrogram(dendrogram))
    )
}

# Validate `vars` (and derive display `labels`) against the annotation
# `data.frame` for one axis, resolving `NULL` to all available variables.
.validate_annotation_vars <- function(vars, data, axis_name) {
    if (is.null(vars)) vars <- names(data)
    if (!is.character(vars) || anyNA(vars) || anyDuplicated(unname(vars))) {
        stop(
            "'vars' must be a character vector of unique annotation names",
            call. = FALSE
        )
    }
    labels <- names(vars)
    if (is.null(labels)) labels <- unname(vars)
    missing <- setdiff(unname(vars), names(data))
    if (length(missing)) {
        stop(
            sprintf(
                "unknown %s annotation variable(s): %s",
                axis_name, paste(missing, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    list(vars = vars, labels = labels)
}

.annotation_plots <- function(
    x, margin, vars, side, tileColor, palette, showNames) {
    metadata <- if (margin == 1L) {
        SummarizedExperiment::rowData(x)
    } else {
        SummarizedExperiment::colData(x)
    }
    data <- as.data.frame(metadata)
    ids <- .axis_ids(x, margin)
    order <- if (margin == 1L) rowOrder(x) else colOrder(x)
    axis_name <- if (margin == 1L) "row" else "column"
    validated <- .validate_annotation_vars(vars, data, axis_name)
    vars <- validated$vars
    labels <- validated$labels
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
    # Returned as a raw (unwrapped) patchwork rather than a
    # `patchwork::wrap_elements()`-fixed patch: wrapping would keep it safe
    # from flattening when combined with `+`/`/`/`|`, but it also blocks
    # patchwork's panel alignment and guide-collection machinery, breaking
    # `plotHeatmap()`'s aligned annotation strips and shared guide area. To
    # combine this result with another plot without risking that operators
    # silently flatten its panel list into the parent's (which can break
    # `plot_layout(heights = ...)` or misplace panels under
    # `plot_layout(design = ...)`), use `patchwork::wrap_plots(list(...))`
    # instead of the operators; see `plotColData()`/`plotRowData()`.
    # `plotRowData()`/`plotColData()` restrict `side` to left/right or
    # top/bottom respectively, so `margin == 2L` (column annotations) always
    # implies `side %in% c("top", "bottom")` here.
    if (margin == 2L) {
        patchwork::wrap_plots(plots, ncol = 1L, guides = "auto")
    } else {
        patchwork::wrap_plots(plots, guides = "auto")
    }
}

# Build the base plot and axis-text theme elements for a row-axis
# annotation panel (`side %in% c("left", "right")`), where `position` (row
# identifiers) sits on the y axis and `label` (the variable name) sits on
# the rotated x axis.
.annotation_plot_row_layer <- function(
    plot_data, side, tileColor, position_text) {
    p <- ggplot2::ggplot(
        plot_data, ggplot2::aes(y = position, x = label, fill = value)
    ) +
        ggplot2::geom_tile(colour = tileColor) +
        ggplot2::scale_x_discrete(
            # In `plotHeatmap()`'s composite grid, a row-axis panel's top
            # neighbour is always blank (column/row-annotation panels only
            # occupy the main column, not the row-dendrogram/row-annotation
            # columns), regardless of whether the panel sits to the "left"
            # or "right" of the heatmap. Anchoring the label at "top"
            # therefore keeps it compact for both sides; "right" places it
            # at "bottom" instead, where it is more likely to compete with
            # the heatmap's own column identifier labels for space (see
            # `plotRowData()`'s docs).
            position = if (side == "left") "top" else "bottom",
            expand = ggplot2::expansion(add = 0)
        ) +
        ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0))
    label_text <- ggplot2::element_text(
        colour = "black", angle = 90, vjust = 0.5,
        hjust = if (side == "left") 0 else 1
    )
    list(p = p, axis_text = list(x = label_text, y = position_text))
}

# Build the base plot and axis-text theme elements for a column-axis
# annotation panel (`side %in% c("top", "bottom")`), where `position`
# (column identifiers) sits on the x axis and `label` sits on the y axis.
.annotation_plot_col_layer <- function(plot_data, tileColor, position_text) {
    p <- ggplot2::ggplot(
        plot_data, ggplot2::aes(x = position, y = label, fill = value)
    ) +
        ggplot2::geom_tile(colour = tileColor) +
        ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0)) +
        ggplot2::scale_y_discrete(
            position = "right", expand = ggplot2::expansion(add = 0)
        )
    label_text <- ggplot2::element_text(colour = "black", hjust = 0)
    list(p = p, axis_text = list(x = position_text, y = label_text))
}

.annotation_plot <- function(
    value, axis_ids, label, side, tileColor, palette, showNames) {
    if (is.list(value) && !is.factor(value)) {
        stop("annotation values must be atomic vectors", call. = FALSE)
    }
    plot_data <- data.frame(
        position = factor(axis_ids, levels = axis_ids),
        value = value,
        label = label,
        stringsAsFactors = FALSE
    )
    # `position` (the row/column identifiers) is only labelled when
    # `showNames` is set. `label` (the annotation variable's display name)
    # is always shown as axis text, mirroring how a variable name appears
    # next to a facet or annotation track in a standard geom_tile() plot,
    # and matches `plotHeatmapMain()`'s black axis text. Axis text next to
    # the tiles is justified to hug them rather than float away, using the
    # ggplot2 convention that `hjust`/`vjust` = 0 anchors text at its near
    # edge; row annotations (where `label` sits on the rotated x axis) print
    # the variable name vertically since that axis is narrow.
    position_text <- if (showNames) {
        ggplot2::element_text(colour = "black")
    } else {
        ggplot2::element_blank()
    }
    layer <- if (side %in% c("left", "right")) {
        .annotation_plot_row_layer(plot_data, side, tileColor, position_text)
    } else {
        .annotation_plot_col_layer(plot_data, tileColor, position_text)
    }
    p <- layer$p + ggplot2::labs(x = NULL, y = NULL, fill = label) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text.x = layer$axis_text$x,
            axis.text.y = layer$axis_text$y,
            panel.grid = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(
                fill = NA, colour = "black", linewidth = 0.25
            ),
            plot.margin = ggplot2::margin(2, 2, 2, 2)
        )
    if (!is.null(palette)) p <- p + ggplot2::scale_fill_manual(values = palette)
    p
}
