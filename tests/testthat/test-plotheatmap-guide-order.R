# End-to-end regression test for the guide ordering documented in
# `.PANEL_GUIDE_ORDER`/`.orderPanelsForGuides()` (R/plot-heatmap.R):
# `plotHeatmap()` should collect guides in the order main heatmap, column
# annotation, row annotation, regardless of which side each panel is placed
# on. Rather than reaching into patchwork's (undocumented) internal object
# representation, this renders the real, assembled patchwork object and
# inspects the grid grob tree that patchwork actually draws -- i.e. the
# same collected guide box a user would see -- for the order legend titles
# appear in.

# Depth-first search of a grob tree (gtable "grobs" lists and grid
# "children" gLists both nest this way) for `grid::textGrob()` labels,
# returned in traversal order. For a gtable of stacked legends, traversal
# order matches the top-to-bottom order the legends were assembled in.
find_grob_labels <- function(g) {
    labels <- character(0)
    if (is.null(g)) {
        return(labels)
    }
    if (!is.null(g$label) && is.character(g$label)) {
        labels <- c(labels, g$label)
    }
    for (nm in c("grobs", "children")) {
        kids <- g[[nm]]
        if (!is.null(kids)) {
            for (kid in kids) labels <- c(labels, find_grob_labels(kid))
        }
    }
    labels
}

# Order in which a set of (distinguishing) legend titles first appear in a
# rendered patchwork object's collected guide box.
guide_title_order <- function(patch, titles) {
    gt <- patchwork::patchworkGrob(patch)
    guide_boxes <- gt$grobs[vapply(gt$grobs, function(g) identical(g$name, "guide-box"), logical(1))]
    stopifnot(length(guide_boxes) == 1L)
    found <- find_grob_labels(guide_boxes[[1]])
    titles[order(match(titles, found))]
}

test_that("plotHeatmap() collects guides in main, column-annotation, row-annotation order on default sides", {
    x <- make_heatmap()
    p <- plotHeatmap(
        x,
        colVars = c(ColLegendTitle = "condition"),
        rowVars = c(RowLegendTitle = "feature_type"),
        fillTitle = "MainLegendTitle"
    )
    titles <- c("MainLegendTitle", "ColLegendTitle", "RowLegendTitle")
    expect_identical(guide_title_order(p, titles), titles)
})

test_that("plotHeatmap() keeps guide order fixed when panels are moved to non-default sides", {
    x <- make_heatmap()
    p <- plotHeatmap(
        x,
        colVars = c(ColLegendTitle = "condition"),
        rowVars = c(RowLegendTitle = "feature_type"),
        fillTitle = "MainLegendTitle",
        rowDendroSide = "right", rowAnnotationSide = "right",
        colDendroSide = "bottom", colAnnotationSide = "bottom"
    )
    titles <- c("MainLegendTitle", "ColLegendTitle", "RowLegendTitle")
    # Same guide order as the default-sides case above, even though every
    # panel has moved to the opposite side of the heatmap: guide order is
    # controlled by each panel's fixed letter identity (`.PANEL_GUIDE_ORDER`/
    # `.orderPanelsForGuides()`), not by where it lands spatially.
    expect_identical(guide_title_order(p, titles), titles)
})
