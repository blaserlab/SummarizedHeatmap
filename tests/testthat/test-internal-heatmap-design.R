# `.heatmap_design()` returns a plain multi-line character string (the
# `design` argument `patchwork::wrap_plots()` parses into a grid), plus
# matching `widths`/`heights` vectors. These helpers re-parse that string
# ourselves so tests can assert panel placement without reaching into
# patchwork's own (undocumented) parsed layout.
design_matrix <- function(design) {
    rows <- strsplit(design, "\n")[[1]]
    do.call(rbind, lapply(rows, function(r) strsplit(r, "")[[1]]))
}

# Bounding box (row/column index ranges) of a letter within the parsed grid,
# or `NULL` if the letter doesn't appear (i.e. its panel was omitted).
design_bbox <- function(design, letter) {
    m <- design_matrix(design)
    hits <- which(m == letter, arr.ind = TRUE)
    if (!nrow(hits)) return(NULL)
    list(
        top = min(hits[, "row"]), bottom = max(hits[, "row"]),
        left = min(hits[, "col"]), right = max(hits[, "col"])
    )
}

test_that(".heatmap_design() places default sides with annotations adjacent to the main panel", {
    # Defaults: row dendrogram ("D") and row annotation ("E") both left of
    # the heatmap ("A"); column dendrogram ("B") and column annotation ("C")
    # both above it, dendrogram further out.
    layout <- .heatmap_design(
        rowDendroSide = "left", rowAnnotationSide = "left",
        colDendroSide = "top", colAnnotationSide = "top",
        guideWidth = 3
    )
    expect_identical(layout$design, "##BM\n##CM\nDEAM")
    expect_equal(layout$widths, c(1, 1, 8, 3))
    expect_equal(layout$heights, c(1, 1, 8))
})

test_that(".heatmap_design() moves the row dendrogram independently of the row annotation", {
    default <- .heatmap_design("left", "left", "top", "top", guideWidth = 3)
    main <- design_bbox(default$design, "A")

    dendro_right <- .heatmap_design("right", "left", "top", "top", guideWidth = 3)
    main_dr <- design_bbox(dendro_right$design, "A")
    expect_gt(design_bbox(dendro_right$design, "D")$left, main_dr$left)
    expect_lt(design_bbox(dendro_right$design, "E")$left, main_dr$left)
})

test_that(".heatmap_design() moves the row annotation independently of the row dendrogram", {
    ann_right <- .heatmap_design("left", "right", "top", "top", guideWidth = 3)
    main_ar <- design_bbox(ann_right$design, "A")
    expect_gt(design_bbox(ann_right$design, "E")$left, main_ar$left)
    expect_lt(design_bbox(ann_right$design, "D")$left, main_ar$left)
})

test_that(".heatmap_design() keeps the annotation adjacent when both row panels share a side", {
    both_right <- .heatmap_design("right", "right", "top", "top", guideWidth = 3)
    main <- design_bbox(both_right$design, "A")
    e_box <- design_bbox(both_right$design, "E")
    d_box <- design_bbox(both_right$design, "D")
    expect_lt(main$left, e_box$left)
    expect_lt(e_box$left, d_box$left)
})

test_that(".heatmap_design() moves column panels between top and bottom", {
    col_bottom <- .heatmap_design("left", "left", "bottom", "top", guideWidth = 3)
    main <- design_bbox(col_bottom$design, "A")
    expect_gt(design_bbox(col_bottom$design, "B")$top, main$top)

    ann_bottom <- .heatmap_design("left", "left", "top", "bottom", guideWidth = 3)
    main_ab <- design_bbox(ann_bottom$design, "A")
    expect_gt(design_bbox(ann_bottom$design, "C")$top, main_ab$top)
    expect_lt(design_bbox(ann_bottom$design, "B")$top, main_ab$top)

    both_bottom <- .heatmap_design("left", "left", "bottom", "bottom", guideWidth = 3)
    main_bb <- design_bbox(both_bottom$design, "A")
    c_box <- design_bbox(both_bottom$design, "C")
    b_box <- design_bbox(both_bottom$design, "B")
    expect_lt(main_bb$top, c_box$top)
    expect_lt(c_box$top, b_box$top)
})

test_that(".heatmap_design() drops omitted panels from the grid and their width/height entry", {
    default <- .heatmap_design("left", "left", "top", "top", guideWidth = 3)

    no_col_ann <- .heatmap_design("left", "left", "top", NULL, guideWidth = 3)
    expect_null(design_bbox(no_col_ann$design, "C"))
    expect_true(grepl("^[^C]+$", no_col_ann$design))
    expect_equal(length(no_col_ann$heights), length(default$heights) - 1L)
    # The remaining column dendrogram sits directly above the main panel now
    # that the annotation panel between them is gone.
    expect_lt(design_bbox(no_col_ann$design, "B")$top, design_bbox(no_col_ann$design, "A")$top)

    no_row_ann <- .heatmap_design("left", NULL, "top", "top", guideWidth = 3)
    expect_null(design_bbox(no_row_ann$design, "E"))
    expect_equal(length(no_row_ann$widths), length(default$widths) - 1L)
    expect_lt(design_bbox(no_row_ann$design, "D")$left, design_bbox(no_row_ann$design, "A")$left)

    no_row_dendro <- .heatmap_design(NULL, "left", "top", "top", guideWidth = 3)
    expect_null(design_bbox(no_row_dendro$design, "D"))
    expect_equal(length(no_row_dendro$widths), length(default$widths) - 1L)
    expect_lt(design_bbox(no_row_dendro$design, "E")$left, design_bbox(no_row_dendro$design, "A")$left)

    no_col_dendro <- .heatmap_design("left", "left", NULL, "top", guideWidth = 3)
    expect_null(design_bbox(no_col_dendro$design, "B"))
    expect_equal(length(no_col_dendro$heights), length(default$heights) - 1L)
    expect_lt(design_bbox(no_col_dendro$design, "C")$top, design_bbox(no_col_dendro$design, "A")$top)

    # Every dendrogram/annotation panel omitted: only the main panel and the
    # guide column remain.
    bare <- .heatmap_design(NULL, NULL, NULL, NULL, guideWidth = 3)
    expect_identical(bare$design, "AM")
    expect_equal(bare$widths, c(8, 3))
    expect_equal(bare$heights, 8)
})

test_that(".heatmap_design()'s guideWidth sets the guide column's width only", {
    layout <- .heatmap_design("left", "left", "top", "top", guideWidth = 5)
    expect_equal(layout$widths, c(1, 1, 8, 5))
    expect_equal(layout$heights, c(1, 1, 8))
})

test_that(".heatmap_design() omits the guide column entirely when guideWidth is NULL", {
    layout <- .heatmap_design("left", "left", "top", "top", guideWidth = NULL)
    expect_null(design_bbox(layout$design, "M"))
    expect_true(grepl("^[^M]+$", layout$design))
    expect_identical(layout$design, "##B\n##C\nDEA")
    # No trailing entry for the guide column's width.
    expect_equal(layout$widths, c(1, 1, 8))
    expect_equal(layout$heights, c(1, 1, 8))
})
