library(patchwork)
library(ggplot2)
plotHeatmapMain(hm)
plotColData(hm)
design <-
"
D#
A#
BC
"

wrap_plots(A = plotColData(hm, vars = "condition"),
           B = plotHeatmapMain(hm),
           C = guide_area(),
           D = plotColDendro(hm),
           design = design, heights = c(0.5, 0.5, 5),
           widths = c(5, 1),
           guides = "collect")

plotHeatmap(hm, rowDendroSide = "right")# not working:  doesn't
plotHeatmap(hm, rowAnnotationSide = "right")# not working
plotHeatmap(hm, colDendroSide = "right")# should err
plotHeatmap(hm, colDendroSide = "bottom")# should err



