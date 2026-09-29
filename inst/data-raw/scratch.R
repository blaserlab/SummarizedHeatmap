library(patchwork)
library(ggplot2)
plotHeatmapMain(hm)
plotColData(hm)
design <-
"
A#
BC
"

wrap_plots(A = plotColData(hm, vars = "condition"),
           B = plotHeatmapMain(hm),
           C = guide_area(),
           design = design, heights = c(1, 5),
           widths = c(5, 1),
           guides = "collect")
