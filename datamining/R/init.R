library(toolkitEntel)
safeLibrary(scales)
safeLibrary(plyr)
safeLibrary(ggmap)
safeLibrary(ggplot2)
safeLibrary(reshape2)
safeLibrary(bit64)
th=theme_light(base_family = "calibri")
blank_theme <- theme_minimal(base_family = "calibri")+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14)
  )

