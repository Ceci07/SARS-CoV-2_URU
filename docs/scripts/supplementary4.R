library(tidyverse)
library(stringr)
library(ggpubr)
library(grid)


setwd("/home/ceciliasalazar/Documentos/trabajo/paper_delta/scripts/supplementary_4/")

wt <-
  as.data.frame(read.table(
    "GTR_region_whole_tree.txt",
    sep = "\t",
    header = T
  ))

wt <- mutate_all(wt, function(x)
  as.numeric(as.character(x)))
dim(wt)
wt$X <- NULL
sum(wt)

abs <- wt / sum(wt) * 100

region <-
  as.data.frame(read.table(
    "GTR_region_whole_tree_regions.txt",
    sep = "\t",
    header = F
  ))
region <- as.data.frame(region[-6, ])

split <-
  as.data.frame(str_split_fixed(region$`region[-6, ]`, ": ", 2))
split$V2 <- gsub("SouthAmerica", "South America", split$V2)
split$V2 <- gsub("NorthAmerica", "North America", split$V2)
colnames(abs) <- split$V2
rownames(abs) <- split$V2

dt2 <- abs %>%
  rownames_to_column() %>%
  gather(colname, value,-rowname)
head(dt2)

dt2$value <- gsub(",", ".", dt2$value)
dt2$value <- as.numeric(as.character(dt2$value))
dt2$value <- round(dt2$value, 2)
head(dt2)

region <- ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient2(
    low = "#075AFF",
    mid = "#FFFFCC",
    high = "#FF0000"
  )  +
  geom_text(aes(label = value), color = "black", size = 4) +
  coord_fixed() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 16
    ),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  xlab("To") + ylab("From") + ggtitle("Transition rates (%) between regions")
region


### Barplot of the transition rates in the complete tree


intro <- as.data.frame(t(abs[9, ] / sum(abs[9, ]) * 100))
intro$region <- row.names(intro)
row.names(intro) <- NULL
intro <- intro[order(intro$Uruguay, decreasing = F), ]
intro <- select(intro, region, Uruguay)
colnames(intro) <- c("Region", "rate")
intro$rate <- round(intro$rate, 1)
intro <- intro[-1, ]
intro



bar_i <-
  ggplot(intro, aes(
    x = reorder(Region, rate),
    y = rate,
    fill = Region
  )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = rate), hjust = 1, size = 4) +
  theme(
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 16, vjust = 1.5),
    axis.text.y = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Brazil" = "#fcb186",
      "Europe" = "#f3ffbd",
      "South America"  = "#ec9393",
      "Argentina" = "#e6d186",
      "Asia" = "#eca7ba",
      "Africa" = "#d0d9b6",
      "North America" = "#dca76c",
      "Oceania" = "#ffd4d7"
    )
  ) +
  ylab("") + xlab("") +
  coord_flip() + ggtitle("To Uruguay")
bar_i


dif <-  as.data.frame(abs[, 9] / sum(abs[, 9]) * 100)
row.names(dif) <- row.names(abs)
dif$Region <- row.names(dif)
row.names(dif) <- NULL
colnames(dif) <- c("rate", "Region")
dif <- select(dif, Region, rate)
dif <- dif[order(dif$rate, decreasing = F), ]
dif <- dif[-1, ]
dif$rate <- round(dif$rate, 1)

bar_e <-
  ggplot(dif, aes(
    x = reorder(Region, rate),
    y = rate,
    fill = Region
  )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = rate), hjust = 1, size = 4) +
  theme(
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 16, vjust = 1.5),
    axis.text.y = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Brazil" = "#fcb186",
      "Europe" = "#f3ffbd",
      "South America"  = "#ec9393",
      "Argentina" = "#e6d186",
      "Asia" = "#eca7ba",
      "Africa" = "#d0d9b6",
      "North America" = "#dca76c",
      "Oceania" = "#ffd4d7"
    )
  ) +
  ylab("") + xlab("") +
  coord_flip() + ggtitle("From Uruguay")
bar_e


figb <- ggarrange(bar_i, bar_e, nrow = 2, labels = c("B", "C"))
figb <-
  annotate_figure(
    figb,
    left = textGrob(
      "",
      rot = 90,
      vjust = 2,
      hjust = 1,
      gp = gpar(cex = 1.5)
    ),
    bottom = textGrob("Tranistion rate (%)", gp = gpar(cex = 1.5))
  )
figb


fig <-
  ggarrange(
    region,
    figb,
    ncol = 2,
    labels = c("A", ""),
    heights = c(1, 0.5),
    widths = c(1, 0.5)
  )
fig

png(
  'Supplementary4.png',
  res = 800,
  height = 25,
  width = 50,
  units = 'cm'
)
fig
dev.off()
