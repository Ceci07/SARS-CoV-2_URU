library(dplyr)
library(ggplot2)
library(stringr)
library(ggpubr)
library(grid)
library(gridExtra)

setwd("~/Documentos/trabajo/paper_delta/scripts/supplementary_3/")

cl <- read.table("classif_s1_s28_next_pango.tsv", header = T, sep = "\t")
head(cl)
colnames(cl)[3] <- c("Identificador.de.Consorcio")

ck <- read.table("summary_with_depth.tsv", header = T, sep = "\t")
head(ck)
colnames(ck)[1] <- c("Identificador.de.Consorcio")

merged <- merge(cl, ck, by = "Identificador.de.Consorcio")
dim(merged)
head(merged)

merged$check_clade <- merged$clade == merged$Nextstrain_clade
unique(merged$check_clade)
merged[which(merged$check_clade == "FALSE"),]
merged$check_pango <- merged$lineage== merged$PANGO_lineage
unique(merged$check_pango)
merged[which(merged$check_pango == "FALSE"),]

#usar cl para clado y linaje ya que es lo mas actualizado
ck$PANGO_lineage <- NULL
cl[,4:5] <- NULL

merged <- merge(cl, ck, by = "Identificador.de.Consorcio")
head(merged)

clade <- as.data.frame(table(merged$Nextstrain_clade))
clade <- clade[order(clade$Freq, decreasing = T),]
clade$ptc <- clade$Freq/sum(clade$Freq)*100

c20j <- merged[which(merged$Nextstrain_clade == "20J (Gamma, V3)"),]
c20jL <- as.data.frame(table(c20j$lineage))
c20jL$pct <- c20jL$Freq/sum(c20jL$Freq)*100

c21j <- merged[which(merged$clade == "21J (Delta)"),]
c21jL <- as.data.frame(table(c21j$lineage))
c21jL$pct <- c21jL$Freq/sum(c21jL$Freq)*100
c21jL <- c21jL[order(c21jL$Freq, decreasing = T),]

#1049 with Ns < 15% available for analyzing, 118 of these samples have no date associated. Probably travelers
list <- merged %>%
        group_by(Date, lineage, clade) %>%
        tally() %>%
        ungroup()
list <- as.data.frame(list)
dim(list)
head(list)
list$Date <- as.Date(list$Date)

s3 <- ggplot(list, aes(fill=lineage, y=n, x=Date)) + 
        geom_bar(position="stack", stat="identity", alpha = 1) +
        scale_fill_manual(values = c("#4e2e60",
                                     "#00b40f",
                                     "#3018c0",
                                     "#00ae58",
                                     "#bb14d6",
                                     "#4b7e00",
                                     "#d14eff",
                                     "#7f6e00",
                                     "#5100a9",
                                     "#ff8713",
                                     "#005bed",
                                     "#ff4d04",
                                     "#7966ff",
                                     "#c07e00",
                                     "#002ea9",
                                     "#cca650",
                                     "#b167ff",
                                     "#86b879",
                                     "#bc00b4",
                                     "#00805c",
                                     "#ff65e8",
                                     "#004c28",
                                     "#b0009a",
                                     "#baab70",
                                     "#45248d",
                                     "#ff8b61",
                                     "#0196f3",
                                     "#b80011",
                                     "#0170c3",
                                     "#992f00",
                                     "#9e8cff",
                                     "#7c4b00",
                                     "#ee82f3",
                                     "#7d5e33",
                                     "#810075",
                                     "#77aeee",
                                     "#b00023",
                                     "#004272",
                                     "#ff5d6d",
                                     "#572371",
                                     "#ff8885",
                                     "#7f0057",
                                     "#dd92d3",
                                     "#7a0b17",
                                     "#fe7fcc",
                                     "#ad0038",
                                     "#986b97",
                                     "#da0075",
                                     "#a25d62",
                                     "#731243")) +
        theme(axis.line = element_line(colour = "grey"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(hjust = 0.5, vjust = 1, size = 12),
              axis.line.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16),
              legend.position="bottom",
              legend.box.just = "center",
              legend.key.width=unit(1, "lines")) +
        scale_x_date(date_breaks = "1 months") +
        ggtitle("") + 
        guides(fill=guide_legend(title="PANGO lineage", nrow = 5, ncol = 11)) + scale_y_continuous(trans = "reverse", breaks = seq(0, 80, by =10)) +
        ylab("Log scale number of genomes") + xlab("Date") 
s3

s4 <- ggplot(list, aes(fill=clade, y=n, x=Date)) + 
        geom_bar(position="stack", stat="identity", alpha = 0.8) +
        scale_fill_manual(values = c("#cf4ec5",
                                     "#489954",
                                     "#7349cd",
                                     "#747f2e",
                                     "#617bc4",
                                     "#db4e33",
                                     "#904b8a",
                                     "#bc8136",
                                     "#d04a77",
                                     "#9c4c3c")) +
        theme(axis.line = element_line(colour = "grey"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.line.x = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16),
              legend.position="top",
              legend.box.just = "center",
              legend.key.width=unit(1, "lines")) +
        scale_x_date(date_breaks = "2 months") +
        ggtitle("") + scale_y_continuous(breaks = seq(0, 80, by =10)) +
        guides(fill=guide_legend(title="Nextclade", nrow = 2, ncol = 10)) +
        ylab("Log scale number of genomes") + xlab("Date")
s4

sup <- ggarrange(s4, s3, nrow = 2, vjust = 2, widths = c(0.5, 0.5), heights = c(0.5, 0.5))
sup

sup2 <- annotate_figure(sup, left = textGrob("Number of genomes", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
sup2

png('Supplementary_3.png', res = 600, height = 25, width = 40, units = 'cm')
sup2
dev.off()

