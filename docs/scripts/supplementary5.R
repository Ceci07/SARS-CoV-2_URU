library(grid)
library(tidyverse)
library(ggtree)
library(ape)
library(phangorn)
library(RColorBrewer)
library(ggnewscale)
library(data.table)
library(ggthemes)
library(stringr)
library(ggpubr)  
library(dplyr)


setwd("~/Documentos/trabajo/paper_delta/scripts/supplementary_5/")


# Gamma
wt <-
        as.data.frame(read.table(
                "GTR_region_gamma_tree.txt",
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
                "GTR_region_gamma_tree_regions.txt",
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

dt2$value <- gsub(",", ".", dt2$value)
dt2$value <- as.numeric(as.character(dt2$value))
dt2$value <- round(dt2$value, 1)


region <- ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
        geom_tile(color = "white",
                  lwd = 1.5,
                  linetype = 1) +
        scale_fill_gradient2(
                low = "#075AFF",
                mid = "#FFFFCC",
                high = "#FF0000"
        )  +
        geom_text(aes(label = value), color = "black", size = 3) +
        coord_fixed() +
        theme(
                axis.text.x = element_text(
                        angle = 90,
                        vjust = 0.5,
                        hjust = 1,
                        size = 12
                ),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 14),
                legend.text = element_text(size = 10),
                legend.title = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(size = 12)
        ) +
        xlab("To") + ylab("From") + ggtitle("Gamma transition rates (%) between regions")
region  


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
        geom_text(aes(label = rate), hjust = 1, size = 3.5) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 10),
                legend.position = "none",
                plot.title = element_text(size = 10)
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
        geom_text(aes(label = rate), hjust = 1, size = 3.5) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 10),
                legend.position = "none",
                plot.title = element_text(size = 10)
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


figb <- ggarrange(bar_i, bar_e, nrow = 2)
figb <-
        annotate_figure(
                figb,
                left = textGrob(
                        "Region",
                        rot = 90,
                        vjust = 1,
                        gp = gpar(cex = 1)
                ),
                bottom = textGrob("Tranistion rate (%)", gp = gpar(cex = 1))
        )



fig1 <-
        ggarrange(
                region,
                figb,
                ncol = 2,
                labels = c("B", ""),
                heights = c(1, 0.6),
                widths = c(1, 0.6)
        )
fig1

png(
        'transition_region_wt.png',
        res = 800,
        height = 30,
        width = 55,
        units = 'cm'
)
fig1
dev.off()


# Delta

wt <-
        as.data.frame(read.table(
                "GTR_region_delta_tree.txt",
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
                "GTR_region_delta_tree_regions.txt",
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

dt2$value <- gsub(",", ".", dt2$value)
dt2$value <- as.numeric(as.character(dt2$value))
dt2$value <- round(dt2$value, 1)


region <- ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
        geom_tile(color = "white",
                  lwd = 1.5,
                  linetype = 1) +
        scale_fill_gradient2(
                low = "#075AFF",
                mid = "#FFFFCC",
                high = "#FF0000"
        )  +
        geom_text(aes(label = value), color = "black", size = 3) +
        coord_fixed() +
        theme(
                axis.text.x = element_text(
                        angle = 90,
                        vjust = 0.5,
                        hjust = 1,
                        size = 12
                ),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 14),
                legend.text = element_text(size = 10),
                legend.title = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(size = 12)
        )  +
        xlab("To") + ylab("From") + ggtitle("Delta transition rates (%) between regions")
region  


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
        geom_text(aes(label = rate), hjust = 1, size = 3.5) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 10),
                legend.position = "none",
                plot.title = element_text(size = 10)
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
        geom_text(aes(label = rate), hjust = 1, size = 3.5) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 10),
                legend.position = "none",
                plot.title = element_text(size = 10)
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


figb <- ggarrange(bar_i, bar_e, nrow = 2)
figb <-
        annotate_figure(
                figb,
                left = textGrob(
                        "Region",
                        rot = 90,
                        vjust = 1,
                        gp = gpar(cex = 1)
                ),
                bottom = textGrob("Tranistion rate (%)", gp = gpar(cex = 1))
        )

fig2 <-
        ggarrange(
                region,
                figb,
                ncol = 2,
                labels = c("C", ""),
                heights = c(1, 0.6),
                widths = c(1, 0.6)
        )

fig2

png(
        'transition_region_wt.png',
        res = 800,
        height = 30,
        width = 55,
        units = 'cm'
)
fig2
dev.off()



figA <- ggarrange(fig1, fig2, nrow = 2, widths = c(1, 1))
figA


### Read time-scaled ML tree

tre <- read.nexus('./timetree.nexus')

meta  <-
        read.csv('datsaet_phylodynamics.tsv',
                 sep = '\t',
                 header = T)
meta2 <- meta
rownames(meta2) <- meta[, 1]
clade2 <- rep('Other', dim(meta2)[1])
delta <- grep('Delta', meta2$clade)
gamma <- grep('Gamma', meta2$clade)
lambd <- grep('Lambda', meta2$clade)
alpha <- grep('Alpha', meta2$clade)
mu    <- grep('Mu', meta2$clade)
beta  <- grep('Beta', meta2$clade)

clade2[delta] <- 'Delta'
clade2[gamma] <- 'Gamma'
clade2[lambd] <- 'Lambda'
clade2[alpha] <- 'Alpha'
clade2[mu] <- 'Mu'
clade2[beta] <- 'Beta'

meta2$clade2 <- clade2

grp <- list()
uph <- unique(meta2$clade2)
oth <- c()
nam <- c()
r   <- 1

for (u in 1:length(uph)) {
        w <- which(meta2$clade2 == uph[u])
        
        if (length(w) < 1) {
                oth <- c(oth, w)
                
        } else {
                s <- meta2[w, 1]
                grp[[r]] <- s
                nam <- c(nam, uph[u])
                r <- r + 1
        }
}

names(grp) <- uph


g1 <- ggtree(tre, layout = 'fan', size = .1) %<+% meta2

delta.clade  <- MRCA(tre, grp$Delta)
gamma.clade  <- MRCA(tre, grp$Gamma)

dat <- data.frame(id = c(delta.clade, gamma.clade),
                  hclade = c('Delta', 'Gamma'))

gheat2 <- meta2 %>%
        mutate(country = replace(country, country != 'Uruguay', 2)) %>%
        mutate(country = replace(country, country == 'Uruguay', 1)) %>%
        mutate(country = replace(country, accession == 'EPI_ISL_NA', 3)) %>%
        mutate_at(2, as.numeric)

gheat2 <- as.data.frame(gheat2[, 'country'])
rownames(gheat2) <- rownames(meta2)
colnames(gheat2) <- 'uruguay'


g2 <-
        gheatmap(
                g1,
                gheat2,
                offset = .05,
                width = .075,
                colnames = F,
                color = NA
        ) +
        scale_fill_manual(
                labels = c('Uruguay (public)', 'Rest of the world', 'Uruguay (this study)'),
                values = c('black', 'grey90', 'brown1'),
                name = 'Origin'
        )


g3 <- g1 + new_scale_fill() +
        geom_hilight(
                data = dat,
                mapping = aes(node = id, fill = hclade),
                alpha = .4,
                extendto = 2.15
        ) +
        labs(fill = "Variant") +
        theme(
                legend.text = element_text(size = 12),
                legend.key.size = unit(.5, 'cm'),
                legend.position = 'left'
        )
g3


fig <-
        ggarrange(
                g3,
                figA,
                ncol = 2,
                heights = c(0.4, 0.6),
                widths = c(0.4, 0.6),
                labels = c("A", "")
        )
fig

png(
        'Supplementary5.png',
        res = 800,
        height = 30,
        width = 55,
        units = 'cm'
)
fig
dev.off()

