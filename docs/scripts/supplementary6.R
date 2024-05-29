library(dplyr)
library(tidyverse)
library(data.table)
library(ggpubr)              


setwd("~/Documentos/trabajo/paper_delta/scripts/supplementary_6/")
wt <- as.data.frame(read.csv("uru_regions_gamma2.csv", header = T))
wt <- mutate_all(wt, function(x)
        as.numeric(as.character(x)))
dim(wt)
wt$X <- NULL
sum(wt)

abs <- wt / sum(wt) * 100

region <- as.data.frame(colnames(wt))
colnames(region) <- c("region")


region$region <-
        gsub("SouthAmerica", "South America", region$region)
region$region <-
        gsub("NorthAmerica", "North America", region$region)
region$region <- gsub("Dry_border", "Dry border", region$region)
region$region <-
        gsub("Rest_of_the_country", "Rest of the country", region$region)


colnames(abs) <- region$region
rownames(abs) <- region$region

meanto <- as.data.frame(rowMeans(abs))
colnames(meanto) <- c("Mean")
meanto$Region <- row.names(meanto)
row.names(meanto)<- NULL
meanto$Mean <- as.numeric(meanto$Mean)
meanto$Mean <- round(meanto$Mean, 1)


gamma_to <-
        ggplot(meanto, aes(
                x = reorder(Region, Mean),
                y = Mean,
                fill = Region
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = Mean), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 16, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("TO")
gamma_to


meanfrom <- as.data.frame(colMeans(abs))
colnames(meanfrom) <- c("Mean")
meanfrom$Region <- row.names(meanfrom)
row.names(meanfrom)<- NULL
meanfrom$Mean <- as.numeric(meanfrom$Mean)
meanfrom$Mean <- round(meanfrom$Mean, 1)

gamma_from <-
        ggplot(meanfrom, aes(
                x = reorder(Region, Mean),
                y = Mean,
                fill = Region
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = Mean), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 16, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("FROM")
gamma_from


gamma_av_trans <- ggarrange(gamma_to, gamma_from, align = "hv")
gamma_av_trans <- annotate_figure(gamma_av_trans, 
                                  top = text_grob("Gamma average transitions", 
                                                  color = "black", face = "bold", size = 14))

gamma_av_trans

dt2 <- abs %>%
        rownames_to_column() %>%
        gather(colname, value, -rowname)

dt2$value <- as.numeric(as.character(dt2$value))
dt2$value <- round(dt2$value, 1)

gururegion <-
        ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
        geom_tile(color = "white",
                  lwd = 1.5,
                  linetype = 1) +
        scale_fill_gradient2(
                low = "#075AFF",
                mid = "#FFFFCC",
                high = "#FF0000"
        )  +
        geom_text(aes(label = value), color = "black", size = 2) +
        coord_fixed() +
        theme(
                axis.text.x = element_text(
                        angle = 90,
                        vjust = 0.5,
                        hjust = 1,
                        size = 12
                ),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 12),
                legend.text = element_text(size = 10),
                legend.title = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()
        ) +
        xlab("To") + ylab("From") + ggtitle("Gamma transition rates (%)")
gururegion  

introm <- as.data.frame(t(abs[8, ]))
introm$subregion <- row.names(introm)
row.names(introm) <- NULL
introm <- introm[order(introm$Metropolitan, decreasing = F), ]
introm <- select(introm, subregion, Metropolitan)
colnames(introm) <- c("UY_regions", "rate")
introm$rate <- round(introm$rate, 1)
introm <- introm[-1, ]
introm
dim(introm)

bar_i <-
        ggplot(introm, aes(
                x = reorder(UY_regions, rate),
                y = rate,
                fill = UY_regions
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = rate), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 16, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("To Metropolitan")
bar_i

difm <-  as.data.frame(abs[, 8])
row.names(difm) <- row.names(abs)
difm$Subregion <- row.names(difm)
row.names(difm) <- NULL
colnames(difm) <- c("rate", "UY_regions")
difm <- select(difm, UY_regions, rate)
difm <- difm[order(difm$rate, decreasing = F), ]
difm <- difm[-1, ]
difm$rate <- round(difm$rate, 1)

bar_e <-
        ggplot(difm, aes(
                x = reorder(UY_regions, rate),
                y = rate,
                fill = UY_regions
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = rate), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("From Metropolitan")
bar_e

df <- merge(introm, difm, by = "UY_regions")
colnames(df) <- c("UY_regions", "To", "From")

DT = melt(df,
          id.vars = c("UY_regions"),
          measure.vars = c("To", "From"))

bar_e_g <-
        ggplot(DT, aes(
                x = reorder(UY_regions, value),
                y = value,
                fill = variable
        )) +
        geom_bar(stat = 'identity', position = 'dodge') +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "right",
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                plot.title = element_text(size = 12)
        ) +
        scale_fill_manual(values = c("To" = "#FF0000",
                                     "From" = "#fff7b5")) +
        ylab("") + xlab("") + scale_y_continuous(breaks = seq(0, 15, by = 5)) +
        coord_flip() + ggtitle("Metropolitan area")
bar_e_g

fig1 <-
        ggarrange(
                gururegion,
                bar_e_g,
                ncol = 2,
                labels = c("A", "B"),
                heights = c(0.8, 0.5),
                widths = c(0.8, 0.5)
        )
fig1


# Delta
wt <- as.data.frame(read.csv("uru_regions_delta2.csv", header = T))

wt <- mutate_all(wt, function(x)
        as.numeric(as.character(x)))
dim(wt)
wt$X <- NULL
sum(wt)

abs <- wt / sum(wt) * 100

region <- as.data.frame(colnames(wt))
colnames(region) <- c("region")


region$region <-
        gsub("SouthAmerica", "South America", region$region)
region$region <-
        gsub("NorthAmerica", "North America", region$region)
region$region <- gsub("Dry_border", "Dry border", region$region)
region$region <-
        gsub("Rest_of_the_country", "Rest of the country", region$region)

colnames(abs) <- region$region
rownames(abs) <- region$region

dt2 <- abs %>%
        rownames_to_column() %>%
        gather(colname, value,-rowname)

dt2$value <- as.numeric(as.character(dt2$value))
dt2$value <- round(dt2$value, 1)

meanto <- as.data.frame(rowMeans(abs))
colnames(meanto) <- c("Mean")
meanto$Region <- row.names(meanto)
row.names(meanto)<- NULL
meanto$Mean <- as.numeric(meanto$Mean)
meanto$Mean <- round(meanto$Mean, 1)


gamma_to <-
        ggplot(meanto, aes(
                x = reorder(Region, Mean),
                y = Mean,
                fill = Region
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = Mean), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 16, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("TO")
gamma_to


meanfrom <- as.data.frame(colMeans(abs))
colnames(meanfrom) <- c("Mean")
meanfrom$Region <- row.names(meanfrom)
row.names(meanfrom)<- NULL
meanfrom$Mean <- as.numeric(meanfrom$Mean)
meanfrom$Mean <- round(meanfrom$Mean, 1)

gamma_from <-
        ggplot(meanfrom, aes(
                x = reorder(Region, Mean),
                y = Mean,
                fill = Region
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = Mean), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 16, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("FROM")
gamma_from


gamma_av_trans <- ggarrange(gamma_to, gamma_from, align = "hv")
gamma_av_trans <- annotate_figure(gamma_av_trans, 
                                  top = text_grob("Delta average transitions", 
                                                  color = "black", face = "bold", size = 14))

gamma_av_trans

ururegion <-
        ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
        geom_tile(color = "white",
                  lwd = 1.5,
                  linetype = 1) +
        scale_fill_gradient2(
                low = "#075AFF",
                mid = "#FFFFCC",
                high = "#FF0000"
        )  +
        geom_text(aes(label = value), color = "black", size = 2) +
        coord_fixed() +
        theme(
                axis.text.x = element_text(
                        angle = 90,
                        vjust = 0.5,
                        hjust = 1,
                        size = 12
                ),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 12),
                legend.text = element_text(size = 10),
                legend.title = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()
        )  +
        xlab("To") + ylab("From") + ggtitle("Delta transition rates (%)")
ururegion  

introm <- as.data.frame(t(abs[8, ] / sum(abs[8, ]) * 100))
introm$subregion <- row.names(introm)
row.names(introm) <- NULL
introm <- introm[order(introm$Metropolitan, decreasing = F), ]
introm <- select(introm, subregion, Metropolitan)
colnames(introm) <- c("UY_regions", "rate")
introm$rate <- round(introm$rate, 1)
introm <- introm[-1, ]
introm
dim(introm)

bar_i <-
        ggplot(introm, aes(
                x = reorder(UY_regions, rate),
                y = rate,
                fill = UY_regions
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = rate), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 16, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("To Metropolitan")
bar_i

difm <-  as.data.frame(abs[, 8] / sum(abs[, 8]) * 100)
row.names(difm) <- row.names(abs)
difm$Subregion <- row.names(difm)
row.names(difm) <- NULL
colnames(difm) <- c("rate", "UY_regions")
difm <- select(difm, UY_regions, rate)
difm <- difm[order(difm$rate, decreasing = F), ]
difm <- difm[-1, ]
difm$rate <- round(difm$rate, 1)

bar_e <-
        ggplot(difm, aes(
                x = reorder(UY_regions, rate),
                y = rate,
                fill = UY_regions
        )) +
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = rate), hjust = 1, size = 3) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "Dry border" = "#9e7681",
                        "Rest of the country" = "#db40b8",
                        "Metropolitan" = "#e7c1d0",
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
        coord_flip() + ggtitle("From Metropolitan")
bar_e


df <- merge(introm, difm, by = "UY_regions")
colnames(df) <- c("UY_regions", "To", "From")

DT = melt(df,
          id.vars = c("UY_regions"),
          measure.vars = c("To", "From"))

bar_e <-
        ggplot(DT, aes(
                x = reorder(UY_regions, value),
                y = value,
                fill = variable
        )) +
        geom_bar(stat = 'identity', position = 'dodge') +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12, vjust = 1.5),
                axis.text.y = element_text(size = 12),
                legend.position = "right",
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                plot.title = element_text(size = 12)
        ) +
        scale_fill_manual(values = c("To" = "#FF0000",
                                     "From" = "#fff7b5")) +
        ylab("") + xlab("") + scale_y_continuous(breaks = seq(0, 60, by = 10)) +
        coord_flip() + ggtitle("Metropolitan area")
bar_e



fig2 <-
        ggarrange(
                ururegion,
                bar_e,
                ncol = 2,
                labels = c("C", "D"),
                heights = c(0.8, 0.5),
                widths = c(0.8, 0.5)
        )


fig <- ggarrange(fig1, fig2, nrow = 2)
fig

png(
        "Supplementary_6.png",
        res = 600,
        height = 25,
        width = 35,
        units = 'cm'
)
fig
dev.off()


