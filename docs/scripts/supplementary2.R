library(tidyverse)
library(stringr)
library(data.table)
library(sf)
library(viridis)
library(scico)
library(ggpubr)


setwd("~/Documentos/trabajo/paper_delta/scripts/supplementary_2/")

# Tables T04 & T03  were retrived on 2022-07-13 
t04 <- read.csv("./T04B_merge.csv", header = T)
dim(t04)
t042 <-
        t04 %>% mutate(date = ifelse(
                Fecha.de.hisopado %in% "",
                Fecha.de.diagnóstico,
                Fecha.de.hisopado
        ))


filt <- t042 %>% filter(date > "2020-11-01" & date < "2021-10-01")
filt <- filt[order(filt$date, decreasing = T), ]
dim(filt)

write.table(filt, "filtered_T04B.tsv", row.names = F, sep = "\t")

# Filter relevant dates

filtc <- t042 %>% filter(date > "2021-07-01" & date < "2021-10-01")
filtc$Variante.por.PCR <-
        gsub("No-VOC", "B.1.617.2", filtc$Variante.por.PCR) #non-voc samples were assigned to delta in the vpcrs from July to September and was decided to be replaced for visualization purposes

filt <-
        filt[!filt$Identificador.de.Consorcio %in% filtc$Identificador.de.Consorcio,]

filt <- as.data.frame(rbind(filt, filtc))
dim(as.data.frame(unique(filt$Identificador.de.Consorcio)))
filt <- filt[order(filt$date, decreasing = T),]


# Get samples that were screened by the vPCR

vPCR <- filt[which(filt$PCR.Variantes == "Si"),]
vPCR2 <- vPCR %>% filter(date > "2021-03-01" & date < "2021-07-15")


# Percenage of VOCs by vPCR sample detection in July 2021
j <- vPCR %>% filter(date > "2021-07-01" & date < "2021-07-31")
jj <- j %>% group_by(Variante.por.PCR) %>%
        summarise(count = n())
jj$pct <- jj$count/sum(jj$count) *100
jj

# Percenage of VOCs by vPCR sample detection in September 2021
s <- vPCR %>% filter(date > "2021-09-01" & date < "2021-09-30")
ss <- s %>% group_by(Variante.por.PCR) %>%
        summarise(count = n())
ss$pct <- ss$count/sum(ss$count) *100
ss


# Distribution by departamento

table(vPCR$Departamento)


list <- vPCR %>%
        group_by(date, Variante.por.PCR) %>%
        tally() %>%
        ungroup()
list <- as.data.frame(list)
list$date <- as.Date(list$date)



# Edit names

list$Variante.por.PCR <-
        gsub("Descartada", "Discarded", list$Variante.por.PCR)
list$Variante.por.PCR <-
        gsub("No concluyente", "Inconclusive", list$Variante.por.PCR)

list2 <- list %>%
        filter(date > "2021-02-27" & date < "2021-07-31")
unique(list2$Variante.por.PCR)
gamma <- list2[which(list2$Variante.por.PCR == "P.1/B.1.351"),]


# Percentage of Gamma VOC detected with vPCR 
sum(gamma$n)/sum(list2$n)*100


list3 <- list %>%
        filter(date > "2021-07-01" & date < "2021-09-30")
list3$Variante.por.PCR <-
        gsub("No-VOC", "B.1.617.2", list3$Variante.por.PCR) # The no-VOC PCR was compatible with B.1.617.2 during this period of time.
unique(list3$Variante.por.PCR)
delta <- list3[which(list3$Variante.por.PCR == "B.1.617.2"),]
sum(delta$n) / sum(list3$n) * 100
gamma <- list3[which(list3$Variante.por.PCR == "P.1/B.1.351"),]
sum(gamma$n) / sum(list3$n) * 100



s3 <-
        ggplot(list, aes(fill = Variante.por.PCR, y = log10(n), x = date)) +
        geom_bar(position = "stack",
                 stat = "identity",
                 alpha = 1) +
        scale_fill_manual(
                values = c(
                        "B.1.617.2" = "#9b2f36",
                        "P.1/B.1.351" = "#ffc3be",
                        "Discarded" = "#df9f36",
                        "Inconclusive" = "#468471",
                        "No-VOC" = "#2d5192"
                )
        ) +
        theme(
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(
                        angle = 65,
                        hjust = 1,
                        size = 12
                ),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 12),
                legend.position = "top",
                legend.box.just = "center",
                legend.key.width = unit(1, "lines")
        ) +
        scale_x_date(date_breaks = "1 months") +
        ggtitle("") +
        guides(fill = guide_legend(title = "vPCR variant", nrow = 3, ncol = 2)) +
        ylab("Log10 scale vPCR tests") + xlab("Date")
s3



depart_uy <-
        st_read("ine_depto-20231103T191932Z-001/ine_depto/") #Retrived from https://github.com/tereom/taller-mapas-mv/blob/master/material.zip?raw=true
class(depart_uy)

dim(depart_uy)
unique(depart_uy$NOMBRE)

df <- read.table("input_vPCR.tsv", sep = "\t", header = T)
table(vPCR$Departamento)
df$Departament <- toupper(df$Departament)
colnames(df)[1] <- c("NOMBRE")
unique(df$NOMBRE)
df$NOMBRE <- gsub("RÍO NEGRO", "RIO NEGRO", df$NOMBRE)
df$NOMBRE <- gsub("SAN JOSÉ", "SAN JOSE", df$NOMBRE)
df$NOMBRE <- gsub("TACUAREMBÓ", "TACUAREMBO", df$NOMBRE)
df$NOMBRE <- gsub("PAYSANDÚ", "PAYSANDU", df$NOMBRE)

depart_uy <- merge(depart_uy, df, by = "NOMBRE")


# Create Uruguay Map

vpcrt <- ggplot(depart_uy) +
        geom_sf(aes(fill = Count)) +  scico::scale_fill_scico(direction = -1, palette = "lajolla") +
        geom_sf_label(aes(label = NOMBRE),
                      alpha = 0.1,
                      color = "#9b2f36") +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                legend.position = "left"
        ) +
        guides(fill = guide_legend(title = "SARS-CoV-2 vPCR tests"))
vpcrt


# Rename samples

vPCRr <- as.data.frame(table(vPCR2$Variante.por.PCR))
vPCRr <- vPCRr[order(vPCRr$Freq, decreasing = T),]
vPCRr$Var1 <- gsub("Descartada", "Discarded", vPCRr$Var1)
vPCRr$Var1 <- gsub("No concluyente", "Inconclusive", vPCRr$Var1)


# Create count of vPCR test
vPCRr$pct <- round(vPCRr$Freq / sum(vPCRr$Freq) * 100, 0)

bar_c <-
        ggplot(vPCRr, aes(
                x = reorder(Var1, pct),
                y = pct,
                fill = Var1
        )) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = pct), hjust = -0.8, size = 3.5) +
        theme(
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 14, vjust = 1.5),
                axis.text.y = element_text(size = 14),
                legend.position = "none"
        ) +
        scale_fill_manual(
                values = c(
                        "B.1.617.2" = "#9b2f36",
                        "P.1/B.1.351" = "#ffc3be",
                        "Discarded" = "#df9f36",
                        "Inconclusive" = "#468471",
                        "No-VOC" = "#2d5192"
                )
        ) +
        ylab("Number of tests (%)") + xlab("vPCR results") +
        ggtitle("Variant screening from March to mid-July") +
        coord_flip()
bar_c

sup <-
        ggarrange(
                s3,
                bar_c,
                widths = c(0.55, 0.45),
                heights = c(0.55, 0.45),
                nrow = 2,
                labels = c("B", "C"),
                align = "hv"
        )
sup

sup1 <-
        ggarrange(
                vpcrt,
                sup,
                widths = c(0.6, 0.4),
                heights = c(0.6, 0.4),
                labels = c("A", ""),
                align = "h"
        )
sup1

dir.create("Figures")
png(
        "./Figures/sup1_vpcr2.png",
        res = 600,
        height = 25,
        width = 50,
        units = 'cm'
)
sup1
dev.off()


