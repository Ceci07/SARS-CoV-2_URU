library(tidyverse)
library(stringr)
library(ggpubr)
library(lubridate)
library(data.table)

setwd("~/Documentos/trabajo/paper_delta/scripts/supplementary_1/")


# Epidemiological stats
#### data was retrived from https://github.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/

df <-
        read.csv("datos-y-visualizaciones-GUIAD/datos/estadisticasUY.csv",
                 header = T)
df[grepl("30/11/20", df$fecha), ]

df$fecha <-  strptime(as.character(df$fecha), "%d/%m/%Y")
df$fecha <- as.Date(df$fecha)
df <- df %>% filter(fecha > "2020-03-01" & fecha < "2021-10-01")

colnames(df)[5] <- c("Cases")
colnames(df)[10] <- c("UCI")
colnames(df)[8] <- c("Deaths")
colnames(df)[19] <- c("Positivity")
colnames(df)[14] <- c("Tests")

dt <- df %>%
        select(fecha, Cases, UCI, Deaths) %>%
        gather(key = "variable", value = "value",-fecha)
head(dt, 3)



fill.order <-
        factor(dt$variable, levels = c('Tests', 'Cases', 'UCI', 'Deaths'))

c <- ggplot(dt, aes(x = as.Date(fecha), y = value)) +
        geom_area(aes(fill = fill.order),
                  alpha = 0.8,
                  position = position_dodge(1)) +
        scale_fill_manual(
                values = c(
                        "Cases" = "#00c0c1",
                        "UCI" = "#ffca33",
                        "Deaths" = "#a03b47",
                        "Tests" = "#d0ead0"
                )
        ) +
        theme(
                axis.line = element_line(colour = "grey"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5, size = 10),
                axis.text.y = element_text(size = 10),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                legend.text = element_text(size = 16),
                text = element_text(color = "#433236"),
                legend.title = element_blank(),
                legend.position = "top",
                legend.box.just = "center"
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2020-12-05")),
                linetype = 3,
                color = "#433236"
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2021-05-01")),
                linetype = 3,
                color = "#c1121f"
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2021-02-27")),
                color = "grey",
                linetype = 3
        ) +
        annotate(
                "text",
                x = as.Date("2020-12-10"),
                y = 4500,
                label = "Gamma",
                angle = 90,
                size = 5,
                color = "#433236"
        ) +
        annotate(
                "text",
                x = as.Date("2021-05-08"),
                y = 4500,
                label = "Delta",
                angle = 90,
                size = 5,
                color = "#c1121f"
        ) +
        annotate(
                "text",
                x = as.Date("2021-03-04"),
                y = 3500,
                label = "Vaccination campaign",
                angle = 90,
                size = 5,
                color = "grey"
        ) +
        xlab("Date") + ylab("Absolute count per day") +
        guides(fill = guide_legend(reverse = TRUE))
c

png(
        "Supplementary_1.png",
        res = 800,
        heigh = 20,
        width = 25,
        units = 'cm'
)
c
dev.off()



options(scipen = 999)

dc <- select(df, fecha, Cases)
tt <- dc %>% filter(fecha > "2020-03-01" & fecha < "2020-11-30")
sumc <- sum(tt$Cases)


casespermil <- sumc / 3.429


# Cases per million by November
casespermil

dp <- select(df, fecha, Positivity)
tt <- dp %>% filter(fecha > "2020-03-01" & fecha < "2020-11-30")
meanp <- mean(tt$Positivity)
sdp <- sd(tt$Positivity)


# Mean and SD of positivity rate March - November 2020

meanp * 100
sdp * 100

dp <- select(df, fecha, Positivity)
tt <- dp %>% filter(fecha > "2020-11-01" & fecha < "2020-11-30")
meanp <- mean(tt$Positivity)
sdp <- sd(tt$Positivity)


# Mean and SD of positivity rate in November 2020
meanp * 100
sdp * 100

dp <- select(df, fecha, Positivity)
tt <- dp %>% filter(fecha > "2020-12-01" & fecha < "2020-12-31")
meanp <- mean(tt$Positivity)
sdp <- sd(tt$Positivity)

# Mean and SD of positivity rate in December (after the estimated emergence of P.6)

meanp * 100
sdp * 100


dp <- select(df, fecha, Positivity)
tt <- dp %>% filter(fecha > "2021-01-01" & fecha < "2021-06-30")
meanp <- mean(tt$Positivity)
sdp <- sd(tt$Positivity)

# Mean and SD of positivity rate during the Gamma wave
meanp * 100
sdp * 100


### Previous emergenece of P.6
dff <- select(df, fecha, Cases)
dff <- select(df, fecha, Cases, Deaths, UCI, Positivity, Tests)

dff <- dff %>%
        pivot_longer((cols = everything()[-1]))

t1 <- dff %>% filter(fecha > "2020-03-01" & fecha < "2020-11-13") %>%
        group_by(name) %>%
        summarise(
                mean = mean(value),
                SD = sd(value),
                min = min(value),
                max = max(value)
        )
t1$period <- c("Before P.6")


### After P.6 emergence and before Gamma introduction

dff <- select(df, fecha, Cases, Deaths, UCI, Positivity, Tests)

dff <- dff %>%
        pivot_longer((cols = everything()[-1]))
head(dff)

t2 <- dff %>% filter(fecha > "2020-11-14" & fecha < "2020-12-13") %>%
        group_by(name) %>%
        summarise(
                mean = mean(value),
                SD = sd(value),
                min = min(value),
                max = max(value)
        )
t2$period <- c("After P.6 before Gamma")


### After Gamma introduction and before Delta introduction
dff <- select(df, fecha, Cases, Deaths, UCI, Positivity, Tests)

dff <- dff %>%
        pivot_longer((cols = everything()[-1]))

t3 <- dff %>% filter(fecha > "2020-12-14" & fecha < "2021-04-26") %>%
        group_by(name) %>%
        summarise(
                mean = mean(value),
                SD = sd(value),
                min = min(value),
                max = max(value)
        )
t3$period <- c("After Gamma before Delta")


### After Delta introduction
dff <- select(df, fecha, Cases, Deaths, UCI, Positivity, Tests)

dff <- dff %>%
        pivot_longer((cols = everything()[-1]))

t4 <- dff %>% filter(fecha > "2021-04-27" & fecha < "2021-09-30") %>%
        group_by(name) %>%
        summarise(
                mean = mean(value),
                SD = sd(value),
                min = min(value),
                max = max(value)
        )
t4$period <- c("After Delta")

dt <- as.data.frame(rbind(t1, t2, t3, t4))

cases <- dt[which(dt$name == "Cases"), ]
cases$period <-
        factor(
                cases$period,
                levels = c(
                        "Before P.6",
                        "After P.6 before Gamma",
                        "After Gamma before Delta",
                        "After Delta"
                )
        )


coul <- c("#dc98b4",
          "#d545aa",
          "#a15372",
          "#da4370")

bar1 <- ggplot(cases, aes(x = period, y = mean, fill = period)) +
        geom_bar(stat = "identity") +
        ylab("Mean number of cases during the period") +
        scale_fill_manual(values = coul, name = "Period") +
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x = element_blank()
        )
bar1

pos <- dt[which(dt$name == "Positivity"), ]
pos$period <-
        factor(
                pos$period,
                levels = c(
                        "Before P.6",
                        "After P.6 before Gamma",
                        "After Gamma before Delta",
                        "After Delta"
                )
        )

coul <- c("#dc98b4",
          "#d545aa",
          "#a15372",
          "#da4370")

bar2 <- ggplot(pos, aes(x = period, y = mean * 100, fill = period)) +
        geom_bar(stat = "identity") +
        ylab("Mean positivity rate (%)") +
        scale_fill_manual(values = coul, name = "Period") +
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x = element_blank()
        )
bar2

tes <- dt[which(dt$name == "Tests"), ]
tes$period <-
        factor(
                tes$period,
                levels = c(
                        "Before P.6",
                        "After P.6 before Gamma",
                        "After Gamma before Delta",
                        "After Delta"
                )
        )

coul <- c("#dc98b4",
          "#d545aa",
          "#a15372",
          "#da4370")

bar3 <- ggplot(tes, aes(x = period, y = mean * 100, fill = period)) +
        geom_bar(stat = "identity") +
        ylab("Mean number of tests") +
        scale_fill_manual(values = coul, name = "Period") +
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x = element_blank()
        )
bar3

fig <-
        ggarrange(
                bar1,
                bar2,
                bar3,
                nrow = 3,
                widths = c(0.5, 0.5),
                heights = c(0.5, 0.5),
                align = "hv",
                common.legend = T
        )
fig


png(
        "Supplementary_s1.png",
        res = 800,
        heigh = 20,
        width = 25,
        units = 'cm'
)
fig
dev.off()


df <-
        read.csv("datos-y-visualizaciones-GUIAD/datos/estadisticasUY.csv",
                 header = T)
df$fecha <-  strptime(as.character(df$fecha), "%d/%m/%Y")
df$fecha <- as.Date(df$fecha)

colnames(df)[6] <- c("Cases")
colnames(df)[10] <- c("UCI")
colnames(df)[8] <- c("Deaths")
colnames(df)[19] <- c("Positivity")

dtt <- df %>%
        select(fecha, Positivity) %>%
        gather(key = "variable", value = "value",-fecha)
head(dtt, 3)


### Positivity rate

t <-
        dtt %>% filter(fecha > "2020-03-01" &
                               fecha < "2020-11-13") # Before P.6 emergence
mean(t$value) * 100
sd(t$value) * 100

tt <-
        dtt %>% filter(fecha > "2020-11-14" &
                               fecha < "2020-12-14") # After P.6 emergence and before Gamma introduction
mean(tt$value) * 100
sd(tt$value) * 100

ttt <-
        dtt %>% filter(fecha > "2020-12-15" &
                               fecha < "2021-04-27") # After Gamma introduction and before Delta introduction
mean(ttt$value) * 100
sd(ttt$value) * 100

tttt <-
        dtt %>% filter(fecha > "2021-04-28" &
                               fecha < "2021-09-30") # After Delta introduction
mean(tttt$value) * 100
sd(tttt$value) * 100

d <- dtt %>% filter(fecha > "2020-03-01" & fecha < "2021-09-30")


cc <- ggplot(d, aes(x = as.Date(fecha), y = value)) +
        geom_area(aes(fill = variable), alpha = 0.8, position = position_dodge(1)) +
        scale_fill_manual(values = c("#1d3557")) +
        theme(
                axis.line = element_line(colour = "grey"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(hjust = 0.5, size = 10),
                axis.text.y = element_text(size = 10),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                legend.text = element_text(size = 16),
                text = element_text(color = "#433236"),
                legend.title = element_blank(),
                legend.position = "none",
                legend.box.just = "center"
        ) +
        scale_x_date(date_breaks = "3 months") +
        geom_vline(
                xintercept = as.numeric(as.Date("2020-12-05")),
                linetype = 3,
                color = "#433236"
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2021-05-01")),
                linetype = 3,
                color = "#c1121f"
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2021-02-27")),
                color = "grey",
                linetype = 3
        ) +
        xlab("Date") + ylab("Positivity rate") +
        guides(fill = guide_legend(reverse = TRUE))
cc


fig <-
        ggarrange(
                c,
                cc,
                widths = c(0.7, 0.3),
                heights = c(0.7, 0.3),
                nrow = 2,
                labels = c("A", "B"),
                align = "hv"
        )
fig

png(
        "Supplementary_1_both.png",
        res = 800,
        heigh = 30,
        width = 25,
        units = 'cm'
)
fig
dev.off()



# Vaccination campaign
#### data was retrived from https://catalogodatos.gub.uy/dataset/vacunacion-por-covid-19/resource/5c549ba0-126b-45e0-b43f-b0eea72cf2cf

df <- read.table("actos_vacunales.csv", header = T, sep = ";")

df$Fecha2 <- dmy(df$Fecha)
df$Fecha <- NULL
colnames(df)[166] <- c("Fecha")

df <- df %>% filter(Fecha > "2021-02-27" & Fecha < "2021-07-31")


dff <-
        select(df,
               Total.Dosis.1,
               Total.Dosis.2,
               Total.Dosis.3,
               Total.Dosis.4,
               Fecha)

data_long <-
        gather(dff,
               condition,
               measurement,
               Total.Dosis.1:Total.Dosis.4,
               factor_key = TRUE)
data_long

# Total doses between March and July 2021
dt <- data_long %>% group_by(condition) %>%
        summarise(sum = sum(measurement))
dt

dt$condition <- gsub("Total.Dosis.", "Dose ", dt$condition)
dt$pct <- dt$sum / 3429000 * 100

cl <- c("#eac294",
        "#a1d4ff",
        "#9fb281",
        "#85e8f3",
        "#d3fecd")
dosis <-
        ggplot(data = dt, aes(x = condition, y = pct, fill = condition)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme(
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                legend.position = "none",
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16)
        ) +
        ylab("Vaccinated popullation (%)") + xlab("") +
        scale_y_continuous(breaks = seq(0, 100, 10)) +
        scale_fill_manual(values = cl)
dosis

sino <-
        select(
                df,
                Fecha,
                X1era.Dosis.Sinovac,
                X2da.Dosis.Sinovac,
                X3era.Dosis.Sinovac,
                X4ta.Dosis.Sinovac
        )
head(sino)
tail(sino)
colnames(sino) <-
        c("Fecha", "Primera", "Segunda", "Tercera", "Cuarta")
sino$tipo <- c("Sinovac")

pfizer <-
        select(
                df,
                Fecha,
                X1era.Dosis.Pfizer,
                X2da.Dosis.Pfizer,
                X3era.Dosis.Pfizer,
                X4ta.Dosis.Pfizer
        )
head(pfizer)
colnames(pfizer) <-
        c("Fecha", "Primera", "Segunda", "Tercera", "Cuarta")
pfizer$tipo <- c("Pfizer")

astra <-
        select(
                df,
                Fecha,
                X1era.Dosis.Astrazeneca,
                X2da.Dosis.Astrazeneca,
                X3era.Dosis.Astrazeneca,
                X4ta.Dosis.Astrazeneca
        )
head(astra)
colnames(astra) <-
        c("Fecha", "Primera", "Segunda", "Tercera", "Cuarta")
astra$tipo <- c("AstraZeneca")


tab <- as.data.frame(rbind(sino, pfizer, astra))
tab2 <-
        gather(tab, condition, measurement, Primera:Cuarta, factor_key = TRUE)
tab2$pct <- tab2$measurement / 3429000 * 100

tab3 <- tab2 %>% group_by(tipo, condition) %>%
        summarise(sum = sum(measurement))

# Total doses per type of vaccine and dose
tab3

# Percentage of the popullation covered by the vaccines per type
tab3$pct <- tab3$sum / 3429000 * 100
tab3 <- as.data.frame(tab3)
tab3


tab3$condition <- gsub("Primera", "First", tab3$condition)
tab3$condition <- gsub("Segunda", "Second", tab3$condition)
tab3$condition <- gsub("Tercera", "Third", tab3$condition)
tab3$condition <- gsub("Cuarta", "Fourth", tab3$condition)


cl <- c("#e6b8b3",
        "#a2d5d6",
        "#d1bbdf",
        "#cad5b3",
        "#aac4e2")
tab3$condition <-
        factor(tab3$condition, levels = c("First", "Second", "Third", "Fourth"))
ty <- ggplot(data = tab3, aes(x = tipo, y = pct, fill = condition)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme(
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16)
        ) +
        scale_fill_manual(values = cl, name = c("Dose")) +
        ylab("Vaccinated popullation (%)") + xlab("")
ty


tab <- select(tab, Fecha, tipo, Primera, Segunda, Tercera, Cuarta)
long <-
        melt(setDT(tab),
             id.vars = c("tipo", "Fecha"),
             variable.name = "dosis")
long <- as.data.frame(long)
head(long)
long$pct <- long$value / 3429000 * 100


# Vaccine immunization per type between March and July 2021
cl <- c("#e6b8b3",
        "#a2d5d6",
        "#d1bbdf",
        "#cad5b3",
        "#aac4e2")

long$dosis <- gsub("Primera", "First", long$dosis)
long$dosis <- gsub("Segunda", "Second", long$dosis)
long$dosis <- gsub("Tercera", "Third", long$dosis)
long$dosis <- gsub("Cuarta", "Fourth", long$dosis)

long$dosis <-
        factor(long$dosis, levels = c("First", "Second", "Third", "Fourth"))

p <- ggplot(long, aes(x = Fecha, y = value, fill = dosis)) +
        geom_area(alpha = 1) +
        facet_wrap( ~ tipo, nrow = 3) +
        theme(
                axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank()
        ) +
        scale_fill_manual(values = cl, name = "Dose") +
        ylab("Delivered doses")
p

s <- ggarrange(dosis, ty, nrow = 2, labels = c("A", "B"))
fig <- ggarrange(s, p, ncol = 2, labels = c("", "C"))
fig


# Percentage of popullation covered by vaccination between March and July

mj <- long %>% filter(Fecha > "2021-02-27" & Fecha < "2021-07-31")
mj <- mj %>% group_by(dosis) %>%
        summarise(sum = sum(value))

mj$pct <- mj$sum / 3429000 * 100
mj


# Percentage of popullation covered by vaccination per type between March and September
mj <- long %>% filter(Fecha > "2021-02-27" & Fecha < "2021-09-30")
mj <- mj %>% group_by(tipo, dosis) %>%
        summarise(sum = sum(value))

mj$pct <- mj$sum / 3429000 * 100
mj

# Number of vaccine doses delivered between March and September 2021
long <- long %>% filter(Fecha > "2020-03-01" & Fecha < "2021-09-30")

d <- ggplot(long, aes(x = Fecha, y = value, fill = tipo)) +
        geom_area(aes(fill = tipo),
                  alpha = 0.8, position = position_dodge(0.5)) +
        scale_fill_manual(values = c(
                "Sinovac" = "#2ec4b6",
                "Pfizer" = "#ef233c",
                "AstraZeneca" = "#033f63"
        )) +
        theme(
                axis.line = element_line(colour = "grey"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_text(
                        angle = 65,
                        hjust = 1,
                        size = 10
                ),
                axis.text.y = element_text(size = 10),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                legend.text = element_text(size = 16),
                text = element_text(color = "#433236"),
                legend.title = element_blank(),
                legend.position = "right",
                legend.box.just = "center"
        ) +
        scale_x_date(date_breaks = "1 months") +
        geom_vline(
                xintercept = as.numeric(as.Date("2020-11-18")),
                color = "#433236",
                linetype = 3
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2020-12-05")),
                color = "#433236",
                linetype = 3
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2021-05-01")),
                color = "#433236",
                linetype = 3
        ) +
        geom_vline(
                xintercept = as.numeric(as.Date("2021-02-27")),
                color = "grey",
                linetype = 3
        ) +
        annotate(
                "text",
                x = as.Date("2020-11-22"),
                y = 40000,
                label = "P.6",
                angle = 90,
                size = 5,
                color = "#433236"
        ) +
        annotate(
                "text",
                x = as.Date("2020-12-09"),
                y = 40000,
                label = "Gamma",
                angle = 90,
                size = 5,
                color = "#433236"
        ) +
        annotate(
                "text",
                x = as.Date("2021-05-05"),
                y = 40000,
                label = "Delta",
                angle = 90,
                size = 5,
                color = "#433236"
        ) +
        annotate(
                "text",
                x = as.Date("2021-03-03"),
                y = 38000,
                label = "Vaccination campaign",
                angle = 90,
                size = 5,
                color = "grey"
        ) +
        ggtitle("Vaccine administration") +
        xlab("Date") + ylab("Doses")
d

png(
        "vaccination.png",
        res = 800,
        height = 20,
        width = 25,
        units = 'cm'
)
d
dev.off()




