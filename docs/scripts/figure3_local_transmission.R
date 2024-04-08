library(ggplot2)
library(dplyr)
library(ggnewscale)
library(readxl)
library(maps)
library(geosphere)
library(plyr)
library(data.table)
library(ggthemes)
library(reshape2)
library(lubridate)
library(ggpmisc)

# Set working directory
setwd('/Users/giraola/Documents/05_covid/paper/figure3_local_transmission/')

##################################
# Average rates vs. Pop. density #
##################################

popdens   <- read.csv('departments_popdens.csv', sep = ',', header = T)
sub.rates <- as.data.frame(read_xlsx('subregion_joint_tree_transitions.xlsx'))
rownames(sub.rates) <- sub.rates[,1]
sub.rates <- sub.rates[,-1]
rmv.cols <- c('Africa', 'Argentina', 'Asia', 'Brazil', 'Europe', 'NorthAmerica',
              'Oceania', 'SouthAmerica', 'Uruguay')
rmv.idx  <- which(colnames(sub.rates)%in%rmv.cols)
sub.rates2 <- sub.rates[-rmv.idx, -rmv.idx]
mean.rates <- apply(sub.rates2, 1, mean)
mean.rates <- as.data.frame(cbind(names(mean.rates), as.numeric(mean.rates)))
colnames(mean.rates) <- c('Department', 'Rate')

merged <- merge(popdens, mean.rates, by.x = 'Department', by.y = 'Department')
set.seed(324)
depcol <- viridis_pal(option = "H")(19)
depart <- c('Artigas', 'Canelones', 'Cerro Largo', 'Colonia', 'Durazno', 'Flores', 
            'Florida', 'Lavalleja', 'Maldonado', 'Montevideo', 'Paysandú', 'Río Negro',
            'Rivera', 'Rocha', 'Salto', 'San José', 'Soriano', 'Tacuarembó', 'Treinta y Tres')

g1 <- ggplot(merged, aes(x = log(PopDensity), y = log(as.numeric(Rate)))) +
        geom_point(aes(size = AcumCases, color = AcumCases), shape = 19, alpha = 1) +
        scale_size(range = c(.8,8), name = 'Accumulated \ncases') +
        theme_bw() +
        xlab(bquote('Log Population density'~(pop/km^2))) +
        ylab('Log Transmission rate') +
        scale_color_distiller(guide = 'none', trans = 'reverse', palette = 'Reds') +
        theme(legend.position = 'none',
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_text(size = 12)) +
        geom_smooth(method = 'lm', 
                    colour = 'black',
                    alpha = .3) +
                    stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), parse = TRUE)

png('pop_dens_corelation.png', res = 600, height = 10, width = 10, units = 'cm')
g1
dev.off()


##################################
# Transmission rates departments #
##################################

depart2 <- c('Lavalleja', 'Durazno', 'Artigas', 'Rivera', 'Florida', 'Paysandú', 'Flores',
             'Soriano', 'Colonia', 'Tacuarembó', 'Río Negro', 'San José', 'Treinta y Tres',
             'Cerro Largo', 'Salto', 'Maldonado', 'Rocha', 'Canelones', 'Montevideo')
g2 <- ggplot(merged, aes(x = reorder(Department, as.numeric(Rate)),
                         y = as.numeric(Rate),
                         fill = Department)) + 
      geom_bar(stat="identity", alpha = .75) +
      coord_flip() +
      theme_bw() +
      ylab('Transmission rate') +
      xlab('Department') +
      scale_fill_manual(values = depcol) +
      scale_x_discrete(labels = depart2) +
      theme(legend.position = 'none',
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12))

##################################
# Gamma transmission departments #
##################################

gamma.sub.rates <- as.data.frame(read_xlsx('subregion_gamma_subtree_transitions.xlsx'))
rownames(gamma.sub.rates) <- gamma.sub.rates[,1]
gamma.sub.rates <- gamma.sub.rates[,-1]
rmv.cols <- c('Africa', 'Argentina', 'Asia', 'Brazil', 'Europe', 'NorthAmerica',
              'Oceania', 'SouthAmerica', 'Uruguay')
rmv.idx  <- which(colnames(gamma.sub.rates)%in%rmv.cols)
gamma.sub.rates2 <- gamma.sub.rates[-rmv.idx, -rmv.idx]
gamma.mean.rates <- apply(gamma.sub.rates2, 1, function(x){mean(x, na.rm = T)})
gamma.mean.rates <- as.data.frame(cbind(names(gamma.mean.rates), as.numeric(gamma.mean.rates)))
colnames(gamma.mean.rates) <- c('Department', 'Rate')

g2 <- ggplot(gamma.mean.rates, aes(x = reorder(Department, as.numeric(Rate)),
                                   y = as.numeric(Rate),
                                   fill = Department)) +
        ggtitle('Gamma') +
        geom_bar(stat="identity", alpha = .75) +
        coord_flip() +
        theme_bw() +
        ylab('Transmission rate') +
        xlab('') +
        scale_fill_manual(values = rep('firebrick4', 19)) +
        scale_x_discrete(labels = depart2) +
        scale_y_continuous(breaks = round(seq(0, 0.5, len = 3), digits = 1)) +
        theme(legend.position = 'none',
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12))

png('gamma_departments_rates.png', res = 600, height = 10, width = 7, units = 'cm')
g2
dev.off()

##################################
# Delta transmission departments #
##################################

delta.sub.rates <- as.data.frame(read_xlsx('subregion_delta_subtree_transitions.xlsx'))
rownames(delta.sub.rates) <- delta.sub.rates[,1]
delta.sub.rates <- delta.sub.rates[,-1]
rmv.cols <- c('Africa', 'Argentina', 'Asia', 'Brazil', 'Europe', 'NorthAmerica',
              'Oceania', 'SouthAmerica', 'Uruguay')
rmv.idx  <- which(colnames(delta.sub.rates)%in%rmv.cols)
delta.sub.rates2 <- delta.sub.rates[-rmv.idx, -rmv.idx]
delta.mean.rates <- apply(delta.sub.rates2, 1, function(x){mean(x, na.rm = T)})
delta.mean.rates <- as.data.frame(cbind(names(delta.mean.rates), as.numeric(delta.mean.rates)))
colnames(delta.mean.rates) <- c('Department', 'Rate')

g3 <- ggplot(delta.mean.rates, aes(x = reorder(Department, as.numeric(Rate)),
                                   y = as.numeric(Rate),
                                   fill = Department)) +
  ggtitle('Delta') +
  geom_bar(stat="identity", alpha = .75) +
  coord_flip() +
  theme_bw() +
  ylab('Transmission rate') +
  xlab('') +
  scale_fill_manual(values = rep('gold2', 19)) +
  scale_x_discrete(labels = depart2) +
  scale_y_continuous(breaks = seq(0, 0.3, len = 4)) +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

png('delta_departments_rates.png', res = 600, height = 10, width = 7, units = 'cm')
g3
dev.off()

##########################
# Transmission rates map #
##########################

dept <- load_geouy(c = 'Departamentos')
deptos <- dept$nombre
deptos[1] <- 'Montevideo'
deptos[2] <- 'Artigas'
deptos[3] <- 'Canelones'
deptos[4] <- 'Colonia'
deptos[5] <- 'Durazno'
deptos[6] <- 'Florida'
deptos[7] <- 'Lavalleja'
deptos[8] <- 'Paysandu'
deptos[9] <- 'RioNegro'
deptos[10] <- 'Rivera'
deptos[11] <- 'Rocha'
deptos[12] <- 'Salto'
deptos[13] <- 'SanJose'
deptos[14] <- 'Soriano'
deptos[15] <- 'TreintayTres'
deptos[16] <- 'Tacuarembo'
deptos[17] <- 'Flores'
deptos[18] <- 'Maldonado'
deptos[19] <- 'CerroLargo'

rank1 <- merged[order(as.numeric(merged$Rate)),c(1,4)]

rank.data <- st_geometry(dept) %>% 
  st_point_on_surface() %>% 
  st_coordinates() 

rank.data <- cbind(rank.data, deptos)
rank.data <- merge(rank.data, rank1, by.x = 'deptos', by.y = 'Department')
rank2 <- rank(rank.data$Rate)
rank.data$ranking <- abs(rank2-19)+1

colnames(rank.data) <- c('Department', 'X', 'Y', 'Rate', 'Ranking')

yend_rank1 <- min(as.numeric(rank.data$Y))-30000
yend_rank  <- NULL

for (x in 1:19) {
  yend_rank2 <- as.numeric(yend_rank1) + 30000
  yend_rank  <- c(yend_rank, yend_rank2)
  yend_rank1 <- yend_rank2
}

rank.data$yend_rank <- yend_rank[rank.data$Ranking]
rank.data$xend <- rep(0, 19)
rank.data$x_axis_start <- rank.data$xend + 10
rank.data$Norm_rank2 <- normalize(as.numeric(rank.data$Ranking), range = c(-40.2, -10.2), method = "range")

rank.data$Norm_rate <- normalize(as.numeric(rank.data$Rate), range = c(first(as.numeric(rank.data$x_axis_start)), 80), method = "range")

# Cases in Uruguay from GUIAD GitHub
cases <- read.csv('https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY_porDepto_detalle.csv')
acum1 <- cases[which(dmy(cases$fecha) == '2021-09-30'),c('departamento','acumcasos')]
acum1$code <- gsub('-', '', gsub(')', '', unlist(lapply(acum1$departamento, function(x){strsplit(x, '(', fixed = T)[[1]][2]}))))
acum2 <- acum1[,c(2,3)]

dept2 <- dept %>% 
          left_join(acum2, by = c('depto' = 'code'))

g4 <- ggplot() + 
        geom_sf(data = dept2,
                size = .1,
                aes(fill = acumcasos),
                color = 'black') +
                coord_sf(x = c(min(dept$x)-700000, max(dept$x)+170000)) +
                scale_fill_distiller(trans = 'reverse', 
                                     palette = 'Reds',
                                     name = 'Accumulated \ncases') +
        geom_sigmoid(data = rank.data,
                     aes(x = as.numeric(X),
                         y = as.numeric(Y), 
                         xend = x_axis_start + 45000, 
                         yend = yend_rank, 
                         color = Department, 
                         group = Department), 
                     alpha = .3,
                     smooth = 8,
                     size = 1) +
        geom_point(data = rank.data,
                   aes(x = as.numeric(X),
                       y = as.numeric(Y),
                       color = Department),
                   size = 1) +
        geom_text(data = rank.data,
                  aes(x = x_axis_start + 45000,
                      y = yend_rank + 12000,
                      label = namesok,
                      color = Department),
                  hjust = 0,
                  size = 3) +
        geom_segment(data = rank.data, 
                     aes(x = x_axis_start + 40000 ,
                         y = yend_rank + 11000,
                         xend = Norm_rate*-5000,
                         yend = yend_rank + 11000,
                         color = Department), 
                     alpha = .8,
                     size = 3) +
                     scale_color_discrete(guide = 'none') +
        theme_void()

png('map_rates_departments.png', res = 600, height = 15, width = 22, units = 'cm')
g4
dev.off()
