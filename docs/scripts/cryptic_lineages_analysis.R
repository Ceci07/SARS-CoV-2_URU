library(ggplot2)
library(reshape2)
library(dplyr)
library(reshape2)
library(ggsignif)
library(ggpubr)
library(rstatix)
library(ggmap)
library(geouy)
library(scales)
library(cowplot)

source('./get_cryptic_lineages.R')

# Introduction to Uruguay

out_uy <- get_cryptic_lineages(tree = 'fixed_mugration_region/annotated_tree.nexus',
							   dates = 'dates.tsv',
							   location = 'Uruguay',
							   min.size = 1)

# Introductions to Uruguayan departments

montevideo <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							   	   dates = 'dates.tsv',
							       location = 'Montevideo',
							       min.size = 1)

maldonado <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							   	  dates = 'dates.tsv',
							      location = 'Maldonado',
							      min.size = 1)

rocha <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							  dates = 'dates.tsv',
							  location = 'Rocha',
							  min.size = 1)

colonia <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							    dates = 'dates.tsv',
							    location = 'Colonia',
							    min.size = 1)

artigas <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							    dates = 'dates.tsv',
							    location = 'Artigas',
							    min.size = 1)

rivera <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							   dates = 'dates.tsv',
							   location = 'Rivera',
							   min.size = 1)

florida <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							    dates = 'dates.tsv',
							    location = 'Florida',
							    min.size = 1)

soriano <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							    dates = 'dates.tsv',
							    location = 'Soriano',
							    min.size = 1)

rionegro <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							     dates = 'dates.tsv',
							     location = 'RioNegro',
							     min.size = 1)

lavalleja <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							      dates = 'dates.tsv',
							      location = 'Lavalleja',
							      min.size = 1)

durazno <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							    dates = 'dates.tsv',
							    location = 'Durazno',
							    min.size = 1)

canelones <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							      dates = 'dates.tsv',
							      location = 'Canelones',
							      min.size = 1)

flores <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							   dates = 'dates.tsv',
							   location = 'Flores',
							   min.size = 1)

salto <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							  dates = 'dates.tsv',
							  location = 'Salto',
							  min.size = 1)

cerrolargo <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							       dates = 'dates.tsv',
							       location = 'CerroLargo',
							       min.size = 1)

sanjose <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							    dates = 'dates.tsv',
							    location = 'SanJose',
							    min.size = 1)

paysandu <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							     dates = 'dates.tsv',
							     location = 'Paysandu',
							     min.size = 1)

tacuarembo <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							       dates = 'dates.tsv',
							       location = 'Tacuarembo',
							       min.size = 1)

treintaytres <- get_cryptic_lineages(tree = 'fixed_mugration_subregion/annotated_tree.nexus',
							         dates = 'dates.tsv',
							         location = 'TreintayTres',
							         min.size = 1)

montevideo$department <- rep('Montevideo', dim(montevideo)[1])
maldonado$department  <- rep('Maldonado', dim(maldonado)[1])
rocha$department      <- rep('Rocha', dim(rocha)[1])
colonia$department    <- rep('Colonia', dim(colonia)[1])
artigas$department    <- rep('Artigas', dim(artigas)[1])
rivera$department     <- rep('Rivera', dim(rivera)[1])
florida$department    <- rep('Florida', dim(florida)[1])
soriano$department    <- rep('Soriano', dim(soriano)[1])
rionegro$department   <- rep('RioNegro', dim(rionegro)[1])
lavalleja$department  <- rep('Lavalleja', dim(lavalleja)[1])
durazno$department    <- rep('Durazno', dim(durazno)[1])
canelones$department  <- rep('Canelones', dim(canelones)[1])
flores$department     <- rep('Flores', dim(flores)[1])
salto$department      <- rep('Salto', dim(salto)[1])
cerrolargo$department <- rep('CerroLargo', dim(cerrolargo)[1])
sanjose$department    <- rep('SanJose', dim(sanjose)[1])
paysandu$department   <- rep('Paysandu', dim(paysandu)[1])
tacuarembo$department <- rep('Tacuarembo', dim(tacuarembo)[1])
treintaytres$department <- rep('TreintayTres', dim(treintaytres)[1])

all.departments <- rbind(montevideo, maldonado, rocha, colonia, artigas, rivera, florida,
						 soriano, rionegro, lavalleja, durazno, canelones, flores, salto,
						 cerrolargo, sanjose, paysandu, tacuarembo, treintaytres)

all.dep.filtered <- all.departments[-which(all.departments$location.ancestral == 'Uruguay'), ]

vocs <- rep('No-VOC', dim(all.dep.filtered)[1])
vocs[grep('P.1', all.dep.filtered$lineage)] <- 'Gamma'
vocs[grep('B.1.1.7', all.dep.filtered$lineage)] <- 'Alpha'
vocs[grep('B.1.351', all.dep.filtered$lineage)] <- 'Beta'
vocs[grep('B.1.617.2', all.dep.filtered$lineage)] <- 'Delta'
vocs[grep('AY.', all.dep.filtered$lineage)] <- 'Delta'

all.dep.filtered$VOCs <- vocs

region <- rep('Rest of the country', dim(all.dep.filtered)[1])
region[which(all.dep.filtered$department%in%c('Artigas', 'Rocha', 'CerroLargo', 'TreintayTres', 'Rivera'))] <- 'Dry border'
region[which(all.dep.filtered$department%in%c('Montevideo', 'Canelones'))] <- 'Metropolitan'

all.dep.filtered$region <- region
all.dep.filtered2 <- all.dep.filtered %>% 
                      filter(VOCs != 'Alpha') %>%
                      filter(cluster.size > 1) %>%
                      mutate_at('time.detection', as.numeric)

all.dep.filtered2$VOCs <- factor(all.dep.filtered2$VOCs, levels = rev(levels(factor(all.dep.filtered2$VOCs))))
map_levels <- c("***" = 1e-4, "**" = 1e-3, "*"= 1e-2)

anno_dry <- compare_means(
            data = all.dep.filtered2 %>% filter(region == 'Dry border'),
            time.detection ~ VOCs)

anno_res <- compare_means(
            data = all.dep.filtered2 %>% filter(region == 'Rest of the country'),
            time.detection ~ VOCs,
            method = 'wilcox.test',
            p.adjust.method = 'none')

p1 <- ggplot(all.dep.filtered2, aes(x = region, 
                                    y = time.detection, 
                                    colour = VOCs)) +
        geom_point(size = 1, position = position_jitterdodge(jitter.width = .15)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.01) +
        theme_classic() +
        ylab('Time of cryptic circulation (days)') +
        xlab('') +
        theme(legend.position = 'top',
              axis.text = element_text(size = 11),
              axis.text.x = element_text(colour = c('mediumblue', 'blueviolet', 'deeppink')),
              axis.title = element_text(size = 12)) +
        scale_colour_manual(values = c('grey', 'orange', 'firebrick3')) +
        scale_x_discrete(limits = c('Metropolitan', 'Dry border', 'Rest of the country')) +
        geom_signif(data = , comparisons = list(c('Gamma','Delta')),
              map_signif_level = map_levels)
  

png('time_panel.png', res = 300, width = 12, heigh = 12, units = 'cm')
p1
dev.off()

#s1 <- all.dep.filtered2 %>%
#        group_by(region) %>%
#        wilcox_test(time.detection ~ VOCs)

dept <- load_geouy(c = 'Departamentos')
p2 <- ggplot(dept) + geom_sf(fill = 'white') + theme_classic()

png('country_panel.png', res = 300, width = 12, heigh = 12, units = 'cm')
p2
dev.off()

p3 <- ggplot(all.dep.filtered2 %>% filter(date.ancestral >= '2021-01-01'),
             aes(x = as.Date(date.ancestral), y = time.detection)) +
  geom_point(aes(color = VOCs)) +
  scale_x_date(date_labels = "%b %y",
               breaks = '1 month') +
  scale_colour_manual(values = c('grey', 'orange', 'firebrick3')) +
  stat_smooth(method = 'loess', color = 'black', alpha = .2) +
  theme_classic() +
  theme(legend.position = 'top',
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) +
  ylab('Time of cryptic circulation (days)') +
  xlab('Date')

png('time2_panel.png', res = 300, width = 12, heigh = 12, units = 'cm')
p3
dev.off()

cluster.size.category <- rep('<10', dim(all.dep.filtered2)[1])
cluster.size.category[which(as.numeric(all.dep.filtered2$cluster.size) >= 10 & as.numeric(all.dep.filtered2$cluster.size) <= 50)] <- '10-50'
cluster.size.category[which(as.numeric(all.dep.filtered2$cluster.size) > 50)] <- '>50'

all.dep.filtered2$cluster.size.category <- cluster.size.category
all.dep.filtered2$cluster.size.category <- factor(all.dep.filtered2$cluster.size.category,
                                                  levels = c('<10', '10-50', '>50'))

p4 <- ggplot(all.dep.filtered2 %>% filter(date.ancestral >= '2021-01-01')) +
        geom_density(aes(x = as.numeric(time.detection),
                         group = cluster.size.category,
                         fill = cluster.size.category),
                         alpha = 0.3) +
        theme_classic() +
        theme(legend.position = c(0.8, 0.8),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) +
        ylab('Density') +
        xlab('Time of cryptic circulation (days)') +
        scale_fill_discrete(name = 'Cluster size') +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

png('cluster_panel.png', res = 300, width = 12, heigh = 12, units = 'cm')
p4
dev.off()





