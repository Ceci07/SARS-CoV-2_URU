library(ape)
library(phangorn)
library(ggtree)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggnewscale)
library(readxl)
library(maps)
library(geosphere)
library(rworldmap)
library(plyr)
library(data.table)
library(ggthemes)
library(ggsankey)

# Set working directory
setwd('/Users/giraola/Documents/05_covid/paper/figure2_transmission/')

##########################################
# Panel A: Phylogeny highlighting clades #
##########################################

tre <- read.nexus('./timetree.nexus')

meta  <- read.csv('./datsaet_phylodynamics.tsv', sep = '\t', header = T)
meta2 <- meta
rownames(meta2) <- meta[,1]
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
    
    s <- meta2[w,1]
    grp[[r]] <- s
    nam <- c(nam, uph[u])
    r <- r + 1
  }
}

names(grp) <- uph

g1 <- ggtree(tre, layout = 'fan', size = .1) %<+% meta2

delta.clade  <- MRCA(tre, grp$Delta)
gamma.clade  <- MRCA(tre, grp$Gamma)
beta.clade   <- MRCA(tre, grp$Beta)
alpha.clade  <- MRCA(tre, grp$Alpha)
lambda.clade <- MRCA(tre, grp$Lambda)
mu.clade     <- MRCA(tre, grp$Mu)

dat <- data.frame(id = c(delta.clade, gamma.clade, beta.clade,
                         alpha.clade, lambda.clade, mu.clade),
                  hclade = c('Delta', 'Gamma', 'Beta', 'Alpha', 'Lambda', 'Mu'))

gheat2 <- meta2 %>%
            mutate(country = replace(country, country != 'Uruguay', 2)) %>%
            mutate(country = replace(country, country == 'Uruguay', 1)) %>%
            mutate(country = replace(country, accession == 'EPI_ISL_NA', 3)) %>%
            mutate_at(2, as.numeric)

gheat2 <- as.data.frame(gheat2[,'country'])
rownames(gheat2) <- rownames(meta2)
colnames(gheat2) <- 'uruguay'

cls <- c('green', 'blue', 'gold2', 'firebrick4', 'darkgreen', 'violet')

g2 <- gheatmap(g1, gheat2, offset = .05, width = .075, colnames = F, color = NA) +
        scale_fill_manual(labels = c('Uruguay (public)', 'Rest of the world', 'Uruguay (this study)'), values = c('black', 'grey90', 'brown1'), name = 'Origin')

g3 <- g2 + new_scale_fill() + 
           geom_hilight(data = dat, mapping = aes(node = id, fill = hclade), alpha = .4, extendto = 2.15) +
           scale_fill_manual(values = cls, name = 'Variant') +
           theme(legend.text = element_text(size = 12),
                 legend.key.size = unit(.5, 'cm'),
                 legend.position = 'left')

png('sarscov2_tree.png', res = 600, height = 22, width = 25, units = 'cm')
g3
dev.off()

################################
# Panel B: Global transmission #
################################

regions    <- read_xlsx('region_joint_tree_transitions.xlsx')
to_uruguay <- NULL

for (d in 2:(dim(regions)[2]-1)) {
  
  frm <- colnames(regions)[d]
  rat <- regions[9,d]
  res <- c(frm, 'Uruguay', rat)
  to_uruguay <- rbind(to_uruguay, res)
}

colnames(to_uruguay) <- c('From', 'To', 'Rate')
rownames(to_uruguay) <- NULL
to_uruguay <- as.data.frame(to_uruguay)

from_lats <- c(4.549046, -37.731625, 42.060509, -11.743681, 47.953145, 37.561997, -28.671311, -2.332577)
from_lons <- c(24.69005, -65.368790, 96.377361, -49.690669, 10.523538, -99.30713, 136.830733, -75.88683)
uy_lats <- rep(-33.018884, 8)
uy_lons <- rep(-55.837402, 8)

to_uruguay$from_latitude <- from_lats
to_uruguay$from_longitude <- from_lons
to_uruguay$uy_latitude <- uy_lats
to_uruguay$uy_longitude <- uy_lons

to_uruguay$edgecols <- c('darkgreen', 'orange', 'purple', 'red', 'deepskyblue2',
                         'gold2', 'lightseagreen', 'orange4')

worldMap <- getMap()
new_world <- subset(worldMap, continent != "Antarctica")
mp <- fortify(new_world)

mapids <- mp$id
mapcols <- rep('No data', length(mapids))

samerica <- unique(meta2[which(meta2$location == 'SouthAmerica'), 'country'])
namerica <- unique(meta2[which(meta2$location == 'NorthAmerica'), 'country'])
asia     <- unique(meta2[which(meta2$location == 'Asia'), 'country'])
africa   <- unique(meta2[which(meta2$location == 'Africa'), 'country'])
oceania  <- unique(meta2[which(meta2$location == 'Oceania'), 'country'])
europe   <- unique(meta2[which(meta2$location == 'Europe'), 'country'])
argentin <- 'Argentina'
brazil   <- 'Brazil'
uruguay  <- 'Uruguay'

namerica[24] <- 'United States of America'

mapcols[which(mapids%in%africa)] <- 'Africa'
mapcols[which(mapids%in%asia)] <- 'Asia'
mapcols[which(mapids%in%samerica)] <- 'South America'
mapcols[which(mapids%in%namerica)] <- 'North America'
mapcols[which(mapids%in%oceania)] <- 'Oceania'
mapcols[which(mapids%in%europe)] <- 'Europe'
mapcols[which(mapids%in%argentin)] <- 'Argentina'
mapcols[which(mapids%in%brazil)] <- 'Brazil'
mapcols[which(mapids%in%uruguay)] <- 'Uruguay'

mp$colors <- mapcols
mp$calpha <- rep(0.2, dim(mp)[1])
mp[which(mp$colors == 'Uruguay'), 'calpha'] <- 1

g4 <- ggplot() + 
        geom_polygon(data= mp, aes(long, lat, group = group, fill = colors), alpha = mp$calpha) +
        scale_fill_manual(values = c(unlist(to_uruguay$edgecols), 'black', 'grey'),
                          limits = c('Africa', 'Argentina', 'Asia', 'Brazil', 'Europe', 'North America', 'Oceania', 'South America', 'Uruguay', 'No data'),
                          name = 'Region') +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[1]][1],
                       xend = to_uruguay$from_longitude[[1]][1],
                       x = to_uruguay$uy_longitude[[1]][1],
                       y = to_uruguay$uy_latitude[[1]][1]),
                   color = to_uruguay$edgecols[[1]][1],
                   size = to_uruguay$Rate[[1]][1]/2,
                   curvature = 0.3,
                   lineend = 'round',
                   arrow = NULL) +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[2]][1],
                       xend = to_uruguay$from_longitude[[2]][1],
                       x = to_uruguay$uy_longitude[[2]][1],
                       y = to_uruguay$uy_latitude[[2]][1]),
                   color = to_uruguay$edgecols[[2]][1],
                   size = to_uruguay$Rate[[2]][1]/2,
                   curvature = 0.3,
                   lineend = 'round',
                   arrow = NULL) +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[3]][1],
                       xend = to_uruguay$from_longitude[[3]][1],
                       x = to_uruguay$uy_longitude[[3]][1],
                       y = to_uruguay$uy_latitude[[3]][1]),
                   color = to_uruguay$edgecols[[3]][1],
                   size = to_uruguay$Rate[[3]][1]/2,
                   curvature = 0.4,
                   lineend = 'round',
                   arrow = NULL) +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[4]][1],
                       xend = to_uruguay$from_longitude[[4]][1],
                       x = to_uruguay$uy_longitude[[4]][1],
                       y = to_uruguay$uy_latitude[[4]][1]),
                   color = to_uruguay$edgecols[[4]][1],
                   size = to_uruguay$Rate[[4]][1]/2,
                   curvature = 0.2,
                   lineend = 'round',
                   arrow = NULL) +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[5]][1],
                       xend = to_uruguay$from_longitude[[5]][1],
                       x = to_uruguay$uy_longitude[[5]][1],
                       y = to_uruguay$uy_latitude[[5]][1]),
                   color = to_uruguay$edgecols[[5]][1],
                   size = to_uruguay$Rate[[5]][1]/2,
                   curvature = 0.2,
                   lineend = 'round',
                   arrow = NULL) +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[6]][1],
                       xend = to_uruguay$from_longitude[[6]][1],
                       x = to_uruguay$uy_longitude[[6]][1],
                       y = to_uruguay$uy_latitude[[6]][1]),
                   color = to_uruguay$edgecols[[6]][1],
                   size = to_uruguay$Rate[[6]][1]/2,
                   curvature = -0.3,
                   lineend = 'round',
                   arrow = NULL) +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[7]][1],
                       xend = to_uruguay$from_longitude[[7]][1],
                       x = to_uruguay$uy_longitude[[7]][1],
                       y = to_uruguay$uy_latitude[[7]][1]),
                   color = to_uruguay$edgecols[[7]][1],
                   size = to_uruguay$Rate[[7]][1]/2,
                   curvature = 0.3,
                   lineend = 'round',
                   arrow = NULL) +
        geom_curve(stat = 'identity', 
                   aes(yend = to_uruguay$from_latitude[[8]][1],
                       xend = to_uruguay$from_longitude[[8]][1],
                       x = to_uruguay$uy_longitude[[8]][1],
                       y = to_uruguay$uy_latitude[[8]][1]),
                   color = to_uruguay$edgecols[[8]][1],
                   size = to_uruguay$Rate[[8]][1]/2,
                   curvature = -0.2,
                   lineend = 'round',
                   arrow = NULL) +
        geom_point(aes(y = to_uruguay$from_latitude[[1]][1],
                       x = to_uruguay$from_longitude[[1]][1],
                       size = to_uruguay$Rate[[1]][1]*100),
                       color = to_uruguay$edgecols[[1]][1]) +
        geom_point(aes(y = to_uruguay$from_latitude[[2]][1],
                       x = to_uruguay$from_longitude[[2]][1],
                       size = to_uruguay$Rate[[2]][1]*100),
                       color = to_uruguay$edgecols[[2]][1]) +
        geom_point(aes(y = to_uruguay$from_latitude[[3]][1],
                       x = to_uruguay$from_longitude[[3]][1],
                       size = to_uruguay$Rate[[3]][1]*100),
                       color = to_uruguay$edgecols[[3]][1]) +
        geom_point(aes(y = to_uruguay$from_latitude[[4]][1],
                       x = to_uruguay$from_longitude[[4]][1],
                       size = to_uruguay$Rate[[4]][1]*100),
                       color = to_uruguay$edgecols[[4]][1]) +
        geom_point(aes(y = to_uruguay$from_latitude[[5]][1],
                       x = to_uruguay$from_longitude[[5]][1],
                       size = to_uruguay$Rate[[5]][1]*100),
                       color = to_uruguay$edgecols[[5]][1]) +
        geom_point(aes(y = to_uruguay$from_latitude[[6]][1],
                       x = to_uruguay$from_longitude[[6]][1],
                       size = to_uruguay$Rate[[6]][1]*100),
                       color = to_uruguay$edgecols[[6]][1]) +
        geom_point(aes(y = to_uruguay$from_latitude[[7]][1],
                       x = to_uruguay$from_longitude[[7]][1],
                       size = to_uruguay$Rate[[7]][1]*100),
                       color = to_uruguay$edgecols[[7]][1]) +
        geom_point(aes(y = to_uruguay$from_latitude[[8]][1],
                       x = to_uruguay$from_longitude[[8]][1],
                       size = to_uruguay$Rate[[8]][1]*100),
                       color = to_uruguay$edgecols[[8]][1]) +
        guides(size = guide_legend('Introduction rate'),
               color = guide_legend('black')) +                    
        theme_void()

png('sarscov2_map_uy_intros.png', res = 600, height = 10, width = 25, units = 'cm')
g4
dev.off()

######################################
# Panel C: Gamma & Delta intro rates #
######################################

gamma <- read_xlsx('./subtree/region_gamma_subtree_transitions.xlsx')
delta <- read_xlsx('./subtree/region_delta_subtree_transitions.xlsx')

delr <- as.data.frame(as.vector(delta[9, -c(1,10)]))*-1
delv <- rep('Delta', length(delr))
gamr <- as.data.frame(as.vector(gamma[9, -c(1,10)]))
gamv <- rep('Gamma', length(gamr))

df <- as.data.frame(t(rbind(cbind(delr, gamr), c(delv, gamv))))
df2 <- cbind(gsub('.1', '', rownames(df)), df)

rownames(df2) <- NULL
colnames(df2) <- c('Location', 'Rate', 'Variant')

df.delta <- df2 %>% filter(Variant == 'Delta')
df.gamma <- df2 %>% filter(Variant == 'Gamma')
location <- c('North America', 'Africa', 'Asia', 'Europe', 'Oceania', 'Argentina',
              'South America', 'Brazil')
location2 <- rev(c('Europe', 'South America', 'Africa', 'North America', 'Asia',
               'Argentina', 'Brazil', 'Oceania'))


g5 <- ggplot(df.gamma, aes(x = reorder(Location, as.numeric(Rate)), y = as.numeric(Rate), fill = Variant)) + 
      geom_bar(stat="identity") +
      ggtitle('Gamma') +
      coord_flip() +
      theme_bw() +
      ylab('Introduction rate') +
      xlab('Location') +
      ylim(0,15) +
      scale_fill_manual(values = 'firebrick4') +
      scale_x_discrete(labels = location) +
      theme(legend.position = 'none',
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 12))

png('gamma_intro_rates.png', res = 600, height = 10, width = 10, units = 'cm')
g5
dev.off()

g6 <- ggplot(df.delta, aes(x = reorder(Location, as.numeric(Rate)*-1), y = as.numeric(Rate)*-1, fill = Variant)) + 
  geom_bar(stat="identity") +
  ggtitle('Delta') +
  coord_flip() +
  theme_bw() +
  ylab('Introduction rate') +
  xlab('Location') +
  ylim(0,3) +
  scale_fill_manual(values = 'gold2') +
  scale_x_discrete(labels = location2) +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))

png('delta_intro_rates.png', res = 600, height = 10, width = 10, units = 'cm')
g6
dev.off()
