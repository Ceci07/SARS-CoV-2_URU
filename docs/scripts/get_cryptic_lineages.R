
get_cryptic_lineages <- function(

							tree,     # Phylogenetic tree in nexus format with annotated nodes (locations)
							dates,    # Table with nodes dates
							location, # Destiny location to be evaluated
							min.size  # Minimum cluster size to be reported
) {

library(treeio)
library(phylobase)
library(lubridate)
library(berryFunctions)

out  <- NULL
tre  <- treeio::read.beast(tree)
dat  <- read.table(dates, sep = '\t', header = T) 

phy  <- tre@phylo
phy4 <- phylo4(phy)

locs <- as.data.frame(tre@data)
target.nodes <- locs[which(locs[, 1] == location),]

for (n in 1:dim(target.nodes)[1]) {

	n.node  <- as.numeric(target.nodes[n, 2])
	ancest  <- phylobase::ancestor(phy = phy4, node = n.node)
	anc.lab <- names(ancest)
	ancest2 <- as.numeric(ancest)

	loc <- locs[which(locs[, 2] == ancest2), 1]

	if (loc != location) {

		descend <- phylobase::descendants(phy = phy4, node = n.node)

		if (length(descend) >= min.size) {

			date.in <- dat[which(dat[,1] == anc.lab), 2]

			if (is.error(as.Date(date.in)) == FALSE) {
			
				cl.size <- length(descend)
				cl.data <- do.call(rbind,lapply(names(descend), function(x){strsplit(x, '|', fixed = T)[[1]]}))
				min.dat <- as.character(min(as.Date(cl.data[, 6])))
				dif.date <- as.numeric(difftime(as.Date(min.dat), as.Date(date.in), units = 'days'))
				lin <- as.vector(cl.data[1, 5])

				resu <- c(n.node, anc.lab, date.in, loc, as.numeric(cl.size), min.dat, dif.date, lin)
				out  <- rbind(out, resu)
			}
		}
	}
}

colnames(out) <- c('node.id', 'node.label', 'date.ancestral', 'location.ancestral', 'cluster.size', 'date.detection', 'time.detection', 'lineage')
rownames(out) <- NULL

out <- as.data.frame(out)

return(out)

}
