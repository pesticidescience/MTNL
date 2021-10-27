library(vegan)
phylum <- read.csv(file.choose(),header=T,row.names = 1)
gene <- read.csv(file.choose(),header=T,row.names = 1)
otu_bray<-vegdist(phylum, method='bray')
gene1<-vegdist(gene, method='bray')
m=mantel(otu_bray, gene1, method="spear", permutations=999)
m
