library(WGCNA)
gene<-read.csv(file.choose(),row.names = 1)
gene <- t(gene)
gene2 <- t(gene2)
gene2<-read.csv(file.choose(),row.names = 1)
occor =corAndPvalue(gene, gene2 use = "pairwise.complete.obs", method="spearman")
occor.r = occor$cor
occor.p = occor$p
occor.r[occor.p>0.05] = 0
write.csv(gene,file="LC50-gene.csv")
