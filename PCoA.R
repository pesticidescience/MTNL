library(vegan)
library(ggplot2)
library(RColorBrewer)
gene <- read.csv(file.choose(),header=T,row.names = 1)
meta<-read.csv(file.choose())
gene<-t(gene)
dis_bray <- vegdist(gene, method = 'bray')
pcoa <- cmdscale(dis_bray, k = 2, eig = TRUE, add = TRUE)
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig) 
sample_site <- data.frame({pcoa$point})[1:2] 
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')
sample_site$name <- rownames(gene)
sample_site <- merge(sample_site, meta, by = 'name', all.x = TRUE)
sample_site$group<-as.factor(sample_site$infection)
#plot_color <- c(brewer.pal(9,"Paired"))
fig3<-ggplot(sample_site) +
  aes(x = PCoA1, y = PCoA2, colour = infection) +scale_color_manual(values=c(plot_color))+
  geom_point(size = 4) +theme_classic() + theme(panel.background = element_blank(),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                legend.text = element_text(size=12),
                                                legend.position = 'right',
                                                axis.text = element_text(size=12),
                                                axis.title = element_text(size=12),
                                                aspect.ratio = 1)+guides(color = guide_legend(title="",ncol = 1))+
  labs(x = paste('PCoA axis1: ', round(100 * pcoa_eig[1], 2),'%'), y = paste('PCoA axis2: ', round(100 * pcoa_eig[2], 2),'%'))+stat_ellipse(aes(x = PCoA1, y = PCoA2,color=group),level = 0.95, show.legend = F)
