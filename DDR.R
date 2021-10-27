gene=read.table(file.choose())
library(ggplot2)
gene$similiar<-1-gene$value
gene <- merge(gene, meta, by = 'D')asd
ggplot(gene) +
  aes(x = value.y, y = similiar) +
  geom_point(size = 3L, colour = "#6baed6") +
  geom_smooth(method="lm",span = 0.95) +
  ggthemes::theme_economist_white()+geom_text(x=20, y=26, aes(label=lm_func(data.frame(gene[, c(5,12)]))), parse=T)

gene$"similiar(%)"<-gene$similiar*100
gene$distance<-gene$value.y/100000


ggplot(gene) +
  aes(x = distance, y = similiar...) +
  geom_point(size = 5L, colour = "#0c4c8a",alpha=0.2) +geom_smooth(method = "lm")+xlab(paste0("Geographic distance"))+
  ylab(paste0("Community similiar (%)"))



library(vegan)
library(geosphere)
library(ggplot2)
geo<-read.csv(file.choose(),row.names = 1)
otu<-read.csv(file.choose(),row.names = 1)
d.geo <- distm(geo, fun = distHaversine) #经纬度转换为地理距离
dist.geo <- as.dist(d.geo)
dist_geo <- as.data.frame(as.vector(dist.geo))
dist_geo <- dist_geo/1000 #地理距离单位转换为千米
colnames(dist_geo) <- "dist_geo"
dist.otu <- vegdist(otu, method = "bray")
dist_otu <- as.data.frame(as.vector(dist.otu))
colnames(dist_otu) <- "dist_otu"
data <- data.frame(dist_geo, dist_otu)
data$dist_otu <- 1-data$dist_otu
data$dist_otu <- data$dist_otu * 100 #转换为百分比
summary(lm(data$dist_otu ~ data$dist_geo))
lm(data$dist_otu ~ data$dist_geo)
ggplot(data, aes(x = dist_geo,y = dist_otu)) +
  geom_point(size=3,color=c("#FF8247"),alpha = 0.2) +
  geom_smooth(method = "lm",color="Red",alpha = 0.2) +
  labs(x = "Geographic Distance (Km)", y = "Community Similarity (%) ") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),panel.grid.major = element_line(colour = "gray", size=0.5),panel.background=element_blank()
  )


write.csv(data,"data.csv")
