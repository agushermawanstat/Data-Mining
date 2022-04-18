data=data.frame(x1=c(2,4,3,8,9,10,12,8,6,6.5,6),x2=c(12,11,10,10,8.5,10.5,8,5,6,5.3,4))
plot(data,pch=19,cex=2)
kmeans3=kmeans(data,centers=3)
kmeans3$cluster
table(kmeans3$cluster)
plot(data,pch=kmeans3$cluster+14,cex=2)
kmeans3$centers=
plot(data,pch=kmeans3$cluster+14,cex=2)
points(kmeans3$cluster,cex=2,pch=13)
set.seed(15)

library(cluster)
jarak=dist(data)
siluet=NULL
for(k in 2:8){
  label_gerombol=kmeans(data,centers=k,iter.max=100)$cluster
  sil=silhouette(label_gerombol,jarak)
  siluet[k]=mean(sil[,3])
}
plot(siluet,type="b",xlab="banyaknya gerombol",cex=1.5, pch=19)
