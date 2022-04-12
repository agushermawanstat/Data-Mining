Cluster <- read_excel("D:/AAA STAT SEMESTER 6/DATA MINING/Cluster.xlsx")
View(Cluster)
data1=(Cluster[-1])
data1
data2=as.matrix(data1)
data2
d=dist(data2,method="euclide")

#metode hirarki clustering
fit=hclust(d,method="single")
fit1=hclust(d,method="complete")
fit3=hclust(d,method="centroid")
fit4=hclust(d,method="median")

plot(fit, labels = Cluster$Provinsi)
group=cutree(fit,k=5)

#menggambar dendogram dengan warna border merah sekitar 5 kluster
rect.hclust(fit,k=5,border="red")
