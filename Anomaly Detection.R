#library
library(lattice)

# Mengimport Data
data <- read.csv("Maternal Health Risk Data Set.csv")
View(data)

# Membuat Data frame baru untuk keperluan menghilangkan outlier
data<-(data[-c(1,2,3)])
data1<-(data[-4])
names(data1)
View(data1)

# Mencari Outlier Global
# Menggunakan Box Plot 
boxplot(data1, horizontal = TRUE)
plot(data1)
# melihat lebih rinci lagi pada kolom dengan outlier
boxplot(data1$BS, horizontal =  TRUE)
boxplot(data1$BodyTemp,  horizontal=  TRUE)
boxplot(data1$HeartRate,  horizontal=  TRUE)

d <- density(data1$BS) # returns the density data
plot(d) # plots the results

d <- density(data1$BodyTemp) # returns the density data
plot(d) # plots the results

d <- density(data1$HeartRate) # returns the density data
plot(d) # plots the results


data2 <- subset(data1, HeartRate > 30)


# boxplot
boxplot(data2, horizontal = TRUE)

# Mencari Outlier Kolektif 
plot(data2)

names(data2)
#berdasarkan BS Vs BodyTemp
df <- data2[,c(1,2)]
plot(df, pch = 19)

kmeans3 <- kmeans(df, center=3)
kmeans3$cluster

table(kmeans3$cluster)
plot(df, pch=kmeans3$cluster+14, cex=1)
points(kmeans3$centers, cex=1, pch=13)
df$class <-  kmeans3$cluster
m <- kmeans3$center
m
df$dist <- 0
for (i in 1:nrow(df)) {
  kategori <- df[i,3]
  df[i,4] <- sqrt((m[kategori,1]-df[i,1])^2+(m[kategori,2]-df[i,2])^2)
}

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('grey','black'))
View(df)
#This adds a column of color values
# based on the y values
df$Col <- rbPal(10)[as.numeric(cut(df$dist,breaks = 10))]

plot(df[,1], df[,2], pch=kmeans3$cluster+15, cex=1.5, col = df$Col)
points(kmeans3$centers, cex=1.5, pch=13)



#berdasarkan BS Vs Heart Rate
df <- data2[,c(1,3)]
plot(df, pch = 19)

kmeans3 <- kmeans(df, center=3)
kmeans3$cluster

table(kmeans3$cluster)
plot(df, pch=kmeans3$cluster+14, cex=1)
points(kmeans3$centers, cex=1, pch=13)
df$class <-  kmeans3$cluster
m <- kmeans3$center
m
df$dist <- 0
for (i in 1:nrow(df)) {
  kategori <- df[i,3]
  df[i,4] <- sqrt((m[kategori,1]-df[i,1])^2+(m[kategori,2]-df[i,2])^2)
}

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('grey','black'))

#This adds a column of color values
# based on the y values
df$Col <- rbPal(10)[as.numeric(cut(df$dist,breaks = 10))]

plot(df[,1], df[,2], pch=kmeans3$cluster+15, cex=1.5, col = df$Col)
points(kmeans3$centers, cex=1.5, pch=13)



#berdasarkan Bodytemp Vs Heart Rate
df <- data2[,c(2,3)]
plot(df, pch = 19)

kmeans3 <- kmeans(df, center=3)
kmeans3$cluster

table(kmeans3$cluster)
plot(df, pch=kmeans3$cluster+14, cex=1)
points(kmeans3$centers, cex=1, pch=13)
df$class <-  kmeans3$cluster
m <- kmeans3$center
m
df$dist <- 0
for (i in 1:nrow(df)) {
  kategori <- df[i,3]
  df[i,4] <- sqrt((m[kategori,1]-df[i,1])^2+(m[kategori,2]-df[i,2])^2)
}

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('grey','black'))

#This adds a column of color values
# based on the y values
df$Col <- rbPal(10)[as.numeric(cut(df$dist,breaks = 10))]

plot(df[,1], df[,2], pch=kmeans3$cluster+15, cex=1.5, col = df$Col)
points(kmeans3$centers, cex=1.5, pch=13)


