# Mengimpor data
mydata<-read.csv("D:/File Tugas/grocery.csv",header=T, colClasses = "factor")
summary(mydata)

# Melihat aturan asosiasi data secara keseluruhan
library(arules)
rules <- apriori(mydata)


# Mencari aturan asosiasi yang menarik
rules1 <- apriori(mydata,parameter = list(minlen=2, maxlen=5,supp=.1, conf=.5),appearance=list(rhs=c("BREAD=Yes"),lhs=c("MILK=Yes", "BISCUIT=Yes", "BOURNVITA=Yes", "COCK=Yes", "COFFEE=Yes", "CORNFLAKES=Yes", "JAM=Yes", "MAGGI=Yes", "SUGER=Yes","TEA=Yes"),default="none"))
inspect(rules1)
quality(rules1)<-round(quality(rules1),digits=3)
rules.sorted <- sort(rules1, by="lift")
inspect(rules.sorted)

# Menghilangkan redudancy
redundant <- is.redundant(rules1, measure="confidence")
which(redundant)
rules.pruned <- rules1[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)

# Graphs and Charts
library(arulesViz)
plot(rules1)
plot(rules1,method="grouped")
plot(rules1,method="graph")