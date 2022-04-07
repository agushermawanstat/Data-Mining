#Import data
library(readxl)
house_price <- read_excel("D:/AAA STAT SEMESTER 6/DATA MINING/praktikum/house_price.xlsx")
View(house_price)
str(house_price)
set.seed(1234)
ind=sample(2, nrow(house_price), replace=TRUE, prob=c(0.7, 0.3))
trainData <- house_price[ind==1,]
testData <- house_price[ind==2,]
View(testData <- house_price[ind==2,])

#Selanjutnya memuat package party, membangun pohon keputusan, dan melakukan pengecekkan pada hasil prediksi.
#Pada sintaks dibawah, myFormula menjelaskan bahwa price adalah variabel target dan semua variabel lain adalah variabel bebas.

#Bagian 1. Pohon Keputusan dengan Package party
library(party)
myFormula <- price ~ area + rooms
houseprice_ctree <- ctree(myFormula, data=trainData)
table(predict(houseprice_ctree), trainData$price)

#akan ditunjukkan hasil dari pohon keputusan dengan mencetak aturan dan melakukan plot pada pohon.
print(houseprice_ctree)
plot(houseprice_ctree)
plot(houseprice_ctree,type="simple")

#Bagian 2. Pohon Keputusan dengan Package rpart
data(house_price, package = "TH.data")
dim(house_price)
attributes(house_price)
house_price[1:5,]

#data dibagi menjadi data latih (training) dan data uji (testing), dan pohon keputusan dibangun pada data training
set.seed(1234)
ind <- sample(2, nrow(house_price), replace=TRUE, prob=c(0.7, 0.3))
house_price.train <- house_price[ind==1,]
house_price.test <- house_price[ind==2,]

# melatih pohon keputusan
library(rpart)
myFormula <- price ~ area + rooms
house_price_rpart=rpart(myFormula,data = house_price.train,control = rpart.control(minsplit = 10))
attributes(house_price_rpart)

#pohon keputusan yang telah dibangun akan diplot
plot(house_price_rpart)
text(house_price_rpart, use.n=T)

#Kemudian memilih pohon dengan error prediksi terendah
opt = which.min(house_price_rpart$cptable[,"xerror"])
cp <- house_price_rpart$cptable[opt, "CP"]
house_price_prune <- prune(house_price_rpart, cp = cp)
print(house_price_prune)
plot(house_price_prune)
text(house_price_prune, use.n=T)

#pohon yang terpilih digunakan untuk membuat prediksi dan hasil prediksi tersebut dibandingkan dengan label aktualnya.
price_pred <- predict(house_price_prune, newdata=house_price.test)
xlim <- range(house_price$price)
plot(price_pred ~ price, data=house_price.test, xlab="Observed",ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)
     