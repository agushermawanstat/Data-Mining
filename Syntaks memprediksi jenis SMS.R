library(tm)
library(e1071)
library(gmodels)

data <- read.csv('dataset_sms_spam _v1.csv',header=TRUE,sep=",", stringsAsFactors = FALSE)

data$label<-ifelse(data$label=="0", "SMS Biasa", data$label)
data$label<-ifelse(data$label=="1", "SMS Penipuan", data$label)
data$label<-ifelse(data$label=="2", "SMS Operator", data$label)
View(data)
str(data)

data$Teks <- factor(data$Teks)
sms_corpus <- VCorpus(VectorSource(data$Teks))
print(sms_corpus)

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) 
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) 
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

sms_dtm <- DocumentTermMatrix(sms_corpus_clean) 
print(sms_dtm)

smp_size <- floor(0.80 * nrow(sms_dtm))
train_ind <- sample(seq_len(nrow(sms_dtm)), size = smp_size)

sms_dtm_train <- sms_dtm[train_ind, ]
sms_dtm_test <- sms_dtm[-train_ind, ]

sms_train_labels <- data[train_ind, ]$label
sms_test_labels  <- data[-train_ind, ]$label

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
sms_dtm_freq_train

#menghitung jumlah kata
sms_freq_words <- findFreqTerms(sms_dtm_train, 1)

sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words] 

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)

sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred

CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

################
#Coba data baru
################

new_sms = c("Hai bro, apa kabar ?",
           "Dapatkan kuota harian hanya 1000 rupiah per gb, kunjungi aplikasi mygsm",
           "pesugihan halal, lipatgandakan uang anda sekarang bersama ki ....",
           "besok futsal ya")

new_sms=as.data.frame(new_sms)

new_sms$new_sms <- factor(new_sms$new_sms)
sms_corpus <- VCorpus(VectorSource(new_sms$new_sms))

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) 
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) 
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

sms_dtm <- DocumentTermMatrix(sms_corpus_clean) 
sms_dtm_freq_new <- removeSparseTerms(sms_dtm, 0.999)
sms_freq_words <- findFreqTerms(sms_dtm, 1)
sms_dtm_freq_new <- sms_dtm[ , sms_freq_words]
sms_new <- apply(sms_dtm_freq_new, MARGIN = 2, convert_counts)

sms_test_new <- predict(sms_classifier, sms_new)
sms_test_new

new=as.data.frame(sms_test_new)

hasil_final=as.data.frame(cbind(new_sms,new))
