# library yang digunakan 
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(tokenizers)

# data yang digunakan 
datatext = read.csv("DataLatihText.csv")
dim(datatext)

# membuat korpus / kumpulan dokumen
corpusdc <- iconv(datatext$narasi, to = "UTF-8") # UTF koding karakter
corpusdc <- Corpus(VectorSource(corpusdc))
inspect(corpusdc[1:6]) # memunculkan 6 dokumen teratas

# pembersihan data text / cleaning text
# set kalimat dalam bentuk huruf kecil semua
corpusdc <- tm_map(corpusdc,tolower) 
inspect(corpusdc[1:6])

# punctuation period (full stop) ( . ) comma ( , ) question mark ( ? ) 
# exclamation mark ( ! ) colon ( : ) semicolon ( ; )
# single quotation marks ( ' ' ) double quotation marks ( " " )
corpusdc <- tm_map(corpusdc, removePunctuation)
corpusdc <- tm_map(corpusdc, removeNumbers)
inspect(corpusdc[1:6])

# stopword
library(stopwords)
stopwordID <- read.delim("ID-Stopwords.txt")
ccStopwordID <- readline(stopwordID)
# tokenize_words(corpusdc, stopwords = ccStopwordID)
corpusdc <- tm_map(corpusdc, removeWords, ccStopwordID)
inspect(corpusdc[1:6])

corpusdc <- tm_map(corpusdc, removeWords,
                   c('dan','yg','yth','ini','dari','ada','tidak','itu',
                     'yang','untuk','dengan','dalam','dgn','bahwa',
                     'lagi','sebagai','jangan','bisa','satu',
                     'sudah','akan', 'kita', 'akan','dia','utk',
                     'telah','kepada','saya','kami','saat','jadi','tak','tdk','v',
                     'oleh','karena','mereka','pada','hanya','seperti','masih',
                     'anda','tersebut','adalah','the','orang','mau','atau','hari',
                     'semua','bukan','saja','para','tau','sdh','anak','jika',
                     'tahun','nya','kalau','banyak','masuk','gak','apa','belum',
                     'sampai','...','foto','lebih','seorang','tapi','baru',
                     'terjadi','dapat','info','ternyata','bagi','buat',
                     'juga','atas','tadi','kalo','bila','kali'))
inspect(corpusdc[1:6])

corpusdc <- tm_map(corpusdc, stripWhitespace)
inspect(corpusdc[1:6])

length(corpusdc)

# pembobotan kata tf-idf
dtmbdc = DocumentTermMatrix(corpusdc)
# write.csv((as.matrix(dtmbdc)), "tfbdc.csv")
dtmbdc.matrix = as.matrix(dtmbdc)
inspect(dtmbdc)
dtmtfidff <- weightTfIdf(dtmbdc, normalize = TRUE)
dtmtfidff.matrix = as.matrix(dtmtfidff)
inspect(dtmtfidff)
# write.csv((as.matrix(dtmtfidff)), "tfidfbdc.csv")

barplot <- rowSums(t(dtmbdc.matrix))
barplot
barplot <- subset(barplot, barplot>=10)
barplot
barplot(barplot, las=2, col=rainbow(50), cex.names = 0.8)

# wordcloud
library(wordcloud)
dtmbdcc = TermDocumentMatrix(corpusdc)
dtmbdcc = as.matrix(dtmbdcc)
bar <- sort(rowSums(dtmbdcc),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(bar),freq = bar, max.words = 200,
          min.freq = 5, colors = brewer.pal(8,"Dark2"),
          scale = c(5,0,3), rot.per = 0.25)

library(wordcloud2)
g <- data.frame(names(bar), bar)
colnames(g) <- c('narasi','freq')
head(g)
wordcloud2(g)
wordcloud2(g,size = 0.8,shape = 'star',rotateRatio = 0.5,minSize = 1)

# Sentimen analisis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

bdc <- iconv(corpusdc, to ='UTF-8')
g1 <- get_nrc_sentiment(bdc)
head(g1)
barplot(colSums(g1), las=2, col=rainbow(10),main='Sentiment Analysis Untuk Berita Hoax')
