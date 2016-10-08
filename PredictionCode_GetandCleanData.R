

## This is code for the Capstone project, where we will obtain and clean the data, then store the N-grams 
## that are created so that it is efficiently retrieved by the Predition Code.  

setwd("C:\\Users\\youssef\\Documents\\DataScience Capstone\\Coursera-SwiftKey\\final\\en_US")
library(tm)
library(ggplot2)
library(quanteda)
library(grid)
library(gridExtra)
library(stringi)
library(reshape2)

# con<-file("./en_US.twitter.txt")
# full_twitter<-readLines(con,encoding="UTF-8")
# close(con)
# 
# con<-file("./en_US.news.txt")
# full_news<-readLines(con,encoding="UTF-8")
# close(con)
# 
# con<-file("./en_US.blogs.txt")
# full_blogs<-readLines(con,encoding="UTF-8")
# close(con)
# 
# CorpusTwitter<-corpus(full_twitter)
# CorpusNews<-corpus(full_news)
# CorpusBlogs<-corpus(full_blogs)
# 
# CorpusComplete<-CorpusTwitter+CorpusNews+CorpusBlogs
# 
# # Now take a 10% sample of the complete corpus in order to make the processing time reasonable
# 
# samplerate<-0.10
# 
# CorpusSample<-CorpusComplete[sample(nrow(CorpusComplete$documents),nrow(CorpusComplete$documents)*samplerate)]
# 
# # Clear memory to provide more space for the exploratory analysis
# 
# rm(CorpusTwitter,CorpusNews,CorpusBlogs,full_twitter,full_news,full_blogs,CorpusComplete)
# gc()

#Trying it with the Benchmark input documents
con<-file("./tweets.txt")
full_twitter<-readLines(con,encoding="UTF-8")
close(con)


con<-file("./blogs.txt")
full_blogs<-readLines(con,encoding="UTF-8")
close(con)

CorpusTwitter<-corpus(full_twitter)

CorpusBlogs<-corpus(full_blogs)

CorpusComplete<-CorpusTwitter+CorpusBlogs

# Now take a 10% sample of the complete corpus in order to make the processing time reasonable

samplerate<-1.00

CorpusSample<-CorpusComplete[sample(nrow(CorpusComplete$documents),nrow(CorpusComplete$documents)*samplerate)]

# Clear memory to provide more space for the exploratory analysis

rm(CorpusTwitter,CorpusBlogs,full_twitter,full_blogs,CorpusComplete)
gc()

frequencycutoff<-0

# The following code will find the 1-grams, 2-grams, 3-grams, 4-grams, and 5-grams 
# in the sampled corpus.  

Onegrams <- dfm(CorpusSample,
           ngrams=1,
           toLower=T,
           removeNumbers=T,
           concatenator=" ", 
           removePunct=T,
           removeSeparators = T,
           stem=F)

N1 <- data.frame(Content = features(Onegrams), Frequency = colSums(Onegrams),row.names = NULL, stringsAsFactors = FALSE)

NN1<-transform(N1, content = colsplit(N1$Content, pattern = " ", names = c('content.a')))

NN1<-subset(NN1, select=c(content.a,Frequency))

NN1<-subset(NN1, Frequency>=frequencycutoff)


Twograms <- dfm(CorpusSample,
                ngrams=2,
                toLower=T,
                removeNumbers=T,
                concatenator=" ", 
                removePunct=T,
                removeSeparators = T,
                stem=F)


N2 <- data.frame(Content = features(Twograms), Frequency = colSums(Twograms),row.names = NULL, stringsAsFactors = FALSE)

NN2<-transform(N2, content = colsplit(N2$Content, pattern = " ", names = c('a', 'b')))

NN2<-subset(NN2, select=c(content.a,content.b,Frequency))

NN2<-subset(NN2, Frequency>=frequencycutoff)

save(NN1, file = "OneGrams.Rdata")
save(NN2, file = "TwoGrams.Rdata")

rm(N1,NN1,N2,NN2,Onegrams,Twograms)
gc()


Threegrams <- dfm(CorpusSample,
                  ngrams=3,
                  toLower=T,
                  removeNumbers=T,
                  concatenator=" ", 
                  removePunct=T,
                  removeSeparators = T,
                  stem=F)

N3 <- data.frame(Content = features(Threegrams), Frequency = colSums(Threegrams),row.names = NULL, stringsAsFactors = FALSE)

NN3<-transform(N3, content = colsplit(N3$Content, pattern = " ", names = c('a', 'b','c')))

NN3<-subset(NN3, select=c(content.a,content.b,content.c,Frequency))

NN3<-subset(NN3, Frequency>=frequencycutoff)
save(NN3, file = "ThreeGrams.Rdata")
rm(N3,NN3,Threegrams)
gc()

Fourgrams <- dfm(CorpusSample,
                 ngrams=4,
                 toLower=T,
                 removeNumbers=T,
                 concatenator=" ", 
                 removePunct=T,
                 removeSeparators = T,
                 stem=F)

N4 <- data.frame(Content = features(Fourgrams), Frequency = colSums(Fourgrams),row.names = NULL, stringsAsFactors = FALSE)

NN4<-transform(N4, content = colsplit(N4$Content, pattern = " ", names = c('a', 'b','c','d')))

rm(N4,Fourgrams)
gc()

# save(NN4, file = "FourGrams initial step.Rdata")
# 
# load("FourGrams initial step.Rdata")
NN4<-subset(NN4, select=c(content.a,content.b,content.c,content.d,Frequency))
NN4<-subset(NN4, Frequency>=frequencycutoff)

save(NN4, file = "FourGrams.Rdata")

rm(NN4)
gc()

Fivegrams <- dfm(CorpusSample,
                 ngrams=5,
                 toLower=T,
                 removeNumbers=T,
                 concatenator=" ", 
                 removePunct=T,
                 removeSeparators = T,
                 stem=F)

N5 <- data.frame(Content = features(Fivegrams), Frequency = colSums(Fivegrams),row.names = NULL, stringsAsFactors = FALSE)

# save(N5, file = "FiveGrams initial step.Rdata")
# 
# load("FiveGrams initial step.Rdata")

NN5<-transform(N5, content = colsplit(N5$Content, pattern = " ", names = c('a', 'b','c','d','e')))

# save(NN5, file = "FiveGrams initial step.Rdata")
# 
# load("FiveGrams initial step.Rdata")

NN5<-subset(NN5, select=c(content.a,content.b,content.c,content.d,content.e,Frequency))
NN5<-subset(NN5, Frequency>=frequencycutoff)

save(NN5, file = "FiveGrams.Rdata")
rm(N5,NN5, Fivegrams)
gc()
