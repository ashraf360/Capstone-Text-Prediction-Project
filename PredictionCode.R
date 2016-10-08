# Predict the next word based on previously determined N-Grams for n=1,..,5
#


library(reshape2)


load("data/FiveGrams.Rdata")
load("data/FourGrams.Rdata")
load("data/ThreeGrams.Rdata")
load("data/TwoGrams.Rdata")
load("data/OneGrams.Rdata")

NN1<- NN1[order(-NN1$Frequency),]
NN2<- NN2[order(-NN2$Frequency),]
NN3<- NN3[order(-NN3$Frequency),]
NN4<- NN4[order(-NN4$Frequency),]
NN5<- NN5[order(-NN5$Frequency),]

## This Function cleans the input phrase and makes it standard and lower case
PrepInputPhrase <- function(x1) {
  # convert to lowercase
  x <- tolower(x1)
  # remove numbers
  x <- gsub("\\S*[0-9]+\\S*", " ", x1)
  # change common hyphenated words to non
  x <- gsub("e-mail","email", x1)
  # remove any brackets at the ends
  x <- gsub("^[(]|[)]$", " ", x1)
  # remove any bracketed parts in the middle
  x <- gsub("[(].*?[)]", " ", x1)
  # remove punctuation, except intra-word apostrophe and dash
  x <- gsub("[^[:alnum:][:space:]'-]", " ", x1)
  x <- gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x1)
  # compress and trim whitespace
  x <- gsub("\\s+"," ",x1)
  x <- gsub("^\\s+|\\s+$", "", x1)
  return(x1)
}


## This Function returns the last N words of prepared phrase in a vector of type character
LastNWords <- function(x2, n) {
  x2 <- PrepInputPhrase(x2)
 
  words <- unlist(strsplit(x2, " "))
  
  len <- length(words)
  
  if (n < 1) {
    stop("Error in LastNWords: zero words found")
  }
  if (n > len) {
    n <- len
  }
  if (n==1) {
    return(words[len])
  } else {
    r1 <- words[len]
    
    for (i in 1:(n-1)) {
      r1 <- c(words[len-i], r1)
    }
    r1
  }
}

#


## Function that computes stupid backoff score
StupidBackoffScore <- function(alpha=0.4, s5, s4, s3, s2) {
  ss <- 0
  if (s5 > 0) {
    ss <- s5
  } else if (s4 >= 1) {
    ss <- s4 * alpha
  } else if (s3 > 0) {
    ss <- s3 * alpha * alpha
  } else if (s2 > 0) {
    ss <- s2 * alpha * alpha * alpha
  }
  return(round(ss,2))
}



# This function obtains the next word matches from the previously loaded N-Gram dataframes

CalcScore <- function(input) {
  
  getm5rows<-5
  getm4rows<-5
  getm3rows<-5
  getm2rows<-5
  
  # input<-"is going to be"
  
  wds<-LastNWords(input,4)
  m5<-subset(NN5,content.a == wds[1] & content.b == wds[2] & content.c == wds[3] & content.d == wds[4])
  
  m5<- m5[order(-m5$Frequency),]
  summ5freq<-sum(m5$Frequency)
  m5$frequencycalc<-round(m5$Frequency/summ5freq * 100)
  if (nrow(m5) < getm5rows) {
    getm5rows <-nrow(m5)
  }
  NN5match <- m5[1:getm5rows,]
  NN5match$response<-NN5match$content.e
  colnames(NN5match)<-c("NN5.content.a", "NN5.content.b","NN5.content.c","NN5.content.d","NN5.content.e","NN5.frequency","NN5.frequencycalc","response")
  #
  
  wds<-LastNWords(input,3)
  m4<-subset(NN4,content.a == wds[1] & content.b == wds[2] & content.c == wds[3])
  
  m4<- m4[order(-m4$Frequency),]
  summ4freq<-sum(m4$Frequency)
  m4$frequencycalc<-round(m4$Frequency/summ4freq * 100)
  if (nrow(m4) < getm4rows) {
    getm4rows <-nrow(m4)
  }
  NN4match <- m4[1:getm4rows,]
  NN4match$response<-NN4match$content.d
  colnames(NN4match)<-c("NN4.content.a", "NN4.content.b","NN4.content.c","NN4.content.d","NN4.frequency","NN4.frequencycalc","response")
  #
  
  wds<-LastNWords(input,2)
  m3<-subset(NN3,content.a == wds[1] & content.b == wds[2])
  m3<- m3[order(-m3$Frequency),]
  summ3freq<-sum(m3$Frequency)
  m3$frequencycalc<-round(m3$Frequency/summ3freq * 100)
  if (nrow(m3) < getm3rows) {
    getm3rows <-nrow(m3)
  }
  NN3match <- m3[1:getm3rows,]
  NN3match$response<-NN3match$content.c
  colnames(NN3match)<-c("NN3.content.a", "NN3.content.b","NN3.content.c","NN3.frequency","NN3.frequencycalc","response")
  #
  
  wds<-LastNWords(input,1)
  m2<-subset(NN2,content.a == wds[1])
  
  m2<- m2[order(-m2$Frequency),]
  summ2freq<-sum(m2$Frequency)
  m2$frequencycalc<-round(m2$Frequency/summ2freq * 100)
  if (nrow(m2) < getm2rows) {
    getm2rows <-nrow(m2)
  }
  NN2match <- m2[1:getm2rows,]
  NN2match$response<-NN2match$content.b
  colnames(NN2match)<-c("NN2.content.a", "NN2.content.b","NN2.frequency","NN2.frequencycalc","response")
  
  # Now combine all of these results
  
  NN54match <- merge(NN5match, NN4match, by="response", all=TRUE)
  NN543match <- merge(NN54match, NN3match, by= "response", all=TRUE)
  NN5432match <- merge(NN543match, NN2match, by="response", all=TRUE)
  
  Nmatch<-subset(NN5432match, !is.na(response))
  
  if (nrow(Nmatch)>0 ){
    Nmatch<-Nmatch[order(-Nmatch$NN5.frequencycalc,-Nmatch$NN4.frequencycalc,-Nmatch$NN3.frequencycalc,-Nmatch$NN2.frequencycalc), ]
    Nmatch[is.na(Nmatch)]<-0
    
  # Now calculate the score and create a new column
    
    Nmatch$score <- mapply(StupidBackoffScore, alpha=0.4, Nmatch$NN5.frequencycalc, Nmatch$NN4.frequencycalc, Nmatch$NN3.frequencycalc, Nmatch$NN2.frequencycalc)
  #
    Nmatch <- Nmatch[order(-Nmatch$score), ]
  }
  
  return(Nmatch)
#
}



# Prediction function using the Stupid Back off algorithm

PredictResponse <- function(xx) {
  
  if(is.null(xx))
  {
    return()
  }
  
  predictedresponse <- ""
  if (xx == "") {
    return("The entry was Null")
  }
  
  df <- CalcScore(xx)
  # If the returned dataframe has zero rows then return with a common word
  if (nrow(df) == 0) {
    return("the")
  }
  
  topwords <- df[df$score == max(df$score), ]$response
  
# if multiple candidates, randomly select one
  
  predictedresponse <- sample(topwords, 1)
  
  return(predictedresponse)
}


