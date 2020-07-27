
install.packages("xml2")
install.packages("XML")
install.packages("rapportools")
library(rvest)
library(XML)
library(magrittr)
library(rapportools)

######## EXTRACT CUSTOMER REVIEWS OF "MOTO E5 Plus" ON AMAZON #####################

aurl <- "https://www.amazon.in/product-reviews/B077PWK5BT/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"MOTO_E_5_Plus.txt",row.names = F)
getwd()

####################### SENTIMENTAL ANALYSIS_AMAZON REVIEWS #####################

install.packages("syuzhet")
library("syuzhet")

moto <- readLines(file.choose()) # Read Moto E5 Plus review txt file
View(moto)
str(moto) # found class as charactor
moto[1:20] # See first 20 reviews

# Emotion Mining by nrc method->

nrc_moto <- get_nrc_sentiment(moto) 
head(nrc_moto,n=20) # check the scores for 20 sentenses
barplot(colSums(nrc_moto),las=2,col = rainbow(10),ylab = 'Count',main = 'Emotion Score_nrc')

get_sent <- get_sentences(moto)
class(get_sent)
str(get_sent)
head(get_sent)

# Emotion Mining by bing method ->

sentiment_vector <- get_sentiment(get_sent, method = "bing") # Get Sentiment scores by bing method
head(sentiment_vector,n=50)
sum(sentiment_vector) # 95
mean(sentiment_vector) # 0.05
summary(sentiment_vector) # Min: -3, Max: 6
# plot Trajectory
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

negative_bing <- get_sent[which.min(sentiment_vector)] # Most negative emotional sentence
negative_bing
positive_bing <- get_sent[which.max(sentiment_vector)] # Most positive emotional sentence
positive_bing

# Emotion Mining by afinn method ->

afinn_s_v <- get_sentiment(get_sent, method = "afinn")# Get Sentiment scores by afinn method
head(afinn_s_v)
sum(afinn_s_v) # 307
mean(afinn_s_v) # 0.17
summary(afinn_s_v) # Min: -5, Max: 15
# plot Trajectory
plot(afinn_s_v, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

negative_afinn <- get_sent[which.min(afinn_s_v)] # Most negative emotional sentence
negative_afinn
positive_afinn <- get_sent[which.max(afinn_s_v)] # Most positive emotional sentence
positive_afinn

# Emotion Mining by syuzhet method ->

syuzhet_vector <- get_sentiment(get_sent, method="syuzhet")# Get Sentiment scores by syuzhet method
head(syuzhet_vector)
sum(syuzhet_vector) # 145.1
mean(syuzhet_vector) # 0.08
summary(syuzhet_vector) # Min: -2, Max: 4.45
plot(syuzhet_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

negative_syuzhet <- get_sent[which.min(syuzhet_vector)] # Most negative emotional sentence
negative_syuzhet
positive_syuzhet <- get_sent[which.max(syuzhet_vector)] # Most positive emotional sentence
positive_syuzhet

################ SEMENTIC ANALYSIS ON AMAZON CUSTOMER REVIEW ######################

library(rJava)
library(tm)		
library(SnowballC)
library(wordcloud)
library(RWeka)	
library(qdap)		
library(textir)
library(maptpx)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)

makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 

# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negatice wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching negtive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names,freq_neg,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}



words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}


# --- func to make cluster dendograms --- #
clusdend = function(a){	# writing func clusdend() 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  # making dissimilarity matrix out of the freq one
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  # now plot collocation dendogram
  d <- dist(test2, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward")
  windows()
  plot(fit) # display dendogram
} # clusdend() func ends


# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos.words,"wow", "kudos", "hurray","great") #including our own positive words to the existing list
neg.words = c(neg.words,"disgust","pathetic") # include own negative words
stopwdrds = readLines(file.choose())

# Loading the text file to be analysed 
x = readLines(file.choose()) 	# Moto E5 Plus.txt
x[1:20]
x <- stemDocument(x)
x[1:20]

# Preparing corpus from the text document 
x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input
inspect(x1[c(1:20)])

x1 = tm_map(x1, tolower)		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords("english"),stopwdrds))
x1 = tm_map(x1, stripWhitespace) 	# removes white space

# Term document frequency matrix
tdm0 <- TermDocumentMatrix(x1)

# Term document matrix with inverse frequency 
tdm1 <- TermDocumentMatrix(x1,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwdrds=T)) #,stemming=T))
inspect(tdm1)
a0 <- NULL
a1 <- NULL
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

a0
a1

# Removing empty docs 
tdm0 <- tdm0[,-a0]
tdm1 <- tdm1[,-a1]

# Document term matrix 
dtm0 <- t(tdm0)
dtm1 <- t(tdm1)

# Word cloud - TF - Uni gram
makewordc(tdm0)
title(sub = "UNIGRAM - Wordcloud using TF")

# Frequency Bar plot - TF - Uni gram
words_bar_plot(tdm0)

# Word cloud - TFIDF - Unigram
makewordc(tdm1)

# Frequency Barplot - TFIDF - Unigram
words_bar_plot(tdm1)

# Positive word cloud - TF - Unigram
makeposwordc(tdm0)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TF")

# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)

# Positive word cloud - Unigram - TFIDF
makeposwordc(tdm1)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")

# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm1)

# Negative word cloud - TF - unigam
makenegwordc(tdm0) 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TF")

# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm0)

# Negative word cloud - Unigram - TFIDF
makenegwordc(tdm1) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")

# Frequency Barplot - Negative words - TFIDF
neg_words_bar_plot(dtm1)

############################ END  ##########################

