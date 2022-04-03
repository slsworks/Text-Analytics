#' Title: NBA Fan Engagement - Tweet Analysis
#' Purpose: NLP- Case 1
#' Author: Sree Duddupudi
#' email: sduddupudi@student.hult.edu
#' Date: March 2022


############################# SETTING UP THE LIBRARIES ##########################

library(ggplot2)
library(ggthemes)
library(tm)
library(qdap)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stringi)
library(dplyr)
library(ggdendro)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(textdata)

#Options and Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

######################### SETTING UP THE WORKING DIRECTORY ######################
setwd('~/Text-Mining-NLP/Case/Case I/Data')


################# CREATING UNIVERSAL FUNCTIONS FOR ANALYSIS #####################
#For lower case text
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

#Setting a function to clean the corpus
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}
# stopwords list
stops <- c(stopwords("SMART"),
                 "nba","celtics", "coach","butler", "game","will","lol",
                 "score","us","league","team","heat", "bam",
                 "playoffs", "los", "angeles", "games", "finals", "season",
                 "championship", "won", "win", "tonight", "rt","evidence", "scandal", "miamiheat", 
                 "nbatwitter", "twitter", "announcement", "basketball","rockets", "raptors", "lakers", "bulls", "golden",
                 "warriors", "knicks", "bucks", "clippers", "suns", "mavericks", "jazz", "magic", "hawks", "nuggets",
                 "kings", "pistons", "san", "spurs", "pelicans", "pacers", "wizards", "blazers","trail", "hornetts", 
                 "grizzlies", "cavaliers", "timberwolves", "city", "thunder", "sixers")


############################ DATA LOADING AND PROCESSING ########################
#Setting up the first three months in different data frames
df1 <-read.csv("A_Oct2019.csv", encoding ='UTF-8',header=TRUE)
df2 <-read.csv("B_Nov2019.csv", encoding ='UTF-8',header=TRUE)
df3 <-read.csv("C_Dec2019.csv", encoding ='UTF-8',header=TRUE)
df4 <-read.csv("E_Feb2020.csv", encoding ='UTF-8',header=TRUE)
df5 <-read.csv("K_Aug2020.csv", encoding ='UTF-8',header=TRUE)
df6 <-read.csv("L_Sep2020.csv", encoding ='UTF-8',header=TRUE)
df7 <-read.csv("M_Oct2020.csv", encoding ='UTF-8',header=TRUE)

#Binding all the 7 dfs in one large df 
text_bind1 <- rbind(df1[sample(nrow(df1), 5000), ],df2[sample(nrow(df2), 5000), ],
                    df3[sample(nrow(df3), 5000), ],df4[sample(nrow(df4), 5000), ],
                        df5[sample(nrow(df5), 5000), ], df6[sample(nrow(df6), 5000), ],
                        df7[sample(nrow(df7), 5000), ])
head(text_bind1)


#Exploring the frequency w.r.t. to  teams 
teams<- text_bind1$team
freq_teams <- sort(table(unlist(strsplit(teams, " "))),      # Create frequency table
               decreasing = TRUE)
freq_teams                             

########################## FREQUENCY AND TERM ASSOCIATIONS ######################
# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text_bind1$text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make a Document Term Matrix or Term Document Matrix depending on analysis
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)
txtTdmM <- as.matrix(txtTdm)

# Get the most frequent terms
topTermsA <- colSums(txtDtmM)
topTermsB <- rowSums(txtTdmM)
topTermsA
topTermsB

# Add the terms
topTermsA <- data.frame(terms = colnames(txtDtmM), freq = topTermsA)
topTermsB <- data.frame(terms = rownames(txtTdmM), freq = topTermsB)

# Remove row attributes
rownames(topTermsA) <- NULL
rownames(topTermsB) <- NULL

# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]
head(exampleReOrder)

# most frequent terms
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# Reduce TDM
reducedTDM <- removeSparseTerms(txtTdm, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

########################## WORD CLOUD & BAR PLOTS################################
#Setting up the data
txtTdm  <- TermDocumentMatrix(txtCorpus)
m <- as.matrix(txtTdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#Plotting wordclouds
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#Setting up the low frequency 
findFreqTerms(txtTdm, lowfreq = 700)

#Finding Association w.r.t. 'Nike', 'Adidas' and 'sales'
findAssocs(txtTdm, terms = "nike", corlimit = 0.2)
findAssocs(txtTdm, terms = "adidas", corlimit = 0.2)
findAssocs(txtTdm, terms = "sales", corlimit = 0.2)

#Plotting the bar plot for most frequent terms
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


###########################OVERALL SENTIMENT ANALYSIS ###########################
text_bind1$doc_id <-as.character(text_bind1$doc_id)
d <- data_frame(txt = text_bind1)
d %>%
  unnest_tokens(word, txt)

#Joy
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
joy <- nrow(nrc_joy)

#Anger
nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")
anger <- nrow(nrc_anger)

#Positive
nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")
positve <- nrow(nrc_positive)

#Sadness
nrc_sadness <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")
sadness<- nrow(nrc_sadness)

#Negative
nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")
negative <- nrow(nrc_negative)

#Creating a bar pot for the sentiments
sentimentsdf <- data.frame(sentiment = c("joy", "anger", "positive", "sadness", "negative"),
                           len = c(nrow(nrc_joy), nrow(nrc_anger),nrow(nrc_positive), nrow(nrc_sadness)
                                   , nrow(nrc_sadness) ))
library(ggplot2)
# Basic barplot
sentimentplot<-ggplot(sentimentsdf, aes(x=sentiment, y=len, fill=sentiment)) +
  geom_bar(stat="identity", fill = "light blue")+theme_minimal() + theme(legend.position="none")
sentimentplot

########################## CATEGORY: BRAND ANALYSIS #############################
library(dplyr)
#NIKE data pull
nike_df <- rbind(df1 %>% filter(grepl('Nike', text, ignore.case = T)), 
                 df2 %>% filter(grepl('Nike', text, ignore.case = T)),
                 df3 %>% filter(grepl('Nike', text, ignore.case = T)),
                 df4 %>% filter(grepl('Nike', text, ignore.case = T)),
                 df5 %>% filter(grepl('Nike', text, ignore.case = T)),
                 df6 %>% filter(grepl('Nike', text, ignore.case = T)),
                 df7 %>% filter(grepl('Nike', text, ignore.case = T)))

#Pre processing the corpus
# Make a volatile corpus
nike_corpus <- VCorpus(VectorSource(nike_df$text))

# Preprocess the corpus
nike_corpus <- cleanCorpus(nike_corpus, stops)

# Make a Document Term Matrix or Term Document Matrix depending on analysis
nike_dtm  <- DocumentTermMatrix(nike_corpus)
nike_tdm  <- TermDocumentMatrix(nike_corpus)
nike_tdmM <- as.matrix(nike_tdm)

# Get Row Sums & organize
nike_tdmv <- sort(rowSums(nike_tdmM), decreasing = TRUE)
nikeDF   <- data.frame(word = names(nike_tdmv), freq = nike_tdmv)

#Creating a wordcloud
wordcloud(nike_corpus[1:1000], color = "black")

#ADIDAS data pull
adi_df <- rbind(df1 %>% filter(grepl('adidas', text, ignore.case = T)), 
                 df2 %>% filter(grepl('adidas', text, ignore.case = T)),
                 df3 %>% filter(grepl('adidas', text, ignore.case = T)),
                df4 %>% filter(grepl('adidas', text, ignore.case = T)),
                df5 %>% filter(grepl('adidas', text, ignore.case = T)),
                df6 %>% filter(grepl('adidas', text, ignore.case = T)),
                df7 %>% filter(grepl('adidas', text, ignore.case = T)))

#Pre processing the corpus
# Make a volatile corpus
adi_corpus <- VCorpus(VectorSource(adi_df$text))

# Preprocess the corpus
adi_corpus <- cleanCorpus(adi_corpus, stops)

# Make a Document Term Matrix or Term Document Matrix depending on analysis
adi_dtm  <- DocumentTermMatrix(adi_corpus)
adi_tdm  <- TermDocumentMatrix(adi_corpus)
adi_tdmM <- as.matrix(adi_tdm)
# Get Row Sums & organize
adi_tdmv <- sort(rowSums(adi_tdmM), decreasing = TRUE)
adiDF   <- data.frame(word = names(adi_tdmv), freq = adi_tdmv)

#Creating a wordcloud
wordcloud(adi_corpus[1:1000], color = "blue")

##########################NIKE SENTIMENT ANALYSIS################################
#Tokenizing
tokens <- data_frame(text = nike_df$text) %>% unnest_tokens(word, text)

# get the sentiment from the first text: 
tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words
#Finalizing the data
n_data <- data.frame(n_sentiment=c("Negative", "Postive", "Difference"),
                       len=c(1371, 4071, 2700))
head(n_data)

# Basic barplot
n_sentiment_plot<-ggplot(data=n_data, aes(x=n_sentiment, y=len)) +
  geom_bar(stat="identity", fill = "black")

n_sentiment_plot

####################ADIDAS SENTIMENT ANALYSIS####################################
library(tidyverse)
library(tidytext)
library(stringr)
tokens <- data_frame(text = adi_df$text) %>% unnest_tokens(word, text)

# get the sentiment from the first text: 
tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words

#Finalizing the data
adi_data <- data.frame(adi_sentiment=c("Negative", "Postive", "Difference"),
                 len=c(354, 1023, 669))
head(adi_data)

# Basic barplot
adi_sentiment_plot<-ggplot(data=adi_data, aes(x=adi_sentiment, y=len)) +
  geom_bar(stat="identity", fill = "dark red")

adi_sentiment_plot

#############################The End ############################################




