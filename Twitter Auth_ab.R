# Login in at https://apps.twitter.com
setwd('C:/Users/Abhinav/Documents/My Code/Jigshaw/Code/R Code/Text analytics')

library(twitteR)
library(ROAuth)
library(bitops)
library(syuzhet)
library(plotly)
library(dplyr)

consumer_key<-"HPJIxBPliuAH5XTKV1nxb6pii"
consumer_secret<-"d6IFaPQ1pbD1MyeJK1OfWLSjZRMRhanlsUhXiGP4kFF1rK6LxO"
token_key<-"1023116276816928769-7sKyvt22yKs4dGnBmq4wIXudaVPcQc"
token_secret<-"kQIjgJODHvw21HNZDyjUi75ke8w8rNtXO5SZ8yF1HZinY"

setup_twitter_oauth(consumer_key,consumer_secret,token_key,token_secret)
#Fetaching tweet from a specfic user
dtweets <- userTimeline("realDonaldTrump", n=200)
ddf <- twListToDF(dtweets)

#what is trending on Twitter
Locs <- availableTrendLocations()
LocsIndia = subset(Locs, country == "India")                
woeidDelhi = subset(LocsIndia, name == "Delhi")$woeid
#Getting Delhi Trends
trends = getTrends(woeid=woeidDelhi)

tweets<-searchTwitter("#AyushmannKhurrana -filter:retweets",n=2000,lang = 'en')

df <- twListToDF(tweets)
rm(tweets)
head(df$text)

#Cleaning twitter Data
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # Try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

cleanTweets<- function(tweet){
  # Clean the tweet for sentiment analysis
  # remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from
  #the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet = gsub("[^[:alnum:][:blank:]?&/\\-]", "", tweet)
  tweet = gsub("U00..", "", tweet)
  # if anything else, you feel, should be removed, you can.
  #For example "slang words" etc using the above function and methods.
  # Next we'll convert all the word in lower case.
  #This makes uniform pattern.
  tweet = catch.error(tweet)
  tweet
}

cleanTweetsAndRemoveNAs<- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

tweetclean <- cleanTweetsAndRemoveNAs(df$text)

#Using nrc emotion lexion for sentiment
emotion.df <- get_nrc_sentiment(tweetclean)
emo_bar = colSums(emotion.df)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments
plot_ly(x=emo_sum$emotion, y=emo_sum$count, type="bar", color=emo_sum$emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for #AyushmannKhurrana")

textwithemotion <- cbind(tweetclean,emotion.df)

#Using four lexicons in syuzhet pacakage

sent.value <- get_sentiment(tweetclean)

syuzhet <- get_sentiment(tweetclean, method="syuzhet")
bing <- get_sentiment(tweetclean, method="bing")
afinn <- get_sentiment(tweetclean, method="afinn")
nrc <- get_sentiment(tweetclean, method="nrc")

sentiments <- data.frame(tweetclean,syuzhet, bing, afinn, nrc)

most.positive <- word.df[sent.value == max(sent.value)]

positive.tweets <- data.frame(tweetclean[sent.value > 0])

negative.tweets <- data.frame(tweetclean[sent.value < 0])

netural.tweets <- data.frame(tweetclean[sent.value == 0])

library(tm)
library(wordcloud)

# Comparison word cloud
all = c(
  paste(tweetclean[emotion.df$anger > 0], collapse=" "),
  paste(tweetclean[emotion.df$anticipation > 0], collapse=" "),
  paste(tweetclean[emotion.df$disgust > 0], collapse=" "),
  paste(tweetclean[emotion.df$fear > 0], collapse=" "),
  paste(tweetclean[emotion.df$joy > 0], collapse=" "),
  paste(tweetclean[emotion.df$sadness > 0], collapse=" "),
  paste(tweetclean[emotion.df$surprise > 0], collapse=" "),
  paste(tweetclean[emotion.df$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
#
# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
#
# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

#Read the dictionaries
pos = scan('positive.txt',what='character',comment.char=';')
neg = scan('negative.txt',what='character',comment.char=';')

#Adding words to dictionaries
pos[2007:2013]<-c("spectacular","everyday","better","top","thumbs","four","five")
neg[4784:4789]<-c("one","two","careful","sync","Beware","suck")

#Famous Jeffreybreen Algorithm to "Tag" sentiments to sentences

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  #we got a vector of sentences. plyr will handle a list
  #or a vector as an "l" for us
  #we want a simple array of scores back, so we use
  #"l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    #clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence) #removes punctuations
    sentence = gsub('[[:cntrl:]]', '', sentence) #removes control characters
    sentence = gsub('\\d+', '', sentence) #removes digits
    
    #and convert to lower case:
    sentence = tolower(sentence)
    
    #split sentences into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    #sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    #compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    #match() returns the position of the matched term or NA
    #we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    #and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

analysis<-score.sentiment(tweetclean, pos, neg, .progress="text")
names(analysis)
View(analysis)
str(analysis)
#Checking out overall sentiment
table(analysis$score)
mean(analysis$score)
hist(analysis$score)


analysis$text<-as.character(analysis$text)
str(analysis)
analysis$sentiment<-ifelse(analysis$score>0,"positive",ifelse(analysis$score<0,"negative","neutral"))

#sentiment Method 3 by using sentiment pacakge
library(sentiment)

TweetsClassEmo = classify_emotion(tweetclean,algorithm="bayes", prior=1.0)
TweetEmotion = TweetsClassEmo[,7]
TweetEmotion[is.na(TweetEmotion)] = "unknown"

TweetsClassPol = classify_polarity(tweetclean,algorithm="bayes")
TweetPol = TweetsClassPol[,4]

SentimentDataFrame = data.frame(text=tweetclean,emotion=TweetEmotion, polarity=TweetPol, stringsAsFactors=FALSE)

