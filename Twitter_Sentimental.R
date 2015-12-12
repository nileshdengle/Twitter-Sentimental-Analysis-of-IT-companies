twitter <- function(tweet_search,n=300){
  library(twitteR)
  library(sentiment)
  library(ggplot2)
  library(dplyr)
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  twits <- searchTwitteR(tweet_search,n,lang = "en")
  twit_txt <- sapply(twits,function(x) x$getText())
  twit_txt <- gsub("(RT|via)(?:\\b\\W*@\\w+)+","",twit_txt)
  twit_txt <- gsub("@\\w+","",twit_txt)
  twit_txt <- gsub("[[:punct:]]","",twit_txt)
  twit_txt <- gsub("[[:digit:]]","",twit_txt)
  twit_txt<- gsub("http\\w+","",twit_txt)
  twit_txt <- data.frame(twit_txt)
  twit_txt <- mutate_each(twit_txt,funs(tolower))
  twit_emo <- classify_emotion(twit_txt,algorithm = "bayes",prior = 1.0)
  emotion <- twit_emo[,7]
  emotion[is.na(emotion)] <- "unknown"
  twit_pol <- classify_polarity(twit_txt)
  polar <- twit_pol[,4]
  sentiment_dataframe <- data.frame(text=twit_txt,emotions=emotion,polarity=polar,stringsAsFactors = F)
  ggplot(sentiment_dataframe,aes(x=emotions))+geom_bar(aes(y = ..count..,fill=emotions))+scale_fill_brewer(palette = "Dark2")+ggtitle('Sentimental analysis')+theme(legend.position='right')+ylab('number of tweets')+xlab('Emotion Categories')
  View(sentiment_dataframe)
  View(twit_txt)
  
}