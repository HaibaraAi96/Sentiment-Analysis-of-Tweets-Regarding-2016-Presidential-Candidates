library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)
consumer_key<-"*******"
consumer_secret<-"*****"
access_token<-"****"
#the above three are re-dacted due to rpivacy reasons
access_secret<-"pgPQx0WTgBQWHQTGcEU6VFfSiscccvciPQg6ziYD8ejf3"
#handshake authorization
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
trump<-searchTwitter('DonaldTrump',n=1500,lang='en',resultType="recent")
hillary<-searchTwitter('HillaryClinton',lang='en',n=1500,resultType="recent")
bernie<-searchTwitter('BernieSanders',lang="en",n=1500,resultType="recent")
#mii is a list
trump_text<-sapply(trump,function(x) x$getText())
hillary_text<-sapply(hillary,function(x) x$getText())
bernie_text<-sapply(bernie,function(x) x$getText())


#we need to convert this char array to corpus data
#mii_corpus<-Corpus(VectorSource(mii_text))
#corpus does not behave like a standard dataframe
#The below code cleans the text
# mii_clean<-tm_map(mii_corpus,removePunctuation)
# mii_clean<-tm_map(mii_clean,content_transformer(tolower))
# mii_clean<-tm_map(mii_clean,removeWords,stopwords("english"))
# mii_clean<-tm_map(mii_clean,removeNumbers)
# mii_clean<-tm_map(mii_clean,stripWhitespace)
# mii_clean<-tm_map(mii_clean,removeWords,c("India","india","http",":","#MakeInIndia","makeinindia","good","great","speakers","packed","timesnow","cxos","press","conference","delegates"))
# wordcloud(mii_clean,random.order=F,scale=c(6,0.5))
# 
source("sentiment.r")
pos<-scan('positive-words.txt',what="character",comment.char=";")
neg<-scan('negative-words.txt',what="character",comment.char=";")
trump.analysis<-score.sentiment(trump_text,pos,neg)
hillary.analysis<-score.sentiment(hillary_text,pos,neg)
bernie.analysis<-score.sentiment(bernie_text,pos,neg)
png("histogram.png")
par(mfrow=c(2,3))
hist(trump.analysis$score,main="Histogram_Donald")
abline(v=mean(trump.analysis$score),col="blue",lwd=2)
hist(hillary.analysis$score,main="Histogram_Hillary")
abline(v=mean(hillary.analysis$score),col="blue",lwd=2)
hist(bernie.analysis$score,main="Histogram_Bernie")
abline(v=mean(bernie.analysis$score),col="blue",lwd=2)
sum_score<-c(sum(trump.analysis$score),sum(hillary.analysis$score),sum(bernie.analysis$score))
names(sum_score)<-c("Trump","Hillary","Bernie")
barplot(sum_score,main="Total Scores")
dev.off()






