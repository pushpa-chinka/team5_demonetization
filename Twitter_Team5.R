

#------------------------Cleaning of the twitter tweets ---------------------------------------------------

data <- read.csv("demonetization-tweets.csv", sep = ",", h = TRUE)
#loading tweets
twitter_tweets <- data.frame( tweet_text = data$text)

#removing retweets
tweets <- gsub("(RT|via|@)((?:\\b\\W*@\\w+):+)", "", twitter_tweets$tweet_text)
# removing html links
tweets <- gsub("http[^[:blank:]]+","",tweets)
#removing @NAMES
tweets<-gsub("@\\w+","",tweets)
#removing punctuations
tweets <- gsub("[[:punct:]]"," ",tweets)
#removing numbers
tweets <- gsub("[^[:alnum:]]"," ",tweets)
#creating a new column in data Frame
twitter_tweets$tweet_text = tweets
#writing tweets to the dataSet
write.csv(twitter_tweets,file = "Processed_twitterDataSet.csv") 
#Reading the dataSet
twitter_tweets = read.csv("Processed_twitterDataSet.csv")
time_df = data.frame(Created = data$created)
time_df
#identifying the Hours
time_df$Hours = format(as.POSIXct(time_df$Created), format = "%H")
#creating the Hours column 
twitter_tweets$Hour = time_df$Hours
#Writing Hours into dataSet
write.csv(twitter_tweets,file = "Processed_twitterDataSet.csv")
#identifying the date
time_df$Date = format(as.POSIXct(time_df$Created), format = "%d")
#Creating a Date column in Dataset
twitter_tweets$Date = time_df$Date
#Writing Date into dataSet
write.csv(twitter_tweets,file = "Processed_twitterDataSet.csv")

#cleaning the statuSource
platform_data = data.frame(Platform_Source = data$statusSource)
platform_data$Platform_Source

platform= (sub("<?/?a?>", "", (sub("<[^>]*>", "", (platform_data$Platform_Source)))))
platform = gsub("(Twitter for)|(Twitter)", "", platform)
names(platform)
platform
#creating  a Platform column in Dataset
twitter_tweets$Platform = platform
#Writing Date into dataSet
write.csv(twitter_tweets,file = "Processed_twitterDataSet.csv")
retweets = data$retweetCount
#Creating retweet column in the dataset
twitter_tweets$re_tweets = retweets
#Writing retweet into dataSet
write.csv(twitter_tweets,file = "Processed_twitterDataSet.csv")


#----------------------------Sentiment Analysis--------------------------------------
library(sentimentr)
library(syuzhet)
twitter_tweets = read.csv("Processed_twitterDataSet.csv", sep = ",", h = TRUE)
#compares the tweets with 10 different emotions
tweet_emotion<-get_nrc_sentiment(char_v = tweets)
#initializing the columns negative , positive and neutral
twitter_tweets$negative = tweet_emotion$negative#negative emotion
twitter_tweets$positive = tweet_emotion$positive#positive emotion
twitter_tweets$neutral = 0
names(twitter_tweets)
#calculating the neutal value for each sentiment
for(i in c(1:8000)){
  if(twitter_tweets$negative[i] == 0 & twitter_tweets$positive[i] == 0) {
    twitter_tweets$neutral[i] = 1#identifying neutral emotion based on positive and negative
  }
}

write.csv(twitter_tweets,file = "Processed_twitterDataSet.csv")#writing neutral values into csv file
twitter_tweets$total_sentiment = "neutral"
for(i in c(1:8000)){#identifying a total tweets either as positive or negative or neutral
  if(twitter_tweets$neutral[i] == 1){
    twitter_tweets$total_sentiment[i] = "neutral"
  }
  else if(twitter_tweets$positive[i] > twitter_tweets$negative[i]){
    twitter_tweets$total_sentiment[i] = "positive"
  }
  else if(twitter_tweets$positive[i] < twitter_tweets$negative[i]){
    twitter_tweets$total_sentiment[i] = "negative"
  }
}
write.csv(twitter_tweets,file = "Processed_twitterDataSet.csv")#writing the total_sentiment into csv file

#------------------------------Plotting of the Relations------------------------------------------

#+++++++++++++++Plot 1 : No.Of.Tweets Vs Sentiment(Positive, Negative, Neutral)++++++++++++++++++++++++++++++++++

TweetFreq = data.frame(table(twitter_tweets$total_sentiment))
ggplot(TweetFreq, aes(x = Var1, y = Freq, fill = Var1 ))+geom_bar(stat = "identity")+ggtitle("Sentiment Analysis")+xlab("Sentiments")+ylab("No.of Tweets")




##++++++++++++++++++++++plot 2: word cloud ++++++++++++++++++++++++++++##
install.packages("slam")
install.packages("tm")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)



?Corpus()
?tm_map()
?VectorSource
?PlainTextDocument
?remove
docs <- c("This is a text.", "This another one.")
(vs1 <- VectorSource(docs))
vs1
inspect(VCorpus(vs))


input <- c('This is line one.','And this is the second one')

Create_the_source <- VectorSource(input)
Create_the_source
the_corpus = VCorpus(Create_the_source)
the_corpus






tweet_file <- read.csv('cleaned_tweets.csv', stringsAsFactors = FALSE)
names(tweet_file)
words_docs <- Corpus(VectorSource(tweet_file$tweet_text))
words_docs$Contents
words_docs <- tm_map(words_docs, PlainTextDocument)
words_docs <- tm_map(words_docs, removeWords, stopwords('english'))
wordcloud(words_docs, max.words = 200, random.order = FALSE)









#+++++++++++++++Plot 2 :  Sentiment Frequency ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plotrix)
#Initializing the Variables
pos_retweet_count = 0
neg_retweet_count = 0
neutral_retweet_count = 0
#Calculating the retweet_count of Sentiment
for(i in c(1:8000)){
  if(twitter_tweets$total_sentiment[i] == "positive"){
    pos_retweet_count = pos_retweet_count + twitter_tweets$re_tweets[i] 
    
  } else  if(twitter_tweets$total_sentiment[i] == "negative"){
    neg_retweet_count =  neg_retweet_count+  twitter_tweets$re_tweets[i] 
    
  }else if(twitter_tweets$total_sentiment[i] == "neutral"){
    
    neutral_retweet_count = neutral_retweet_count +twitter_tweets$re_tweets[i] 
  }
}
retweet_counts = c(pos_retweet_count,neg_retweet_count, neutral_retweet_count)
labels = c("positive", "negative", "neutral")
#Rounding off the Retweet count
percent = round(retweet_counts/sum(retweet_counts)*100)
#Labeling the axes
labels = paste(labels, percent)
labels = paste(labels, "%", sep = "")
#plotting the Graphs
pie3D(retweet_counts, labels = labels, col = rainbow(length(labels)), main = "How Users reacted To Tweets")


#+++++++++++++++++Plot 3 : Time Vs no.of users ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(ggplot2)
cleaned_data = read.csv("cleaned_tweets_sentiment.csv")
tweets_time = (cleaned_data$Hour)
tweets_time
tt = tweets_time

qplot(tt,
      geom="histogram",
      binwidth = 0.5,  
      main = "Twitter Time Usage", 
      xlab = "Time",  
      fill= I("blue"))


dev.off()
#++++++++++++++++++++Plot 4 : Twitter Users From Different platforms ++++++++++++++++++++++++++++++++++++++++++++++++
dt = read.csv("demonetization-tweets.csv")
vec = (sub("<?/?a?>", "", (sub("<[^>]*>", "", (dt$statusSource)))))
vec
vec = gsub("(Twitter for)|(Twitter)", "", vec)
vec
vec_df = data.frame(table(vec))
vec_df

names(vec_df)
num = vec_df$Freq
num 

num = sort(num)
num
freq_num = tail(num, n = 4)
freq_num
8000 - (4247+1407+1098+260)

freq_num = append(freq_num, 998)
#p = rep(NA, length(freq_num))
r = c(1:length(freq_num))
j = 1
for(i in freq_num){
  print(i)
  r[j] =  toString(vec_df$vec[vec_df$Freq == i])
  j = j + 1
}
r = r[1:4]
r
r = append(r, "Others")
r = unique(r)
final_df = data.frame(platfroms = r, No.of_users = freq_num)
final_df
library(ggplot2)

ggplot(final_df, aes(x = platfroms, y = No.of_users, fill = platfroms))+geom_bar(stat = "identity")+ggtitle("Twitter Users From Differt Platforms")




#++++++++++++++++++++++++++++plot 5: Users React To Which Types Of Plots+++++++++++++++++++++++#
data <- read.csv("demonetization-tweets.csv", sep = ",", h = TRUE)
twitter_tweets = read.csv("cleaned_tweets_sentiment.csv")
time_df = data.frame(Created = data$created)
time_df
#identifying the time
time_df$Hours = format(as.POSIXct(time_df$Created), format = "%H")
time_df$Hours
twitter_tweets$Hour = time_df$Hours
write.csv(twitter_tweets,file = "cleaned_tweets_sentiment.csv")
#identifying the date
time_df$Date = format(as.POSIXct(time_df$Created), format = "%d")
time_df$Date
twitter_tweets$Date = time_df$Date

write.csv(twitter_tweets,file = "cleaned_tweets_sentiment.csv")

#cleaning the statuSource
platform_data = data.frame(Platform_Source = data$statusSource)
platform_data$Platform_Source

platform= (sub("<?/?a?>", "", (sub("<[^>]*>", "", (platform_data$Platform_Source)))))
platform = gsub("(Twitter for)|(Twitter)", "", platform)
names(platform)
platform
length(platform)

platform[1]
platform[10]
twitter_tweets$Platform = platform

#adding retweets to cleaned data
retweets = data$retweetCount
twitter_tweets$re_tweets = retweets

write.csv(twitter_tweets,file = "cleaned_tweets_sentiment.csv")

#plot  retweets reaction
pos_retweet_count = 0
neg_retweet_count = 0
neutral_retweet_count = 0

for(i in c(1:8000)){
  if(twitter_tweets$total_sentiment[i] == "positive"){
    pos_retweet_count = pos_retweet_count + twitter_tweets$re_tweets[i] 
    
  } else  if(twitter_tweets$total_sentiment[i] == "negative"){
    neg_retweet_count =  neg_retweet_count+  twitter_tweets$re_tweets[i] 
    
  }else if(twitter_tweets$total_sentiment[i] == "neutral"){
    
    neutral_retweet_count = neutral_retweet_count +twitter_tweets$re_tweets[i] 
  }
}
retweet_counts = c(pos_retweet_count,neg_retweet_count, neutral_retweet_count)
retweet_counts

labels = c("positive", "negative", "neutral")
labels
percent = round(retweet_counts/sum(retweet_counts)*100)
percent
labels = paste(labels, percent)
labels = paste(labels, "%", sep = "")
library(plotrix)
pie3D(retweet_counts, labels = labels, col = rainbow(length(labels)), main = "How Users reacted To Tweets")






#++++++++++++++++++ additional: users reactions according to the date++++++++++++++++#
cleaned_data = read.csv("cleaned_tweets_sentiment.csv")

sent_freq_per_day = table( cleaned_data$total_sentiment, cleaned_data$Date)

barplot(sent_freq_per_day, main="People reaction on 22nd and 23rd",
        xlab="Sentiment", ylab = "Frequency " ,col=c("red", "blue" , "green"),
        beside=TRUE) 
legend("topleft", c("Negative", "Positive","Neutral"), cex=1.3, bty="n", fill =c("red", "blue" , "green"))



