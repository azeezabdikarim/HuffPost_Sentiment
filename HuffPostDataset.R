library(shiny)
library(plotly)
library(rjson)
library(dplyr)
library(jsonlite)
library(SnowballC)
library(syuzhet)
library(ggplot2)
library(hrbrthemes)
library(zoo)
library(tm)
library(scales)


df =  read.csv("News_Category_Dataset_v2.csv")

print(df)
colnames(df)

corpus <- iconv(df$headline)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:7])

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:7])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:7])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:7])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:7])

cleanset <- tm_map(cleanset, removeWords)
inspect(cleanset[1:7])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:7])

cleanset_df = data.frame(text = sapply(cleanset, as.character), stringsAsFactors = FALSE)


df$clean_sentences = cleanset_df

?get_sentiment()
print(head(df$clean_sentences,3))

# Get Sentiment Scores
df$sentiment_syuzhet <- get_sentiment(unlist(df$clean_sentences), method="syuzhet")
df$sentiment_bing <- get_sentiment(unlist(df$clean_sentences), method="bing")
df$sentiment_afinn <- get_sentiment(unlist(df$clean_sentences), method="afinn")
df$sentiment_nrc <- get_sentiment(unlist(df$clean_sentences), method="nrc", lang="english")

pos_neg_nut = function(score){
  if (score < 0) {
    return (-1)
  } else if ( score > 0) {
    return (1)
  } else
    return (0)
}

df$sentiment_syuzhet_binary <- lapply(df$sentiment_syuzhet, pos_neg_nut)
df$sentiment_bing_binary <- lapply(df$sentiment_bing, pos_neg_nut)
df$sentiment_afinn_binary <- lapply(df$sentiment_afinn, pos_neg_nut)
df$sentiment_nrc_binary <- lapply(df$sentiment_nrc, pos_neg_nut)

df %>% group_by(date) %>% summarise( headline_count = n()) -> headlines_by_day
colnames(headlines_by_day)
tail(headlines_by_day,20)

# Most basic bubble plot
p <- ggplot(headlines_by_day, aes(x=date, y=headline_count, group = 1)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p


rolling_interval = 14
headlines_by_day$date = as.Date(paste(headlines_by_day$date, 1, sep="-"), format="%Y-%m-%d")
headlines_by_day$headline_count_week_roll =  rollmean(headlines_by_day$headline_count, rolling_interval,
                                                      align="left",
                                                      fill=0)
article_count_rolling <- ggplot(head(headlines_by_day,-rolling_interval), aes(x=date, y=headline_count_week_roll, group = 1)) +
  geom_line() +
  labs(title = "Published New Headlines (14 day rolling average)",
       x = "Date",
       y = "# of Published Headlines") +
  ylim(0, 105) +
  scale_x_date(breaks=date_breaks("3 months"),
                labels=date_format("%b - %y")) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1, size =10))
article_count_rolling

article_count_first_month <- ggplot(head(headlines_by_day, 30), aes(x=date, y=headline_count, group = 1)) +
  geom_line() +
  labs(title = "Published New Headlines (first 30 days)",
       x = "Date",
       y = "# of Published Headlines") +
  ylim(0, 105) +
  scale_x_date(breaks=date_breaks("1 week"),
               labels=date_format("%Y-%m-%d")) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1, size =10))
article_count_first_month

df %>% group_by(date, sentiment_syuzhet_binary) %>% summarise(sentiment_counts = n()) -> syuzhet_daily_sums
syuzhet_daily_percent = syuzhet_daily_sums %>% group_by(date) %>% mutate(percent = sentiment_counts/sum(sentiment_counts)) 

df %>% group_by(date, sentiment_bing_binary) %>% summarise(sentiment_counts = n()) -> bing_daily_sums
bing_daily_percent = bing_daily_sums %>% group_by(date) %>% mutate(percent = sentiment_counts/sum(sentiment_counts)) 

df %>% group_by(date, sentiment_afinn_binary) %>% summarise(sentiment_counts = n()) -> afinn_daily_sums
afinn_daily_percent = afinn_daily_sums %>% group_by(date) %>% mutate(percent = sentiment_counts/sum(sentiment_counts)) 

df %>% group_by(date, sentiment_nrc_binary) %>% summarise(sentiment_counts = n()) -> nrc_daily_sums
nrc_daily_percent = nrc_daily_sums %>% group_by(date) %>% mutate(percent = sentiment_counts/sum(sentiment_counts)) 

# Daily Percent of Syuzhet Sentiment
syuzhet_daily_percent <- data.frame()
for (d in unique(df$date)){
  positive_count = count(filter(df, (sentiment_syuzhet_binary == 1) & (date == d)))
  negative_count = count(filter(df, (sentiment_syuzhet_binary == -1) & (date == d)))
  nuetral_count = count(filter(df, (sentiment_syuzhet_binary == 0) & (date == d)))
  total_count = positive_count + negative_count + nuetral_count

  rbind(syuzhet_daily_percent,data.frame(date=d, pos = (positive_count/total_count) , 
                                         perc_neg = (negative_count/total_count), perc = (nuetral_count/total_count)))->syuzhet_daily_percent
}
syuzhet_daily_percent$date <- as.Date(syuzhet_daily_percent$date, format="%Y-%m-%d")
names(syuzhet_daily_percent)[names(syuzhet_daily_percent) == 'n'] <- 'pos_perc'
names(syuzhet_daily_percent)[names(syuzhet_daily_percent) == 'n.1'] <- 'neg_perc'
names(syuzhet_daily_percent)[names(syuzhet_daily_percent) == 'n.2'] <- 'nuet_perc'

syuzhet_daily_percent$pos_perc_rolling =  rollmean(syuzhet_daily_percent$pos_perc, rolling_interval,
                                                      align="left", fill=0)
syuzhet_daily_percent$neg_perc_rolling =  rollmean(syuzhet_daily_percent$neg_perc, rolling_interval,
                                                   align="left", fill=0)
syuzhet_daily_percent$nuet_perc_rolling =  rollmean(syuzhet_daily_percent$nuet_perc, rolling_interval,
                                                   align="left", fill=0)

syuzhet_daily_senitmet_graph <- ggplot(head(syuzhet_daily_percent, -rolling_interval), aes(date)) + 
  geom_line(aes(y = pos_perc_rolling, colour = "Positive"))  +
  geom_line(aes(y = neg_perc_rolling, colour = "Negative")) +
  geom_line(aes(y = nuet_perc_rolling, colour = "Neutral")) +
  scale_x_date(breaks=date_breaks("3 months"),
               labels=date_format("%b - %y")) +
  ggtitle("Distirbution of Sentimet (Syuzhet) from 2015-2018") +
  xlab("Date") +
  ylab("Percentage of Headlines") +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =10))
syuzhet_daily_senitmet_graph

strftime(df$date, format = "%Y-%m") -> df$year_week
syuzhet_daily_category_percent <- data.frame()
unique(df[,c('year_week', 'category')]) -> unique_day_cat
for (i in 1:length(unique_day_cat$category)){
  i_date = unique_day_cat$year_week[i]
  i_category = unique_day_cat$category[i]
  positive_count = count(filter(df, (sentiment_syuzhet_binary == 1) & (year_week == i_date) & (category == i_category)))
  negative_count = count(filter(df, (sentiment_syuzhet_binary == -1) & (year_week == i_date) & (category == i_category)))
  nuetral_count = count(filter(df, (sentiment_syuzhet_binary == 0) & (year_week == i_date) & (category == i_category)))
  total_count = positive_count + negative_count + nuetral_count
  
  rbind(syuzhet_daily_category_percent,data.frame(year_week=i_date, category = i_category, pos = (positive_count/total_count) , 
                                                   perc_neg = (negative_count/total_count), perc = (nuetral_count/total_count)))->syuzhet_daily_category_percent
}
names(syuzhet_daily_category_percent)[names(syuzhet_daily_category_percent) == 'n'] <- 'pos_perc'
names(syuzhet_daily_category_percent)[names(syuzhet_daily_category_percent) == 'n.1'] <- 'neg_perc'
names(syuzhet_daily_category_percent)[names(syuzhet_daily_category_percent) == 'n.2'] <- 'nuet_perc'

syuzhet_daily_category_sentiment_graph <- ggplot(syuzhet_daily_category_percent, aes(year_week)) + 
  geom_line(aes(y = pos_perc, colour = "Positive"))  +
  geom_line(aes(y = neg_perc, colour = "Negative")) +
  geom_line(aes(y = nuet_perc, colour = "Neutral")) +
  ggtitle("Distirbution of Sentimet (Syuzhet) from 2015-2018") +
  xlab("Date") +
  ylab("Percentage of Headlines") +
  facet_wrap( ~ category)+
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =10))
syuzhet_daily_category_sentiment_graph




syuzhet_weekly_percent <- data.frame()
for (dw in unique(df$day_of_week)){
  print(dw)
  positive_count = count(filter(df, (sentiment_syuzhet_binary == 1) & (day_of_week == dw)))
  negative_count = count(filter(df, (sentiment_syuzhet_binary == -1) & (day_of_week == dw)))
  nuetral_count = count(filter(df, (sentiment_syuzhet_binary == 0) & (day_of_week == dw)))
  total_count = positive_count + negative_count + nuetral_count
  
  rbind(syuzhet_weekly_percent,data.frame(day_of_week=dw, pos = (positive_count/total_count) , 
                                         perc_neg = (negative_count/total_count), perc = (nuetral_count/total_count)))->syuzhet_weekly_percent
}
names(syuzhet_weekly_percent)[names(syuzhet_weekly_percent) == 'n'] <- 'pos_perc'
names(syuzhet_weekly_percent)[names(syuzhet_weekly_percent) == 'n.1'] <- 'neg_perc'
names(syuzhet_weekly_percent)[names(syuzhet_weekly_percent) == 'n.2'] <- 'nuet_perc'

syuzhet_weekly_senitmet_graph <- ggplot(syuzhet_weekly_percent, aes(day_of_week)) + 
  geom_line(aes(y = pos_perc, colour = "Positive"))  +
  geom_line(aes(y = neg_perc, colour = "Negative")) +
  geom_line(aes(y = nuet_perc, colour = "Neutral")) +
  ggtitle("Distirbution of Sentimet (Syuzhet) by Day of the Week") +
  xlab("Day of Week") +
  ylab("Percent of Headlines") +
  theme( axis.text.x = element_text(angle = 0, hjust = 1, size =10))
syuzhet_weekly_senitmet_graph

syuzhet_weekly_category_percent <- data.frame()
unique(df[,c('day_of_week', 'category')]) -> unique_dow_cat
for (i in 1:length(unique_dow_cat$category)){
  i_week_day = unique_dow_cat$day_of_week[i]
  i_category = unique_dow_cat$category[i]
  positive_count = count(filter(df, (sentiment_syuzhet_binary == 1) & (day_of_week == i_week_day) & (category == i_category)))
  negative_count = count(filter(df, (sentiment_syuzhet_binary == -1) & (day_of_week == i_week_day) & (category == i_category)))
  nuetral_count = count(filter(df, (sentiment_syuzhet_binary == 0) & (day_of_week == i_week_day) & (category == i_category)))
  total_count = positive_count + negative_count + nuetral_count
  
  rbind(syuzhet_weekly_category_percent,data.frame(day_of_week=i_week_day, category = i_category, pos = (positive_count/total_count) , 
                                                   perc_neg = (negative_count/total_count), perc = (nuetral_count/total_count)))->syuzhet_weekly_category_percent
}
names(syuzhet_weekly_category_percent)[names(syuzhet_weekly_category_percent) == 'n'] <- 'pos_perc'
names(syuzhet_weekly_category_percent)[names(syuzhet_weekly_category_percent) == 'n.1'] <- 'neg_perc'
names(syuzhet_weekly_category_percent)[names(syuzhet_weekly_category_percent) == 'n.2'] <- 'nuet_perc'
syuzhet_weekly_category_percent

syuzhet_weekly_category_senitmet_graph <- ggplot(syuzhet_weekly_category_percent, aes(day_of_week)) + 
  geom_line(aes(y = pos_perc, colour = "Positive"))  +
  geom_line(aes(y = neg_perc, colour = "Negative")) +
  geom_line(aes(y = nuet_perc, colour = "Neutral")) +
  ggtitle("Distirbution of Sentimet (Syuzhet) by Day of the Week") +
  xlab("Day of Week") +
  ylab("Percent of Headlines") +
  facet_wrap( ~ category)+
  theme( axis.text.x = element_text(angle = 0, hjust = 1, size =10))
syuzhet_weekly_category_senitmet_graph
print(paste("Number of Categories: ", length(unique(df$category))))



# Daily Percent of Bing Sentiment
# in general leans more to classifying something as nuetral, slight increase in negative over time
bing_daily_percent <- data.frame()
for (d in unique(df$date)){
  positive_count = count(filter(df, (sentiment_bing_binary == 1) & (date == d)))
  negative_count = count(filter(df, (sentiment_bing_binary == -1) & (date == d)))
  nuetral_count = count(filter(df, (sentiment_bing_binary == 0) & (date == d)))
  total_count = positive_count + negative_count + nuetral_count
  
  rbind(bing_daily_percent,data.frame(date=d, pos = (positive_count/total_count) , 
                                         perc_neg = (negative_count/total_count), perc = (nuetral_count/total_count)))->bing_daily_percent
}
bing_daily_percent$date <- as.Date(bing_daily_percent$date, format="%Y-%m-%d")
names(bing_daily_percent)[names(bing_daily_percent) == 'n'] <- 'pos_perc'
names(bing_daily_percent)[names(bing_daily_percent) == 'n.1'] <- 'neg_perc'
names(bing_daily_percent)[names(bing_daily_percent) == 'n.2'] <- 'nuet_perc'

bing_daily_percent$pos_perc_rolling =  rollmean(bing_daily_percent$pos_perc, rolling_interval,
                                                   align="left", fill=0)
bing_daily_percent$neg_perc_rolling =  rollmean(bing_daily_percent$neg_perc, rolling_interval,
                                                   align="left", fill=0)
bing_daily_percent$nuet_perc_rolling =  rollmean(bing_daily_percent$nuet_perc, rolling_interval,
                                                    align="left", fill=0)

p <- ggplot(head(bing_daily_percent, -rolling_interval), aes(date)) + 
  geom_line(aes(y = pos_perc_rolling, colour = "Positive"))  +
  geom_line(aes(y = neg_perc_rolling, colour = "Negative")) +
  geom_line(aes(y = nuet_perc_rolling, colour = "Neutral")) +
  scale_x_date(breaks=date_breaks("3 months"),
               labels=date_format("%b - %y")) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =10))
p


#affin
afinn_daily_percent <- data.frame()
for (d in unique(df$date)){
  positive_count = count(filter(df, (sentiment_afinn_binary == 1) & (date == d)))
  negative_count = count(filter(df, (sentiment_afinn_binary == -1) & (date == d)))
  nuetral_count = count(filter(df, (sentiment_afinn_binary == 0) & (date == d)))
  total_count = positive_count + negative_count + nuetral_count
  
  rbind(afinn_daily_percent,data.frame(date=d, pos = (positive_count/total_count) , 
                                         perc_neg = (negative_count/total_count), perc = (nuetral_count/total_count)))->afinn_daily_percent
}
afinn_daily_percent$date <- as.Date(afinn_daily_percent$date, format="%Y-%m-%d")
names(afinn_daily_percent)[names(afinn_daily_percent) == 'n'] <- 'pos_perc'
names(afinn_daily_percent)[names(afinn_daily_percent) == 'n.1'] <- 'neg_perc'
names(afinn_daily_percent)[names(afinn_daily_percent) == 'n.2'] <- 'nuet_perc'

afinn_daily_percent$pos_perc_rolling =  rollmean(afinn_daily_percent$pos_perc, rolling_interval,
                                                   align="left", fill=0)
afinn_daily_percent$neg_perc_rolling =  rollmean(afinn_daily_percent$neg_perc, rolling_interval,
                                                   align="left", fill=0)
afinn_daily_percent$nuet_perc_rolling =  rollmean(afinn_daily_percent$nuet_perc, rolling_interval,
                                                    align="left", fill=0)

p <- ggplot(head(afinn_daily_percent, -rolling_interval), aes(date)) + 
  geom_line(aes(y = pos_perc_rolling, colour = "Positive"))  +
  geom_line(aes(y = neg_perc_rolling, colour = "Negative")) +
  geom_line(aes(y = nuet_perc_rolling, colour = "Neutral")) +
  scale_x_date(breaks=date_breaks("3 months"),
               labels=date_format("%b - %y")) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =10))
p

#nrc overtime assesment
nrc_daily_percent <- data.frame()
for (d in unique(df$date)){
  positive_count = count(filter(df, (sentiment_nrc_binary == 1) & (date == d)))
  negative_count = count(filter(df, (sentiment_nrc_binary == -1) & (date == d)))
  nuetral_count = count(filter(df, (sentiment_nrc_binary == 0) & (date == d)))
  total_count = positive_count + negative_count + nuetral_count
  
  rbind(nrc_daily_percent,data.frame(date=d, pos = (positive_count/total_count) , 
                                         perc_neg = (negative_count/total_count), perc = (nuetral_count/total_count)))->nrc_daily_percent
}
nrc_daily_percent$date <- as.Date(nrc_daily_percent$date, format="%Y-%m-%d")
names(nrc_daily_percent)[names(nrc_daily_percent) == 'n'] <- 'pos_perc'
names(nrc_daily_percent)[names(nrc_daily_percent) == 'n.1'] <- 'neg_perc'
names(nrc_daily_percent)[names(nrc_daily_percent) == 'n.2'] <- 'nuet_perc'

nrc_daily_percent$pos_perc_rolling =  rollmean(nrc_daily_percent$pos_perc, rolling_interval,
                                                   align="left", fill=0)
nrc_daily_percent$neg_perc_rolling =  rollmean(nrc_daily_percent$neg_perc, rolling_interval,
                                                   align="left", fill=0)
nrc_daily_percent$nuet_perc_rolling =  rollmean(nrc_daily_percent$nuet_perc, rolling_interval,
                                                    align="left", fill=0)

p <- ggplot(head(nrc_daily_percent, -rolling_interval), aes(date)) + 
  geom_line(aes(y = pos_perc_rolling, colour = "Positive"))  +
  geom_line(aes(y = neg_perc_rolling, colour = "Negative")) +
  geom_line(aes(y = nuet_perc_rolling, colour = "Neutral")) +
  scale_x_date(breaks=date_breaks("3 months"),
               labels=date_format("%b - %y")) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =10))
p

