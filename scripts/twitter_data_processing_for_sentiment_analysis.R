# set up ----
library(here)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tm)
library(RColorBrewer)
library(stopwords)
library(infer)
library(lubridate)

# read data ----
hashtags <- read_rds("data/raw/twitter_hashtags.rds")
# cancerfreeaz <- read_rds("data/raw/cancer_twts.rds")
# coe_faves <- read_rds("data/raw/coe_faves.rds")
coe_tweets <- read_rds("data/raw/coe_tweets.rds")
# cancer_usrs <- read_rds("data/raw/cancer_usrs.rds")

# tweets by the cancer center ----
# inspect
glimpse(coe_tweets)

# distinct tweets not retweeted
coe_tweets <- coe_tweets %>%
  filter(is_retweet == FALSE) 

# save to disk 
write_rds(coe_tweets, "data/tidy/coe_tweets.rds")

# how many tweets by COE
coe_tweets_n <- coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  count(status_id) %>%
  summarise(sum(n)) %>%
  as.character()

coe_tweets_n

# view most favorited tweet
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  arrange(desc(favorite_count)) %>%
  slice(1) %>%
  select(favorite_count, created_at, text, is_retweet) %>%
  view()

# histogram of favorited tweets 
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  arrange(desc(favorite_count)) %>%
  select(favorite_count, created_at, ) %>%
  ggplot(mapping = aes(x = favorite_count)) +
  geom_histogram(bins = 10, color = "#81D3EB", fill = "#0C234B") +
  labs(title = "Distribution of Favorited Tweets",
       subtitle = "A histogram",
       x = "Favorites",
       y = "",
       caption = "Source: UAZCC COE Twitter")

# view most retweeted tweet
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  arrange(desc(retweet_count)) %>%
  slice(1) %>%
  select(retweet_count, created_at, text, is_retweet) %>%
  view()

# date range of COE tweets
coe_tweets %>%
  summarise(min(created_at), max(created_at))

# save date range min and max to environment for use in visualization caption 
coe_tweets_min_max <- coe_tweets %>%
  summarise(min(created_at), max(created_at))

# min  
coe_tweets_min <- coe_tweets_min_max %>%
  select(starts_with("min"))

coe_tweets_min <- as_date(coe_tweets_min$`min(created_at)`)

# max
coe_tweets_max <- coe_tweets_min_max %>%
  select(starts_with("max"))

coe_tweets_max <- as_date(coe_tweets_max$`max(created_at)`)

# text of tweets
coe_tweet_text <- coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  distinct(status_id, .keep_all = TRUE) %>%
  select(text)

# tweet text
coe_tweet_text <- coe_tweet_text %>%
  mutate(line = row_number())

# convert tweets to one token per row format
# remove stopwords
coe_tidy_tweet_text <- coe_tweet_text %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) %>%
  filter(word != "cancer")

# most common words
coe_tidy_tweet_text %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sort = TRUE)

# display overall sentiment of COE Tweets
coe_sentiment <- coe_tidy_tweet_text %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(line, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  summarise(overall_sentiment = sum(sentiment))

# 5 most used negative and positive words by COE
coe_tidy_tweet_text %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:5)

# sample tweet text from coe
coe_tweets %>%
  select(created_at, text) %>%
  sample_n(3) %>%
  view()

# visualize 10 most positive and negative words
# caption 
fig_caption <- str_c(sep = " ", "Twitter data from", coe_tweets_min, "to", coe_tweets_max)

# plot
coe_tidy_tweet_text %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Most Frequently Used Words in COE Tweets",
       subtitle = "According to sentiment",
       x = "Word",
       y = "Frequency",
       caption = fig_caption) +
  theme_bw() +
  scale_fill_brewer(palette = "Paired")

# tweets with cancer related hashtag ----
# inspect
glimpse(hashtags)
str(hashtags)

# number of tweets 
hashtags_n <- hashtags %>%
  count(status_id) %>%
  summarise(sum(n)) %>%
  as.character()

# date range of cancer tweets 
hashtags %>%
  summarise(min(created_at), max(created_at))

# save date range min and max to environment for use in visualization caption 
hashtags_min_max <- hashtags %>%
  summarise(min(created_at), max(created_at))

# min  
hashtags_min <- hashtags_min_max %>%
  select(starts_with("min"))

hashtags_min <- as_date(hashtags_min$`min(created_at)`)

# max
hashtags_max <- hashtags_min_max %>%
  select(starts_with("max"))

hashtags_max <- as_date(hashtags_max$`max(created_at)`)

# generate 100 samples
hashtag_samples <- hashtags %>%
  filter(screen_name != "UAZCancer_COE") %>%
  rep_sample_n(size = as.numeric(coe_tweets_n), reps = 500, replace = TRUE)

# select only text
hashtag_samples_tweet_text <- hashtag_samples %>%
  select(replicate, text) %>%
  mutate(line = row_number())

# convert tweets to one token per row format
# remove stopwords
hashtag_samples_tweet_text <- hashtag_samples_tweet_text %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) %>%
  filter(word != "cancer") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(replicate)

# most common words
hashtag_samples_tweet_text %>%
  count(word, sort = TRUE)

# display overall sentiment of each of the samples
hashtag_samples_sentiment <- hashtag_samples_tweet_text %>%
  count(line, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  summarise(overall_sentiment = sum(sentiment))

# mean & median sentiment
hashtag_samples_sentiment %>%
  summarise(mean(overall_sentiment), 
            median(overall_sentiment),
            mode(overall_sentiment),
            sd = sd(overall_sentiment),
            min = min(overall_sentiment),
            max = max(overall_sentiment))

# show histogram of mean sentiment
hashtag_samples_sentiment %>%
  ggplot(mapping = aes(x = overall_sentiment)) +
  geom_histogram(color = "blue", fill = "lightblue") +
  theme_bw() +
  labs(title = "Distribution of Overall Sentiment Mean",
       subtitle = "From 500 samples",
       x = "Sampled mean overall sentiment")
