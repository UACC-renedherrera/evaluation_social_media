# set up ----
library(here)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tm)
library(RColorBrewer)
library(stopwords)

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
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  count(status_id) %>%
  summarise(sum(n))

# favorite count 
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  arrange(desc(favorite_count)) %>%
  slice(1) %>%
  select(favorite_count, created_at, text, is_retweet) %>%
  view()

# retweet count
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  arrange(desc(retweet_count)) %>%
  slice(1) %>%
  select(retweet_count, created_at, text, is_retweet) %>%
  view()

# quote count 
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  arrange(desc(quote_count)) %>%
  slice(1) %>%
  select(quote_count, created_at, text, is_retweet) %>%
  view()

# reply count
coe_tweets %>%
  filter(is_retweet == FALSE) %>%
  arrange(desc(reply_count)) %>%
  slice(1) %>%
  select(reply_count, created_at, text, is_retweet) %>%
  view()

# date range 
coe_tweets %>%
  summarise(min(created_at), max(created_at))

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

# display overall sentiment of the sample
coe_tidy_tweet_text %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(line, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  summarise(overall_sentiment = sum(sentiment))

# 5 most negative and postive words in the sample
coe_tidy_tweet_text %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:5)

# sample tweet text from coe
coe_tweet_text %>%
  sample_n(3) %>%
  view()

# visualize 10 most postive and negative words
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
       caption = "Twitter data from 28 Sep 2018 to 10 Feb 2021") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired")


# tweets with cancer related hashtag ----
# inspect
glimpse(hashtags)

# work with a sample of 1000
hashtags_n <- hashtags %>%
  filter(screen_name != "UAZCancer_COE") %>%
  sample_n(1000, replace = TRUE)

# show who is tweeting about "cancer"
users <- hashtags_n %>%
  distinct(screen_name) %>%
  arrange(screen_name)

# location of users
# hashtags_n %>%
#   count(location, sort = TRUE) %>%
#   mutate(location = reorder(location, n)) %>%
#   top_n(20) %>%
#   ggplot(aes(x = location)) +
#   geom_bar() +
#   coord_flip() +
#   labs(x = "Count",
#        y = "Location",
#        title = "Twitter users - unique locations")

# text of tweets
tweet_text <- hashtags_n %>%
  select(text)

# inspect
glimpse(tweet_text)

# tweet text
tweet_text <- tweet_text %>%
  mutate(line = row_number())

# convert tweets to one token per row format
# remove stopwords
tidy_tweet_text <- tweet_text %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) %>%
  filter(word != "cancer")

# most common words
tidy_tweet_text %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sort = TRUE)

# display overall sentiment of the sample
tidy_tweet_text %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(line, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  summarise(overall_sentiment = sum(sentiment))

# 5 most negative and postive words in the sample
tidy_tweet_text %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:5)

# visualize 20 most postive and negative words
tidy_tweet_text %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(mapping = aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Most frequently used words in tweets about cancer",
       x = "Word",
       y = "Frequency") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired")
  
