# data processing for twitter
# set up ----

library(here)
library(tidyverse)
library(rtweet)
get_token()

# load data into environment ----

coe_tweets <- read_rds("data/raw/coe_tweets.rds") #tweets by UAZCC COE
coe_faves <- read_rds("data/raw/coe_faves.rds") #tweets favorited by UAZCC COE
cancer_twts <- read_rds("data/raw/cancer_twts.rds") #tweets with cancerfreeaz tag

# query newest twitter data----

twts <- search_tweets(
  "#CancerFreeAZ", include_rts = FALSE
) # tweets with cancerfreeaz hashtag from previous 6-9 days

tmls <- get_timelines("UAZCancer_COE") # most recent tweets from uazcancer_coe

fvts <- get_favorites("UAZCancer_COE") # get tweets data for statuses favorited by one ore more target users; 
# tweets with most favorites from uazcancer_coe

# return list of user screen names uazcc_coe follows
friends <- get_friends("uazcancer_coe")
friends_data <- lookup_users(friends$user_id)
friends_data <- friends_data %>%
  arrange(screen_name)

# return list of user screen names following uazcc_coe
followers <- get_followers("uazcancer_coe")
followers_data <- lookup_users(followers$user_id)
followers_data <- followers_data %>%
  arrange(screen_name)

# follower:following
friends_count <- count(friends)
followers_count <- count(followers)

impact <- followers_count/friends_count


# append updated tweets ----

# for most recent tweets with cancerfreeaz hashtag
binded_hash <- bind_rows(cancer_twts, twts)
cancer_twts <- distinct(binded_hash, status_id, .keep_all = TRUE)
cancer_twts <- cancer_twts %>% 
  arrange(desc(created_at))

# for most recent tweets from uazcancer_coe
binded_tweets <- bind_rows(coe_tweets, tmls)
coe_tweets <- distinct(binded_tweets, status_id, .keep_all = TRUE)
coe_tweets <- coe_tweets %>%
  arrange(desc(created_at))

# for tweets with most favorites from uazcancer_coe
binded_faves <- bind_rows(coe_faves, fvts)
coe_faves <- distinct(binded_faves, status_id, .keep_all = TRUE)
coe_faves <- coe_faves %>%
  arrange(desc(created_at))

# update saved raw data ----

write_rds(coe_tweets, "data/raw/coe_tweets.rds")
write_rds(coe_faves, "data/raw/coe_faves.rds")
write_rds(cancer_twts, "data/raw/cancer_twts.rds")

# visualizations

coe_tweets %>%
  ggplot(mapping = aes(x = status_id, y = favorite_count)) +
  geom_bar(stat = "identity") +
  theme_classic()

coe_tweets %>%
  ggplot(mapping = aes(x = status_id, y = retweet_count)) +
  geom_bar(stat = "identity") +
  theme_classic()


### testing below


# data processing for twitter
# set up ----

library(here)
library(tidyverse)
library(rtweet)
get_token()

# load data into environment ----

coe_tweets <- read_rds("data/raw/coe_tweets.rds") #tweets by UAZCC COE
coe_faves <- read_rds("data/raw/coe_faves.rds") #tweets favorited by UAZCC COE
cancer_twts <- read_rds("data/raw/cancer_twts.rds") #tweets with cancerfreeaz tag

glimpse(cancer_twts)

cancer_twts %>%
  distinct(screen_name)

cancer_twts %>%
  summarise(min(created_at),
            max(created_at))

cancer_twts %>%
  arrange(desc(favorite_count))

cancer_twts %>%
  filter(favorite_count >= 1)

cancer_twts %>%
  arrange(desc(retweet_count))

cancer_twts %>%
  filter(retweet_count >= 1)

cancer_twts %>%
  distinct(place_name) %>%
  drop_na()

cancer_twts %>%
  filter(media_url != "NA") %>%
  count()

cancer_twts %>%
  filter(media_type == "photo") 

cancer_twts %>%
  select(text) %>%
  sample_n(3)

coe_hashtags <- cancer_twts %>%
  filter(hashtags != "NA") %>%
  select(hashtags) %>%
  as.character(cancer_twts$hashtags)

coe_hashtags

acs_tweets <- get_timelines("AmericanCancer") # most recent tweets from ACS

acs_hashtags <- acs_tweets %>%
  filter(hashtags != "NA") %>%
  select(hashtags) %>%
  as.character(acs_tweets$hashtags)

str_extract(acs_hashtags, "\\d")
acs_hashtags
