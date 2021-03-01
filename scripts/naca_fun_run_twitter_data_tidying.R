# data processing for twitter to
# collect tweets from the previous few days for specific hashtags

# set up ----
# load packages

library(here)
library(tidyverse)
library(rtweet)

# define hashtags of interest

hashtags <- c("#CancerFreeAZ", #uazcc
              "#gyrigaz" # get your rear in gear az
) %>%
  set_names() # Name the vector so you have meaningful information in the new col

#
# Create df by mapping search_tweets over the named vector
outdf <- purrr::map_dfr(hashtags, search_tweets, include_rts = FALSE, n = 500, .id = "searchtag")

# load existing dataset
naca_fun_run_twitter_hashtags <- read_rds("data/raw/naca_fun_run_twitter_hashtags.rds")

# combine outdf and twitter_hashtags
naca_fun_run_twitter_hashtags <- bind_rows(naca_fun_run_twitter_hashtags, outdf)

# remove duplicates
naca_fun_run_twitter_hashtags <- distinct(naca_fun_run_twitter_hashtags, status_id, .keep_all = TRUE)

# save to data/raw
write_rds(naca_fun_run_twitter_hashtags, "data/raw/naca_fun_run_twitter_hashtags.rds")
