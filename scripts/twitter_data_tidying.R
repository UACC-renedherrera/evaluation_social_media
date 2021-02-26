# data tidying for twitter data mining
# set up ----
library(here)
library(rtweet)
#get_token()
library(tidyverse)

# run ----
# the following block to query data
#

twts <- search_tweets(
  "#CancerFreeAZ", n = 1000, include_rts = FALSE
) # most recent tweets with cancerfreeaz hashtag

tmls <- get_timelines("UAZCancer_COE", n = 1000) # most recent tweets from uazcancer_coe

fvts <- get_favorites("UAZCancer_COE", n = 1000) # tweets with most favorites from uazcancer_coe



# build the durable tibbles ----

coe_tweets <- tmls
coe_faves <- fvts
cancer_twts <- twts

# write to data_raw folder ----

write_rds(coe_tweets, "data_raw/coe_tweets.rds")
write_rds(coe_faves, "data_raw/coe_faves.rds")
write_rds(cancer_twts, "data_raw/cancer_twts.rds")
