# data processing for twitter to
# collect tweets from the previous few days with cancer related hashtags

# set up ----
# load packages

library(here)
library(tidyverse)
library(rtweet)

# define hashtags of interest

hashtags <- c("#CancerFreeAZ", #uazcc
              "#adcsm", # adrenal cancer
              "#amsm", #advanced metastatic cancer
              "#ancsm", #anal cancer
              "#ayacsm", #adolescent and young adult cancer
              "#bcsm", #breast cancer
              "#blcsm", #bladder cancer
              "#btsm", #brain tumor
              "#crcsm", #colorectal cancer
              "#esocsm", #esophageal cancer
              "#gencsm", #genetic cancer
              "#gyncsm", #gynecologic cancer
              "#hncsm", #head and neck cancer
              "#hpbcsm", #hepatobiliary cancer
              "#hepatobiliary",
              "#kcsm", #kidney cancer
              "#lcsm", #lung cancer
              "#leusm", #leukemia
              "#lymsm", #lymphoma
              "#melsm", #melanoma
              "#mmsm", #multiple myeloma
              "#mpnsm", #Myeloproliferative Neoplasms
              "#netsm", #Neuroendocrine Tumors
              "#pancsm", #Pancreatic Cancer
              "#pcsm", #Prostate Cancer
              "#pedcsm", #Pediatric Cancer
              "#scmsm", #Sarcoma
              "#skcsm", #Skin Cancer
              "#stcsm", #Stomach Cancer
              "#thmcsm", #Thymoma and Thymic Carcinoma
              "#thycsm", #Thyroid Cancer
              "#tscsm", #Testicular Cancer
              "#attackingcancer", #acs
              "#lungcancer", #acs
              "#pancreaticcancer", #acs
              "#caregiver", #acs
              "#cancer", #acs
              "#breastcancer", #acs
              "#breastcancersurvivor", #acs
              "#support", # here and below found with "cancer" search on Twitter
              "#fuckcancer",
              "#cancerjourney",
              "#prostatecancer",
              "#endprostatecancer",
              "#livingwithmelanoma",
              "#melanoma",
              "#pediatriccancer",
              "#endcancer",
              "#screwcancer",
              "#childhoodcancer",
              "#breastcancerawareness",
              "#pancan",
              "#pancchat",
              "#kidneycancer",
              "#kidneycancerawareness",
              "#livercancer",
              "#cancerawareness",
              "#btcsm", #billiary tract cancer
              "#beyondcancer"
) %>%
  set_names() # Name the vector so you have meaningful information in the new col

write_rds(hashtags, "data/tidy/hashtags_list_of.rds")

#
# Create df by mapping search_tweets over the named vector
outdf <- purrr::map_dfr(hashtags, search_tweets, include_rts = FALSE, n = 500, .id = "searchtag")

# load existing dataset
twitter_hashtags <- read_rds("data/raw/twitter_hashtags.rds")

# combine outdf and twitter_hashtags
twitter_hashtags <- bind_rows(twitter_hashtags, outdf)

# remove duplicates
twitter_hashtags <- distinct(twitter_hashtags, status_id, .keep_all = TRUE)

# save to data/raw
write_rds(twitter_hashtags, "data/raw/twitter_hashtags.rds")

# load data into environment ----

cancer_twts <- read_rds("data/raw/cancer_twts.rds") #tweets with cancerfreeaz tag
