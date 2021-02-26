# periscope 

glimpse(coe_tweets) 

coe_tweets %>%
  distinct(source)

periscope_posts <- coe_tweets %>%
  filter(source == "Periscope") %>%
  select(created_at, text, status_url) %>%
  arrange(created_at) 

glimpse(periscope_posts)

rene_tweets <- get_timeline("reneherrera", n = 1000)

periscope_by_rene <- rene_tweets %>%
  filter(source == "Periscope") %>%
  select(created_at, text, status_url) %>%
  arrange(created_at) %>%
  slice(1:26)

periscope_posts <- bind_rows(periscope_by_rene, periscope_posts)  

periscope_posts %>%
  arrange(created_at) %>%
  write_csv("data/tidy/periscope_posts.csv")

periscope_posts

# LTBC 

ltbc <- get_timeline("UAZCancer_COE", n = 10000)

glimpse(ltbc)

ltbc_table <- ltbc %>%
  select(status_id,
         created_at, 
         text,
         favorite_count,
         retweet_count,
         quote_count,
         reply_count,
         hashtags,
         place_full_name) %>%
  filter(str_detect(text, "webinar") |
           str_detect(text, "taco")) 

ltbc_table

ltbc_table$hashtags <- as.character(ltbc_table$hashtags)

write_csv(ltbc_table, "data/tidy/LTBC_on_twitter.csv")
