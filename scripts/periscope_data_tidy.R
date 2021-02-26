# tidy and process Periscope analytics csv 

# set up
library(here)
library(tidyverse)

# read raw data from UAZCC COE account
periscope_UAZCCCOE <- read_csv("data/raw/periscope-analytics_UAZCC_COE.csv")

# read raw data from rene herrera account
periscope_rh <- read_csv("data/raw/periscope-analytics_rh.csv")

periscope <- bind_rows(periscope_UAZCCCOE, periscope_rh)

glimpse(periscope)

# tidy 
periscope <- periscope %>%
  select(Title, Date, `Live Viewers`, `Replay Viewers`) %>%
  arrange(Date)

head(periscope)

# write to csv
write_csv(periscope, "data/tidy/periscope_analytics.csv")
