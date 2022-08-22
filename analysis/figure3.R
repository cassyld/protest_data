####################################################################
# Replication Figure 3
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################

# libraries
library(tidyverse)
library(lubridate)
library(haven)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(patchwork)
library(viridis)

# cd user paths
if(Sys.info()['user'] %in% c('dorffc')){
  pathGit = '~/ProjectsGit/protest_data/'
  pathDrop = '~/Dropbox/Research/protest_data/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathDrop, 'graphics/')
}

# ak user paths
if(Sys.info()['user'] %in% c('Amanda')){
  pathGit = '~/Documents/Vanderbilt/c4_research_lab/c4_protestData/'
  pathDrop = '~/Dropbox/c4_protestData/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathDrop, 'graphics/acled_protest_only')
}

# data (clean)
# ccc
ccc <- readr::read_csv(paste0(pathData,"ccc_clean.csv"),
                       col_types = cols(
                         .default = "c",
                         county = "c",
                         organizations = "c",
                         date = col_date(format = ""),
                         online = col_double(),
                         valence = col_double(),
                         size_low = col_double(),
                         size_high = col_double(),
                         size_mean = col_double(),
                         size_cat = col_double(),
                         arrests = col_double(),
                         arrests_any = col_double(),
                         injuries_crowd = col_double(),
                         injuries_crowd_any = col_double(),
                         injuries_police = col_double(),
                         injuries_police_any = col_double(),
                         property_damage = col_double(),
                         property_damage_any = col_double(),
                         chemical_agents = col_logical(),
                         participant_deaths = 'd',
                         police_deaths = 'd',
                         participant_measures = "c",
                         notes = "c",
                         participants = "c",
                         police_measures = "c",
                         title = "c",
                         final = col_double(),
                         lat = col_double(),
                         lon = col_double()
                       )
)

# acled events only in top overlapping cities
acled <- read_csv(paste0(pathData,"acled_clean.csv")) 
acled_top <- acled %>% 
  filter(event_date >= ymd("2020-01-01"), event_date < ymd("2021-08-01")) %>% 
  mutate(location = ifelse(grepl("New York -", location),"New York City", location)) %>%
  filter((location == "New York City" & state == "New York") |
           (location == "Seattle" & state == "Washington") |
           (location == "San Francisco" & state == "California") |
           #(location == "Philadelphia" & state == "Pennsylvania") |
           (location == "Los Angeles" & state == "California") |
           (location == "Washington DC" & state == "District of Columbia"))

# ccc events only in top overlapping cities 
ccc_top <- ccc %>% 
  filter(date >= ymd("2020-01-01"), date < ymd("2021-08-01")) %>% 
  filter((resolved_locality == "New York" & resolved_state == "NY") |
           (resolved_locality == "Los Angeles" & resolved_state == "CA") |
           (resolved_locality == "Washington" & resolved_state == "DC") |
           (resolved_locality == "Seattle" & resolved_state == "WA") |
           #(resolved_locality == "Philadelphia" & resolved_state == "PA") |
           (resolved_locality == "San Francisco" & resolved_state == "CA"))

## get social media only events for CCC
# add ID and add new var for when twitter/instagram is only source
ccc_top_id <- ccc_top %>% mutate(event_id = 1:n()) %>% select(event_id, everything())
ccc_sm_ids <- ccc_top_id %>% 
  filter(across(c(source_1:source_26), ~
                  grepl("twitter|instagram", .x, ignore.case = T)|is.na(.x)))

# ccc social media only events
ccc_sm_only <- ccc_top_id %>% 
  filter(event_id %in% ccc_sm_ids$event_id) %>% 
  mutate(source_soc_media = 1) %>%
  group_by(resolved_locality) %>% 
  summarize(source_soc_media = n())

# ccc news only events 
ccc_news_only <- ccc_top_id %>% filter(!(event_id %in% ccc_sm_ids$event_id)) %>% 
  mutate(source_soc_media = 0) %>%
  group_by(resolved_locality) %>% 
  summarize(source_news = n())

# ccc merge
ccc_grp <- ccc_sm_only %>% left_join(ccc_news_only, by = "resolved_locality") %>% 
  mutate(dataset = "CCC") %>%
  mutate(resolved_locality = case_when(resolved_locality == "New York" ~ "New York City",
                                       resolved_locality == "Washington" ~ "Washington DC",
                                       T ~ resolved_locality)) %>% 
  select(location = resolved_locality, dataset, source_news, source_soc_media)

# acled variable for when source is only social media
acled_grp <- acled_top %>% 
  mutate(source_soc_media = ifelse(source == "Twitter" | source == "Instagram", 1, 0)) %>%
  group_by(location) %>% 
  summarize(source_news = sum(source_soc_media == 0),
            source_soc_media = sum(source_soc_media == 1)) %>% 
  mutate(dataset = "ACLED") %>%
  select(location = location, dataset, everything())

# ccc/acled comp df
both_grp <- ccc_grp %>% rbind(acled_grp) %>% 
  pivot_longer(., cols=c("source_news", "source_soc_media"),
               names_to = c("source"), values_to = c("ct")) %>% 
  mutate(source = ifelse(source == "source_news", "News", "Social Media"),
         source = factor(source, levels = c("Social Media", "News")))

# plot use of social media comparison
figure3 <- both_grp %>% 
  mutate(location = ifelse(location == "Washington DC", "Washington, D.C.", location)) %>% 
  ggplot(., aes(x = dataset, y = ct, fill = source)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~ location) +
  labs(x = "Dataset", y = "Event Count", fill = "Source") +
  theme_bw() +
  theme(panel.border = element_blank(),
        strip.background = element_rect(fill="#ebebeb", color="#ebebeb"),
        strip.text = element_text(face = "bold"))

figure3