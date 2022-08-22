####################################################################
# Replication for all Tables in MS
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################

rm(list = ls())

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
  pathGraphics = paste0(pathDrop, 'graphics')
}

# libraries
library(tidyverse)
library(lubridate)
library(haven)
library(kableExtra)
library(ggpubr)
####################################################################

####################################################################
# data (clean)
ccc <- readr::read_csv(paste0(pathData,"ccc_clean_counter_indicator.csv"),
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

# acled
acled <- read_csv(paste0(pathData,"acled_clean.csv"))

# nspe
nspe <- read_dta(paste0(pathData,"nspe_project.dta"))

## FILTER BOTH DATASETS TO MOST RECENTLY COMPLETED MONTH
acled <- acled %>% filter(event_date < ymd("2021-08-01"))
ccc_counter <- ccc %>% filter(date < ymd("2021-08-01"))
####################################################################

####################################################################
# Table 1: Ten most frequently referenced issues, CCC 
ccc_issues <- ccc_counter %>%
  mutate(issues_clean = str_split(issues, pattern = ";")) %>% 
  select(date, resolved_locality, resolved_county, resolved_state,
         issues, issues_clean) %>% 
  unnest_longer(issues_clean) %>% 
  mutate(issues_clean = str_trim(issues_clean, side = "both"))

ccc_issues %>% distinct(issues_clean) %>% nrow()

# BREAK: numbers do not match MS
# top 10 issues
ccc_top_issues <- ccc_issues %>% 
  filter(!is.na(issues_clean)) %>% 
  group_by(issues_clean) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  head(10)

# top 10 using kableExtra
ccc_top_issues_kb <- ccc_top_issues %>% 
  rename(`Issue Area` = issues_clean, `Event Count` = n) %>% 
  kable(., caption = "Table 1: Top 10 CCC Event Issues") %>% 
  kable_styling() 
####################################################################

####################################################################
# Table 2: Ten most frequently referenced issue areas, NSPE
nspe_top_issues <- nspe %>% 
  select(cabort:cworker, creligcomb) %>%
  summarize(across(.cols = everything(), .fns = ~sum(.))) %>% 
  pivot_longer(., cols = everything(), names_to = "issue", values_to = "num_events") %>%
  arrange(-num_events) %>%
  head(10)

nspe_top_issues_kb <- nspe_top_issues %>% 
  rename(`Issue Area` = issue, `Event Count` = num_events) %>% 
  kable(., caption = "Table 2: Ten most frequently referenced issue areas in the NSPE data.") %>% 
  kable_styling() 
####################################################################

####################################################################
# Table 3: Identified Counter-Protests Events by Source
# ACLED
## add counter-protest variable
acled <- acled %>%
  mutate(counter_event = ifelse(grepl("\\[counter", notes), 1, 0))

## acled counter-protest distribution by year
## BREAK does not match MS
acled %>% filter(counter_event == 1) %>% 
  group_by(year(event_date)) %>% 
  summarize(n = n()) %>% 
  mutate(prop_yr = round(n / sum(n),2))

# CCC
ccc %>% filter(counter_event == 1) %>% 
  group_by(year(date)) %>% 
  summarize(n = n()) %>% 
  mutate(prop_yr = round(n / sum(n), 2))
####################################################################

####################################################################
# Table 4: CCC-Reported Risks to Protesters at Counter-Protest Events

# Table 5: CCC-Reported Risks to Protesters at Counter-Protests

# Table 6: ACLED-Reported Risks to Protesters at Counter-Protest Events