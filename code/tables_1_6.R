####################################################################
# Replication for all Tables in Main MS
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
  pathGit = '~/Documents/Vanderbilt/protest_data/'
  pathDrop = '~/Dropbox/c4_protestData/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathDrop, 'graphics/')
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
acled_pepper_irr <- read_csv(paste0(pathData, "acled_chem_agents/pepper_irritants_acled_notes.csv")) %>%
  filter(keep == 1) %>% 
  select(-target)

acled_teargas <- read_csv(paste0(pathData, "acled_chem_agents/teargas_acled_notes.csv")) %>%
  filter(keep == 1) %>% 
  select(data_id, keep, context = tear_gas_context, notes)

# nspe
nspe <- read_dta(paste0(pathData,"nspe_project.dta"))

# filter both data to most recently completed month 
acled <- acled %>% filter(event_date < ymd("2021-08-01"))
ccc <- ccc %>% filter(date < ymd("2021-08-01"))
####################################################################

####################################################################
# Table 1: Ten most frequently referenced issues, CCC 
ccc_issues <- ccc %>%
  mutate(issues_clean = str_split(issues, pattern = ";")) %>% 
  select(date, resolved_locality, resolved_county, resolved_state,
         issues, issues_clean) %>% 
  unnest_longer(issues_clean) %>% 
  mutate(issues_clean = str_trim(issues_clean, side = "both"))

ccc_issues %>% distinct(issues_clean) %>% nrow()

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
# ACLED issue areas

## identify events with racial justice as an issue area
acled %>% 
  filter(grepl("NAACP|BLM|BVM|African American Group (United States)", assoc_actor_1) | 
           grepl("NAACP|BLM|BVM|African American Group (United States)", assoc_actor_2)) %>%
  nrow()

####################################################################

####################################################################
# NSPE issue areas

## med and mean number of issues reported per event
nspe %>% select(cabort:cworker, creligcomb) %>% 
  mutate(num_issues = rowSums(.)) %>% 
  pull(num_issues) %>% 
  summary()

## number of events with more than ten issues
nspe %>% select(cabort:cworker, creligcomb) %>% 
  mutate(num_issues = rowSums(.)) %>% 
  filter(num_issues >= 10) %>% 
  nrow()

## events w/30 issues
nspe %>% select(cabort:cworker, creligcomb) %>% 
  mutate(num_issues = rowSums(.)) %>% 
  filter(num_issues == 30) %>% 
  nrow()
####################################################################

####################################################################
# Table 3: Identified Counter-Protests Events by Source
# ACLED
## add counter-protest variable
acled <- acled %>%
  mutate(counter_event = ifelse(grepl("\\[counter", notes), 1, 0))

## acled counter-protest distribution by year
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
ccc %>% 
  filter(!(counter_event == 0 & matching_counter == 1)) %>% 
  group_by(counter_event) %>% 
  summarize(n = n(),
           across(.cols = c(arrests_any, injuries_crowd_any,
                           chemical_agents),
                  .fns = ~ sum(.x, na.rm=T))) %>% 
  mutate(n = ifelse(counter_event == 0, n - 1632, n),
          arrests_any = ifelse(counter_event == 0, arrests_any - 204, arrests_any),
          injuries_crowd_any = ifelse(counter_event == 0, injuries_crowd_any - 99, injuries_crowd_any),
          chemical_agents = ifelse(counter_event == 0, chemical_agents - 14, chemical_agents)) %>% 
  mutate(across(.cols = c(arrests_any, injuries_crowd_any,
                           chemical_agents),
                 .fns = ~.x / n * 100)) %>% 
  mutate(across(where(is.numeric), round, 1)) %>% 
  rename(`Event Type` = counter_event, `Event Count` = n, `Arrests` = arrests_any,
          `Crowd Injuries` = injuries_crowd_any, `Chemical Agents` = chemical_agents)  
  
####################################################################
# Table 5: CCC-Reported Risks to Protesters by Counter-Protests type  
ccc %>% 
  filter(counter_event == 1, valence != 0) %>% 
  group_by(valence) %>% 
  summarize(n = n(),
            across(.cols = c(arrests_any, injuries_crowd_any,
                              chemical_agents),
                    .fns = ~ sum(.x, na.rm=T))) %>% 
  mutate(across(.cols = c(arrests_any, injuries_crowd_any,
                          chemical_agents),
                .fns = ~.x / n * 100)) %>% 
  mutate(across(where(is.numeric), round, 1)) %>% 
  rename(Valence = valence, `Event Count` = n, `Arrests` = arrests_any,
          `Crowd Injuries` = injuries_crowd_any, `Chemical Agents` = chemical_agents)  
####################################################################
  
####################################################################  
# Table 6: ACLED-Reported Risks to Protesters at Counter-Protest Events
acled <- acled %>% 
  mutate(arrests_any = case_when(
    is.na(arrests) ~ 0,
    !is.na(arrests) & 
      grepl("no arrests|nor arrests|no injuries or arrests|did not make any arrests|no one was arrested", arrests, ignore.case = T) ~ 0,
    T ~ 1))
  
# rbind the two dfs, keep only distinct ids & val of keep
acled_chem_ids <- rbind(acled_pepper_irr, acled_teargas) %>% 
  distinct(data_id, keep)

# check no duplicate ids (would mean i assigned diff 'keep' vals to same data_id)
# should be 0 rows
acled_chem_ids %>% group_by(data_id) %>% filter(n()>1)

# add in var
acled <- acled %>%
  mutate(
    chemical_agents = ifelse(data_id %in% acled_chem_ids$data_id, 1, 0),
    # these two event types are not protest events
    chemical_agents = ifelse(
      event_type %in% c("Violence against civilians", "Strategic developments"),
      0,
      chemical_agents
    ),
    racism = ifelse((
      grepl(
        "NAACP|BLM|BVM|African American Group (United States)",
        assoc_actor_1
      ) |
        grepl(
          "NAACP|BLM|BVM|African American Group (United States)",
          assoc_actor_2
        )
    ),
    1, 0)
  )

acled %>% group_by(event_type) %>% summarize(n=n()) %>% arrange(-n)
acled %>% group_by(sub_event_type) %>% summarize(n=n()) %>% arrange(-n)

acled %>% 
  group_by(counter_event) %>%
  summarize(n = n(),
            across(.cols = c(arrests_any, chemical_agents),
                   .fns = ~ sum(.x, na.rm=T))) %>% 
  mutate(across(.cols = c(arrests_any, chemical_agents),
                .fns = ~.x / n * 100)) %>% 
  mutate(across(where(is.numeric), round, 1)) %>% 
  rename(`Event Type` = counter_event, `Event Count` = n,
         `Arrests` = arrests_any,`Chemical Agents` = chemical_agents)