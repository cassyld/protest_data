####################################################################
# Replication for Descriptive Appendix tables
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
acled_pepper_irr <- read_csv("../data/acled_chem_agents/pepper_irritants_acled_notes.csv") %>%
  filter(keep == 1) %>% 
  select(-target)

acled_teargas <- read_csv("../data/acled_chem_agents/teargas_acled_notes.csv") %>%
  filter(keep == 1) %>% 
  select(data_id, keep, context = tear_gas_context, notes)

# nspe
nspe <- read_dta(paste0(pathData,"nspe_project.dta"))

# filter both data to most recently completed month 
acled <- acled %>% filter(event_date < ymd("2021-08-01"))
ccc_counter <- ccc %>% filter(date < ymd("2021-08-01"))
####################################################################

####################################################################
# Appendix Table 2 "CCC-Reported Risks to Protesters at 
# Counter-Protests with Racism as Event Issue"
ccc %>% 
  mutate(issue_racism = ifelse(grepl("racism", issues), 1, 0)) %>% 
  filter(counter_event == 1, valence != 0, issue_racism==1) %>% 
  group_by(valence) %>% 
  #mutate(valence = as.factor(valence)) %>% 
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
# Appendix Table 3: ACLED Event Types Used in Main Analysis
acled <- acled %>%
  mutate(counter_event = ifelse(grepl("\\[counter", notes), 1, 0))

acled %>% 
  group_by(event_type) %>% 
  summarize(sum_counter = sum(counter_event == 1),
            n_total = n(),
            prop_counter = sum(counter_event == 1) / n())
####################################################################
