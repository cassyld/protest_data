####################################################################
# Replication Appendix Models 9, 10a, 10b
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################

rm(list = ls())

# libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(stargazer)
library(coefplot)
library(ggeffects)
library(lubridate)
library(caret)
library(ROSE)


# cd user paths
if(Sys.info()['user'] %in% c('dorffc')){
  pathGit = '~/ProjectsGit/protest_data/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathGit, 'graphics/')
}

# ak user paths
if(Sys.info()['user'] %in% c('Amanda')){
  pathGit = '~/Documents/Vanderbilt/protest_data/'
  pathDrop = '~/Dropbox/c4_protestData/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathDrop, 'graphics')
}

####################################################################
# data (clean)
# ccc
ccc_modeling <- readr::read_csv(paste0(pathData,"/modeling_data/ccc_modeling_sm.csv"),
                                col_types = cols(
                                  .default = "c",
                                  date = col_date(format = ""),
                                  valence = col_double(),
                                  arrests_any = col_character(),
                                  issue_racism = col_character(),
                                  chemical_agents = col_logical()
                                )
)

# acled
acled_modeling <-
  read_csv(
    paste0(pathData, "/modeling_data/acled_modeling_sm.csv"),
    col_types = cols(counter_event = col_character(),
                     racism = col_character(),
                     source_soc_media = col_character())
  )


# data cleaning
ccc_modeling <- ccc_modeling %>%
  filter(!(counter_event == 0 & matching_counter == 1)) %>% 
  filter(date < ymd("2021-08-01"))

acled_modeling <- acled_modeling %>% 
  filter(event_date < ymd("2021-08-01"))
####################################################################

####################################################################
# Influence of Social Media Reporting
## ACLED data (Model 9)

acled_chem <- acled_modeling %>%
  select(counter_event, issue_racism = racism, chemical_agents, source_soc_media) %>%
  filter(across(everything(), ~ !is.na(.x)))

# response as factor
acled_chem_train <- acled_chem %>% 
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
set.seed(45)
acled_chem_balanced <- ovun.sample(chemical_agents ~ ., data = acled_chem_train,
                                   N = nrow(acled_chem_train), p = 0.4, 
                                   seed = 45, method = "both")$data

# check
acled_chem_balanced %>% group_by(chemical_agents) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n))

## CCC data (Models 10a, 10b)

ccc_chem <- ccc_modeling %>%
  select(counter_event, issue_racism, chemical_agents, valence, source_soc_media) %>% 
  filter(!is.na(chemical_agents), !is.na(valence), valence != 0) %>% 
  mutate(chemical_agents = chemical_agents*1,
         valence = ifelse(valence == 1, 1, 0),
         valence = as.character(valence))#,

# response as factor
ccc_chem_train <- ccc_chem %>%
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
set.seed(45)
ccc_chem_balanced <- ovun.sample(chemical_agents ~ ., data = ccc_chem_train,
                                 N = nrow(ccc_chem_train), p = 0.4, 
                                 seed = 45, method = "both")$data

# check new balance is ~60/40
ccc_chem_balanced %>% group_by(chemical_agents) %>% summarize(n=n()) %>% 
  mutate(tot = sum(n), prop = n/tot)

# chemical agent models
model9 <- glm(chemical_agents ~ ., data = acled_chem_balanced, family = "binomial")
model10a <- glm(chemical_agents ~ counter_event + issue_racism + source_soc_media,
                data = ccc_chem_balanced, family = "binomial")
model10b <- glm(chemical_agents ~ counter_event + issue_racism + source_soc_media + valence,
                data = ccc_chem_balanced, family = "binomial")

## Latex table
stargazer(model9, model10a, model10b,
          title = "Models 9, .10a, and 10b - Explanatory factors in chemical agent usage",
          label = "tab:ca-logreg-socmed", 
          column.labels = c("(9)", "(10a)", "(10b)"),
          style = "APSR",
          model.numbers = FALSE,
          ci = TRUE,
          omit.stat=c("ser","f"),
          align=TRUE,
          dep.var.labels = "Chemical Agents",
          covariate.labels = c("Counter Event", "Issue Racism", "Source: Social Media", "Left-Wing Protesters", "Constant"),
          stype = "APSR")

