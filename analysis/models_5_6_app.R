####################################################################
# Replication Appendix Models 5, 6a, 6b
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################
# libraries

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(viridis)
library(stargazer)
library(coefplot)
library(ggeffects)
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
  pathGit = '~/Documents/Vanderbilt/c4_research_lab/c4_protestData/'
  pathDrop = '~/Dropbox/c4_protestData/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathDrop, 'graphics')
}

####################################################################
# data (clean)
# ccc
ccc_modeling <- readr::read_csv(paste0(pathData,"/modeling_data/ccc_modeling.csv"),
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
    paste0(pathData, "/modeling_data/acled_modeling.csv"),
    col_types = cols(counter_event = col_character(),
                     racism = col_character())
  )


# data cleaning
ccc_modeling <- ccc_modeling %>% filter(!(counter_event == 0 & matching_counter == 1))
####################################################################

####################################################################
## ACLED data (Model 5)

# set up df with only necessary predictors
acled_chem <- acled_modeling %>%
  select(counter_event, issue_racism = racism, chemical_agents) %>% 
  filter(across(everything(), ~ !is.na(.x)))

# sampling strategy to address extreme data imbalance
acled_chem %>% group_by(chemical_agents) %>% summarize(n = n()) %>% mutate(tot = sum(n), prop = n/tot)

# train/test
set.seed(45)
acled_train_idx <- createDataPartition(acled_chem$chemical_agents, p = .7, list = F)
acled_chem_train <- acled_chem[ acled_train_idx,] %>%
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))
acled_chem_test  <- acled_chem[-acled_train_idx,] %>%
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
acled_chem_balanced <- ovun.sample(chemical_agents ~ ., data = acled_chem_train,
                                   N = nrow(acled_chem_train), p = 0.4, 
                                   seed = 45, method = "both")$data

acled_chem_balanced %>% group_by(chemical_agents) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n))

## CCC Data (Model 6a, 6b)
ccc_chem <- ccc_modeling %>%
  select(counter_event, issue_racism, chemical_agents, valence) %>% 
  filter(!is.na(chemical_agents), !is.na(valence), valence != 0) %>% 
  mutate(chemical_agents = chemical_agents*1,
         valence = ifelse(valence == 1, 1, 0),
         valence = as.character(valence))

ccc_chem %>% group_by(counter_event) %>% summarize(n=n()) %>%
  mutate(tot = sum(n), prop = n/tot)

#train/test
set.seed(45)
ccc_train_idx <- createDataPartition(ccc_chem$chemical_agents, p = .7, list = F)
ccc_chem_train <- ccc_chem[ccc_train_idx,] %>%
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))
ccc_chem_test  <- ccc_chem[-ccc_train_idx,] %>%
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
ccc_chem_balanced <- ovun.sample(chemical_agents ~ ., data = ccc_chem_train,
                                 N = nrow(ccc_chem_train), p = 0.4, 
                                 seed = 45, method = "both")$data

# check new balance is ~60/40
ccc_chem_balanced %>% group_by(chemical_agents) %>% summarize(n=n())
####################################################################

####################################################################
## Chemical Agent Models (5, 6a, 6b)
model5 <- glm(chemical_agents ~ ., data = acled_chem_balanced, family = "binomial")
model6a <- glm(chemical_agents ~ counter_event + issue_racism,
               data = ccc_chem_balanced, family = "binomial")
model6b <- glm(chemical_agents ~ counter_event + issue_racism + valence,
               data = ccc_chem_balanced, family = "binomial")

####################################################################

####################################################################
# Latex table
# TABLE 6 Appendix
stargazer(model5, model6a, model6b,
          title = "Models 5, 6a, and 6b - Explanatory factors in chemical agent usage",
          label = "tab:ca-logreg", 
          column.labels = c("(5)", "(6a)", "(6b)"),
          style = "APSR",
          model.numbers = FALSE,
          ci = TRUE,
          omit.stat=c("ser","f"),
          align=TRUE,
          dep.var.labels = "Chemical Agents",
          covariate.labels = c("Counter Event", "Issue Racism", "Left-Wing Protesters", "Constant"),
          stype = "APSR")

