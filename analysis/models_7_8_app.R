####################################################################
# Replication Models 7, 8a, 8b
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################
# libraries
# data manip
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
# Predicting Arrests
## ACLED data (Model 7)

# set up df with necessary columns
acled_arr <- acled_modeling %>%
  select(counter_event, issue_racism = racism, arrests_any) %>% 
  filter(across(everything(), ~ !is.na(.x)))

acled_arr %>% group_by(arrests_any) %>% summarize(n=n()) %>% mutate(tot = sum(n), prop = n/tot)

# train/test
set.seed(45)
acled_arr_train_idx <- createDataPartition(acled_arr$arrests_any, p = .7, list = F)
acled_arr_train <- acled_arr[ acled_arr_train_idx,] %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))
acled_arr_test  <- acled_arr[-acled_arr_train_idx,] %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
acled_arr_balanced <- ovun.sample(arrests_any ~ ., data = acled_arr_train,
                                  N = nrow(acled_arr_train), p = 0.4, 
                                  seed = 45, method = "both")$data

## CCC data (Models 8a, 8b)
# set up df with necessary columns
ccc_arr <- ccc_modeling %>%
  filter(!is.na(arrests_any), !is.na(valence), valence != 0) %>% 
  select(counter_event, issue_racism, arrests_any, valence, region) %>% 
  mutate(valence = ifelse(valence == 1, 1, 0),
         valence = as.character(valence))

ccc_arr %>% group_by(arrests_any) %>% summarize(n=n()) %>% mutate(tot = sum(n), prop = n/tot)

#train/test
set.seed(45)
ccc_arr_train_idx <- createDataPartition(ccc_arr$arrests_any, p = .7, list = F)
ccc_arr_train <- ccc_arr[ccc_arr_train_idx,] %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))
ccc_arr_test  <- ccc_arr[-ccc_arr_train_idx,] %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
ccc_arr_balanced <- ovun.sample(arrests_any ~ ., data = ccc_arr_train,
                                N = nrow(ccc_arr_train), p = 0.4, 
                                seed = 45, method = "both")$data

ccc_arr_balanced %>% group_by(arrests_any) %>% summarize(n=n()) %>% mutate(tot = sum(n), prop = n/tot)
####################################################################

####################################################################
## Arrest Models (7, 8a, 8b)
# chemical agent models
model7 <- glm(arrests_any ~ ., data = acled_arr_balanced, family = "binomial")
model8a <- glm(arrests_any ~ counter_event + issue_racism,
               data = ccc_arr_balanced, family = "binomial")
model8b <- glm(arrests_any ~ counter_event + issue_racism + valence,
               data = ccc_arr_balanced, family = "binomial")

summary(model8b)
# save
#save(model7, model8a, model8b, file = paste0(pathData,"arrest_models.RData"))

# Latex table
# TABLE 7 Appendix
stargazer(model7, model8a, model8b,
          title = "Models 7, 8a, and 8b - Explanatory factors in arrests",
          label = "tab:arr-logreg", 
          column.labels = c("(7)", "(8a)", "(8b)"),
          model.numbers = FALSE,
          style = "APSR",
          ci = TRUE,
          omit.stat=c("ser","f"),
          align=TRUE,
          dep.var.labels = "Arrest",
          covariate.labels = c("Counter Event", "Issue Racism", "Left-Wing Protesters", "Constant"))#,
