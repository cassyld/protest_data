####################################################################
# Replication Appendix Models 11, 12a, 12b
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
# Predicting Arrests
## ACLED data (Model 11)
# arrests df
acled_arr <- acled_modeling %>%
  select(counter_event, issue_racism = racism, arrests_any, source_soc_media) %>% 
  #mutate(source_soc_media = ifelse(source_soc_media == "0",
  #                                 "Traditional \n Media", "Social \n Media")) %>% 
  filter(across(everything(), ~ !is.na(.x)))

# factor response
acled_arr_train <- acled_arr %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))

# over & undersample
set.seed(45)
acled_arr_balanced <- ovun.sample(arrests_any ~ ., data = acled_arr_train,
                                  N = nrow(acled_arr_train), p = 0.4, 
                                  seed = 45, method = "both")$data

## CCC data (Models 12a, 12b)
# make arrests data
ccc_arr <- ccc_modeling %>%
  filter(!is.na(arrests_any), !is.na(valence), valence != 0) %>% 
  select(counter_event, issue_racism, arrests_any, valence, source_soc_media) %>% 
  mutate(valence = ifelse(valence == 1, 1, 0),
         valence = as.character(valence))#,

# factor response
ccc_arr_train <- ccc_arr %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))

# over & undersample
set.seed(45)
ccc_arr_balanced <- ovun.sample(arrests_any ~ ., data = ccc_arr_train,
                                N = nrow(ccc_arr_train), p = 0.4, 
                                seed = 45, method = "both")$data

ccc_arr_balanced %>% group_by(arrests_any) %>% summarize(n=n()) %>% mutate(tot = sum(n), prop = n/tot)


# chemical agent models
model11 <- glm(arrests_any ~ ., data = acled_arr_balanced, family = "binomial")
model12a <- glm(arrests_any ~ . - valence,
                data = ccc_arr_balanced, family = "binomial")
model12b <- glm(arrests_any ~ .,
                data = ccc_arr_balanced, family = "binomial")


# Latex table
stargazer(model11, model12a, model12b,
          title = "Models 11, 12a, and 12b - Explanatory factors in arrests",
          label = "tab:arr-logreg", 
          column.labels = c("(11)", "(12a)", "(12b)"),
          style = "APSR",
          model.numbers = FALSE,
          ci = TRUE,
          omit.stat=c("ser","f"),
          align=TRUE,
          dep.var.labels = "Chemical Agents",
          covariate.labels = c("Counter Event", "Issue Racism", "Source: Social Media", "Left-Wing Protesters", "Constant"),
          stype = "APSR")

