####################################################################
# Replication Main Models 3, 4a, 4b
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
ccc_modeling <- ccc_modeling %>% filter(!(counter_event == 0 & matching_counter == 1))
# set up df with only necessary predictors
acled_chem <- acled_modeling %>%
  select(counter_event, issue_racism = racism, chemical_agents) %>% 
  filter(across(everything(), ~ !is.na(.x)))
####################################################################

####################################################################
# Predicting Arrests
## ACLED (Model 3)
acled_arr <- acled_modeling %>%
  select(counter_event, issue_racism = racism, arrests_any) %>% 
  filter(across(everything(), ~ !is.na(.x)))

acled_arr %>% group_by(arrests_any) %>% summarize(n=n()) %>% mutate(tot = sum(n), prop = n/tot)

# Under-sample non-events and over-sample events
# train/test
set.seed(45)
acled_arr_train <- acled_arr %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))

# over & undersample
acled_arr_balanced <- ovun.sample(arrests_any ~ ., data = acled_arr_train,
                                  N = nrow(acled_arr_train), p = 0.4, 
                                  seed = 45, method = "both")$data

# make arrests data
ccc_arr <- ccc_modeling %>%
  filter(!is.na(arrests_any), !is.na(valence), valence != 0) %>% 
  select(counter_event, issue_racism, arrests_any, valence) %>% 
  mutate(valence = ifelse(valence == 1, 1, 0),
         valence = as.character(valence)) 

ccc_arr %>% group_by(arrests_any) %>% summarize(n=n()) %>% mutate(tot = sum(n), prop = n/tot)

#train/test
ccc_arr_train <- ccc_arr %>%
  mutate(arrests_any = factor(arrests_any, levels = c(1, 0), labels = c("yes","no")))

# over & undersample
ccc_arr_balanced <- ovun.sample(arrests_any ~ ., data = ccc_arr_train,
                                N = nrow(ccc_arr_train), p = 0.4, 
                                seed = 45, method = "both")$data

ccc_arr_balanced %>% group_by(arrests_any) %>% summarize(n=n()) %>% mutate(tot = sum(n), prop = n/tot)
####################################################################

####################################################################
## Arrest Models (3, 4a, 4b)
# chemical agent models
model3 <- glm(arrests_any ~ ., data = acled_arr_balanced, family = "binomial")
model4a <- glm(arrests_any ~ counter_event + issue_racism,
               data = ccc_arr_balanced, family = "binomial")
model4b <- glm(arrests_any ~ counter_event + issue_racism + valence,
               data = ccc_arr_balanced, family = "binomial")

# save
#save(model3, model4a, model4b, file = paste0(pathData,"arrest_models_full_data.RData"))

# Latex table, APPENDIX table 5
# break, does  not replicate
stargazer(model3, model4a, model4b,
          title = "Models 3, 4a, and 4b - Explanatory factors in arrests",
          label = "tab:arr-logreg", 
          column.labels = c("(3)", "(4a)", "(4b)"),
          model.numbers = FALSE,
          style = "APSR",
          ci = TRUE,
          omit.stat=c("ser","f"),
          align=TRUE,
          dep.var.labels = "Arrest",
          covariate.labels = c("Counter Event", "Issue Racism", "Left-Wing Protesters", "Constant"))#,
#out = paste0(pathDrop, "manuscript/results_full_data_chemical_agents_table.tex"))


# Plots
# Put model estimates into temporary data.frames:
model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Model 3 (ACLED)")
model4aFrame <- data.frame(Variable = rownames(summary(model4a)$coef),
                           Coefficient = summary(model4a)$coef[, 1],
                           SE = summary(model4a)$coef[, 2],
                           modelName = "Model 4a (CCC)")
model4bFrame <- data.frame(Variable = rownames(summary(model4b)$coef),
                           Coefficient = summary(model4b)$coef[, 1],
                           SE = summary(model4b)$coef[, 2],
                           modelName = "Model 4b (CCC)")
# Combine these data.frames
arr_model_frame <- data.frame(rbind(model3Frame, model4aFrame, model4bFrame)) %>% 
  mutate(
    Variable = case_when(
      Variable == "(Intercept)" ~ "Intercept",
      Variable == "counter_event1" ~ "Counter Event",
      Variable == "issue_racism1" ~ "Issue Racism",
      Variable == "valence1" ~ "Left-Wing Protesters",
      T ~ Variable),
    Variable = factor(Variable, levels = c("Intercept", "Counter Event", "Issue Racism", "Left-Wing Protesters")),
    modelName = factor(modelName, levels = c("Model 3 (ACLED)", "Model 4a (CCC)", "Model 4b (CCC)")),
    model_order = case_when(modelName == "Model 3 (ACLED)" ~ 1,
                            modelName == "Model 4a (CCC)" ~ 2,
                            T ~ 3))

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
arr_plot <- ggplot(arr_model_frame, aes(colour = reorder(modelName, -model_order)))
arr_plot <- arr_plot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
arr_plot <- arr_plot + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                          ymax = Coefficient + SE*interval1),
                                      lwd = 1, position = position_dodge(width = 1/2))
arr_plot <- arr_plot + geom_pointrange(aes(x = Variable,
                                           y = Coefficient,
                                           ymin = Coefficient - SE*interval2,
                                           ymax = Coefficient + SE*interval2),
                                       lwd = 1/2, position = position_dodge(width = 1/2),
                                       shape = 16)
arr_plot <- arr_plot + coord_flip() + scale_color_discrete(guide=guide_legend(reverse=T)) + theme_bw()
arr_plot <- arr_plot + #ggtitle("Explanatory Factors in Arrests") +
  labs(y = "Log Odds", colour = "Model")  +
  theme_minimal()
#ggsave("results_full_data_arrests.png", plot = arr_plot, width = 8, height=5, path = pathGraphics)
print(arr_plot)
