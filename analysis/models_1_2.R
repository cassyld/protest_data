####################################################################
# Replication Main Models 1, 2a, 2b
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
# sampling strategy to address extreme data imbalance
# ACLED data (Model 1)

# set up df with only necessary predictors
acled_chem <- acled_modeling %>%
  select(counter_event, issue_racism = racism, chemical_agents) %>% 
  filter(across(everything(), ~ !is.na(.x)))

acled_chem %>% group_by(chemical_agents) %>% summarize(n = n()) %>% mutate(tot = sum(n), prop = n/tot)

# train/test
set.seed(45)
acled_chem_train <- acled_chem %>% 
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
acled_chem_balanced <- ovun.sample(chemical_agents ~ ., data = acled_chem_train,
                                   N = nrow(acled_chem_train), p = 0.4, 
                                   seed = 45, method = "both")$data

acled_chem_balanced %>% group_by(chemical_agents) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n))

## CCC Data (Model 2a, 2b)
ccc_chem <- ccc_modeling %>%
  select(counter_event, issue_racism, chemical_agents, valence) %>% 
  filter(!is.na(chemical_agents), !is.na(valence), valence != 0) %>% 
  mutate(chemical_agents = chemical_agents*1,
         valence = ifelse(valence == 1, 1, 0),
         valence = as.character(valence))

ccc_chem %>% group_by(chemical_agents) %>% summarize(n=n()) %>%
  mutate(tot = sum(n), prop = n/tot)

# train/test
set.seed(45)
ccc_chem_train <- ccc_chem %>%
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# Under-sample non-events and over-sample events
ccc_chem_balanced <- ovun.sample(chemical_agents ~ ., data = ccc_chem_train,
                                 N = nrow(ccc_chem_train), p = 0.4, 
                                 seed = 45, method = "both")$data

# check new balance is ~60/40
ccc_chem_balanced %>% group_by(chemical_agents) %>% summarize(n=n()) %>% 
  mutate(tot = sum(n), prop = n/tot)
####################################################################

####################################################################
## Chemical Agent Models (1, 2a, 2b)
model1 <- glm(chemical_agents ~ ., data = acled_chem_balanced, family = "binomial")
model2a <- glm(chemical_agents ~ counter_event + issue_racism,
               data = ccc_chem_balanced, family = "binomial")
model2b <- glm(chemical_agents ~ counter_event + issue_racism + valence,
               data = ccc_chem_balanced, family = "binomial")

# save
# save(model1, model2a, model2b,
#    file =paste0(pathData,"chemical_agent_models_full_data.RData"))
####################################################################

####################################################################
# Latex table
# TABLE 4 Appendix
stargazer(model1, model2a, model2b,
          title = "Models 1, 2a, and 2b - Explanatory factors in chemical agent usage",
          label = "tab:ca-logreg", 
          column.labels = c("(1)", "(2a)", "(2b)"),
          style = "APSR",
          model.numbers = FALSE,
          ci = TRUE,
          omit.stat=c("ser","f"),
          align=TRUE,
          dep.var.labels = "Chemical Agents",
          covariate.labels = c("Counter Event", "Issue Racism", "Left-Wing Protesters", "Constant"),
          stype = "APSR")


# Model graphics
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Model 1 (ACLED)")
model2aFrame <- data.frame(Variable = rownames(summary(model2a)$coef),
                           Coefficient = summary(model2a)$coef[, 1],
                           SE = summary(model2a)$coef[, 2],
                           modelName = "Model 2a (CCC)")
model2bFrame <- data.frame(Variable = rownames(summary(model2b)$coef),
                           Coefficient = summary(model2b)$coef[, 1],
                           SE = summary(model2b)$coef[, 2],
                           modelName = "Model 2b (CCC)")

# combine these data.frames
ca_model_frame <- data.frame(rbind(model1Frame, model2aFrame, model2bFrame)) %>% 
  mutate(
    Variable = case_when(
      Variable == "(Intercept)" ~ "Intercept",
      Variable == "counter_event1" ~ "Counter Event",
      Variable == "issue_racism1" ~ "Issue Racism",
      Variable == "valence1" ~ "Left-Wing Protesters",
      T ~ Variable),
    Variable = factor(Variable, levels = c("Intercept", "Counter Event", "Issue Racism", "Left-Wing Protesters")),
    modelName = factor(modelName, levels = c("Model 1 (ACLED)", "Model 2a (CCC)", "Model 2b (CCC)")),
    model_order = case_when(modelName == "Model 1 (ACLED)" ~ 1,
                            modelName == "Model 2a (CCC)" ~ 2,
                            T ~ 3))

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# coefficient plot
ca_plot <- ggplot(ca_model_frame, aes(colour = reorder(modelName, -model_order))) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Variable, y = Coefficient,
                      ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 16) +
  coord_flip() +
  scale_color_discrete(guide=guide_legend(reverse=T)) +
  theme_bw() +
  labs(y = "Log Odds", colour = "Model") +
  theme_minimal()

print(ca_plot)
#ggsave(paste0(pathGraphics, "results_full_data_chemical_agents.png"), plot = ca_plot, width = 8, height=5)








