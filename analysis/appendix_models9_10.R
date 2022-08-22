####################################################################
# Replication Main Models 9, 10a, 10b
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
# Influence of Social Media Reporting
## ACLED
# set up df with only necessary predictors; very imbalanced 
acled_chem <- acled_modeling %>%
  select(counter_event, issue_racism = racism, chemical_agents, source_soc_media) %>%
  filter(across(everything(), ~ !is.na(.x)))

# response as factor
acled_chem_train <- acled_chem %>% 
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# oversample CA and undersample non-CA events to 0.4 and 0.6
set.seed(45)
acled_chem_balanced <- ovun.sample(chemical_agents ~ ., data = acled_chem_train,
                                   N = nrow(acled_chem_train), p = 0.4, 
                                   seed = 45, method = "both")$data

# check
acled_chem_balanced %>% group_by(chemical_agents) %>%
  summarize(n = n()) %>% mutate(prop = n/sum(n))

## CCC data
# chem df
ccc_chem <- ccc_modeling %>%
  select(counter_event, issue_racism, chemical_agents, valence, source_soc_media) %>% 
  filter(!is.na(chemical_agents), !is.na(valence), valence != 0) %>% 
  mutate(chemical_agents = chemical_agents*1,
         valence = ifelse(valence == 1, 1, 0),
         valence = as.character(valence))#,

# response as factor
ccc_chem_train <- ccc_chem %>%
  mutate(chemical_agents = factor(chemical_agents, levels = c(1, 0), labels = c("yes","no")))

# over & undersample
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

# save
#save(model9, model10a, model10b,
#     file =paste0(pathData,"chemical_agent_models_full_data_soc_med.RData"))


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

# Coefficient plots
# Put model estimates into temporary data.frames:
model9Frame <- data.frame(Variable = rownames(summary(model9)$coef),
                          Coefficient = summary(model9)$coef[, 1],
                          SE = summary(model9)$coef[, 2],
                          modelName = "Model 9 (ACLED)")
model10aFrame <- data.frame(Variable = rownames(summary(model10a)$coef),
                            Coefficient = summary(model10a)$coef[, 1],
                            SE = summary(model10a)$coef[, 2],
                            modelName = "Model 10a (CCC)")
model10bFrame <- data.frame(Variable = rownames(summary(model10b)$coef),
                            Coefficient = summary(model10b)$coef[, 1],
                            SE = summary(model10b)$coef[, 2],
                            modelName = "Model 10b (CCC)")

# Combine these data.frames
ca_model_frame <- data.frame(rbind(model9Frame, model10aFrame, model10bFrame)) %>% 
  mutate(
    Variable = case_when(
      Variable == "(Intercept)" ~ "Intercept",
      Variable == "counter_event1" ~ "Counter Event",
      Variable == "issue_racism1" ~ "Issue Racism",
      Variable == "source_soc_media1" ~ "Source: Social Media",
      Variable == "valence1" ~ "Left-Wing Protesters",
      T ~ Variable),
    Variable = factor(Variable, levels = c("Intercept", "Counter Event", "Issue Racism",
                                           "Source: Social Media","Left-Wing Protesters")),
    modelName = factor(modelName, levels = c("Model 9 (ACLED)", "Model 10a (CCC)", "Model 10b (CCC)")),
    model_order = case_when(modelName == "Model 9 (ACLED)" ~ 1,
                            modelName == "Model 10a (CCC)" ~ 2,
                            T ~ 3))

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Coefficent plot
ca_plot <- ggplot(ca_model_frame, aes(colour = reorder(modelName, -model_order)))
ca_plot <- ca_plot + geom_hline(yintercept = 1, colour = gray(1/2), lty = 2)
ca_plot <- ca_plot + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                        ymax = Coefficient + SE*interval1),
                                    lwd = 1, position = position_dodge(width = 1/2))
ca_plot <- ca_plot + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                         ymax = Coefficient + SE*interval2),
                                     lwd = 1/2, position = position_dodge(width = 1/2),
                                     shape = 16)
ca_plot <- ca_plot + coord_flip() + scale_color_discrete(guide=guide_legend(reverse=T)) +
  theme_bw()
ca_plot <- ca_plot +
  labs(y = "Log Odds", colour = "Model") +
  theme_minimal()

ggsave(paste0("results_chemical_agents_soc_media.png"), plot = ca_plot, width = 8, height=5, path=pathGraphics)

print(ca_plot)
