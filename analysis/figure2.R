####################################################################
# Replication Figures 2
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################

# data manip
library(tidyverse)
library(lubridate)
library(haven)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(patchwork)
library(viridis)

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
  pathGraphics = paste0(pathDrop, 'graphics/acled_protest_only')
}

# data (clean)
# ccc
ccc <- readr::read_csv(paste0(pathData,"ccc_clean.csv"),
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

# number of events in each dataset by moyr
acled_by_moyr <- acled %>% 
  filter(!is.na(event_date), event_date < ymd("2021-08-01")) %>%
  # floor date to beginning of month
  mutate(moyr = lubridate::floor_date(event_date, "month")) %>% 
  group_by(moyr) %>% summarize(n = n())

ccc_by_moyr <- ccc %>% 
  filter(!is.na(date), date < ymd("2021-08-01")) %>% 
  # floor date to beginning of month
  mutate(moyr = lubridate::floor_date(date, "month")) %>% 
  group_by(moyr) %>% summarize(n = n())

# legend - color by dataset
colors_events_moyr <- c("ACLED" = "forestgreen", "CCC" = "purple")

# plot num events by moyr
figure2 = ggplot() +
  geom_line(aes(x = moyr, y = n, color = "ACLED"),
            data = acled_by_moyr,
            alpha = 0.9) +
  geom_line(aes(x = moyr, y = n, color = "CCC"),
            data = ccc_by_moyr,
            alpha = 0.4) +
  coord_cartesian(ylim = c(0, 8000), xlim = c(ymd("2017-01-01"), ymd("2021-08-01"))) + 
  scale_color_manual(values = colors_events_moyr) +
  scale_x_date(date_labels = "%m-%y", breaks = "6 months",
               limit = c(ymd("2017-01-01"), ymd("2021-08-01")), expand = c(0,0)) +
  labs(
    #title = "Event Timeline",
    color = "Dataset",
    x = "Month-Year",
    y = "Event Count"
  ) +
  theme_minimal()

figure2