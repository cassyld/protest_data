####################################################################
# Replication Figures 4 & 5
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################
rm(list = ls())

# libraries
library(tidyverse)
library(lubridate)
library(haven)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(patchwork)
library(viridis)
library(maps)
library(mapproj)
library(ggthemes)
library(ggmap)
library(usmap)
library(socviz)
library(tidycensus)
library(tidygeocoder)

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
  pathGraphics = paste0(pathDrop, 'graphics/acled_protest_only')
}
####################################################################


####################################################################
# data 
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

# map data
states <- readr::read_csv(paste0(pathData,"states.csv")) %>%
  janitor::clean_names() %>% 
  select(state_full = state, everything())

# state level
us_states_poly <- map_data("state") %>% 
  rename(state = region) %>% 
  mutate(state = str_to_title(state)) %>% 
  left_join(., states, by = c("state" = "state_full"))

# get county level map data
county_map_fips <- county_map %>% rename(fips = id) %>%
  select(fips, long, lat, group) %>% 
  mutate(fips = ifelse(nchar(fips) < 5, paste0("0", fips), fips))

# acled lat/lon fips
acled_coords <- read_csv(paste0(pathData, "acled_reverse_geocoding/acled_coords_geocodio.csv")) %>% 
  select(latitude, longitude, fips_code = `County FIPS`) %>% 
  mutate(fips_code = ifelse(nchar(fips_code) < 5, paste0("0", fips_code), fips_code))

# population by fips
fips_pop <- read_csv(paste0(pathData,"population_by_fips.csv")) %>% 
  mutate(fips_code = as.character(fips_code))
####################################################################


####################################################################
## data prep
## coverage
acled %>% group_by(location, state, .groups = "keep") %>% 
  summarize(n = n()) %>% select(-.groups) %>% nrow()

## median number of events per location
acled %>% 
  mutate(location = ifelse(grepl("New York - ", location), "New York City", location),
         county = ifelse(grepl("New York City", location), "New York City", county)) %>% 
  group_by(location, county, state, .groups = "keep") %>% 
  summarize(n = n()) %>% 
  pull(n) %>% 
  summary()

## proportion of events accounted for by top 10 cities
acled %>%  
  mutate(location = ifelse(grepl("New York - ", location), "New York City", location),
         county = ifelse(grepl("New York City", location), "New York City", county)) %>% 
  group_by(location, county, state, .groups = "keep") %>%
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(prop_events = n / sum(n)) %>% 
  arrange(-prop_events) %>%
  # prop accounted for by top n cities 
  head(5) %>% 
  pull(prop_events) %>% 
  sum()

## proportion of locations with 2 events or less
acled %>%  
  mutate(location = ifelse(grepl("New York - ", location), "New York City", location),
         county = ifelse(grepl("New York City", location), "New York City", county)) %>% 
  group_by(location, county, state, .groups = "keep") %>%
  summarize(n = n()) %>% 
  mutate(n_event = ifelse(n <= 2, 1, 0)) %>% 
  group_by(n_event) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))

## proportion of locations with 3 events or less
acled %>%  
  mutate(location = ifelse(grepl("New York - ", location), "New York City", location),
         county = ifelse(grepl("New York City", location), "New York City", county)) %>% 
  group_by(location, county, state, .groups = "keep") %>%
  summarize(n = n()) %>% 
  mutate(n_event = ifelse(n <= 3, 1, 0)) %>% 
  group_by(n_event) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))
####################################################################

####################################################################
## map: number of events per 100k 
acled_by_county <- acled %>% 
  filter(event_date >= ymd("2020-01-01"), event_date < ymd("2021-08-01")) %>% 
  # add fips code
  left_join(., acled_coords, by = c("latitude", "longitude")) %>% 
  # number of events by fips
  group_by(fips_code) %>% 
  summarize(num_events = n()) %>% 
  # add population estimate for each fips code
  left_join(., fips_pop, by = c("fips_code")) %>% 
  # calculate rate
  mutate(num_events = ifelse(is.na(num_events), 0, num_events),
         rate = (num_events/pop) * 10000) %>% 
  # add geom info
  right_join(., county_map_fips, by = c("fips_code" = "fips")) %>% 
  select(fips_code, lat, long, group, pop, num_events, rate)

# plot
figure4 <- acled_by_county %>% 
  mutate(rate = ifelse(is.na(rate), 0, rate)) %>% 
  mutate(rate_bckt = case_when(
    rate == 0 ~ "0",
    rate > 0 & rate < 1 ~ "< 1",
    rate >= 1 & rate < 2 ~ "1-2",
    rate >= 2 & rate < 3 ~ "2-3",
    TRUE ~ "3+" )) %>% 
  mutate(rate_bckt = fct_relevel(rate_bckt, "0", "< 1", "1-2", "2-3", "3+")) %>% 
  ggplot(., aes(x = long, y = lat, fill = rate_bckt, group = group)) +
  geom_polygon(color = "gray90", size = 0.05) +
  coord_equal() +
  scale_fill_brewer(palette = "Greys") +
  labs(fill = "Events per 100k") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() +
  theme(legend.position = "bottom")

figure4
####################################################################

# BREAK HERE
# get fips code
fips_codes <- fips_codes %>% mutate(fips_code = paste0(state_code,county_code)) %>% 
  mutate(fips_code = ifelse(nchar(fips_code) < 5, paste0("0", fips_code), fips_code)) %>% 
  mutate(county_name = paste0(county, ", ", state_name))

# fips pop based on 2019 estimate
fips_pop <- read_csv("./data/population_by_fips.csv") %>% 
  mutate(fips_code = as.character(fips_code))


# CCC: Use FIPS code directly
# number of events by FIPS
ccc_by_county <- ccc %>% 
  filter(date >= ymd("2020-01-01"), date < ymd("2021-08-01")) %>% 
  mutate(fips_code = ifelse(nchar(fips_code) < 5, paste0("0", fips_code), fips_code)) %>% 
  group_by(fips_code) %>% 
  summarize(num_events = n()) %>% 
  # add pop info
  left_join(., fips_pop, by = c("fips_code" = "fips_code")) %>% 
  # calculate rate
  mutate(num_events = ifelse(is.na(num_events), 0, num_events),
         rate = (num_events/pop) * 100000) %>% 
  # add geom info
  right_join(., county_map_fips, by = c("fips_code" = "fips")) %>% 
  select(fips_code, lat, long, group, pop, num_events, rate)

# plot
figure5 <- ccc_by_county %>% 
  mutate(rate = ifelse(is.na(rate), 0, rate)) %>% 
  mutate(rate_bckt = case_when(
    rate < 1 ~ "0",
    rate >= 1 & rate < 10 ~ "< 10",
    rate >= 10 & rate < 20 ~ "10-20",
    rate >= 20 & rate < 30 ~ "20-30",
    #rate >= 30 & rate < 40 ~ "30-40",
    #rate >= 40 & rate < 50 ~ "40-50",
    TRUE ~ "30+" )) %>% 
  mutate(rate_bckt = fct_relevel(rate_bckt, "0", "< 10", "10-20", "20-30", "30+")) %>% #, "40-50", "50+")) %>% 

  ggplot(., aes(x = long, y = lat, fill = rate_bckt, group = group)) +
  geom_polygon(color = "gray90", size = 0.05) +
  coord_equal() +
  #scale_fill_grey(start = .8, end = 0, na.value = "#FFFFFF") +
  scale_fill_brewer(palette = "Greys") +
  labs(fill = "Events per 100k") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() +
  theme(legend.position = "bottom")

figure5