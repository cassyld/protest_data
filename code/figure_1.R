####################################################################
# Replication for Figure 1 in the MS
# Paper Title: Data Innovations on Protests in the United States
# Authors: Cassy Dorff, Grace Adcox, Amanda Konet
####################################################################
rm(list = ls())

# libraries
library(tidyverse)
library(ggplot2)
library(vistime)

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


tl_df <- readxl::read_xlsx(paste0(pathData, "timeline_events_datasets.xlsx"),
                           sheet = "coverage") %>% 
  rename(start = start_year, end = end_year) %>% 
  mutate(
    start_year = start,
    end_year = end,
    start = lubridate::mdy(paste("1", "1", as.character(start), sep = "-")),
    end = lubridate::mdy(paste("1", "1", as.character(end), sep = "-")),
    coverage = ifelse(coverage == "Global", "Global without U.S.", coverage),
    short_name = case_when(
      name == "Dynamics of Collective Action" ~ "DoCA",
      name == "Conflict and Peace Data Bank" ~ "COPDAB",
      name == "Mass Mobilization in Autocracies" ~ "MMAD",
      name == "Mass Mobilization Dataset" ~ "MM",
      name == "Global Nonviolent Action Database" ~ "GNAD",
      name == "Count Love" ~ "CL",
      name == "Global Protest Tracker" ~ "GPT",
      name == "Women in Resistance Dataset" ~ "WiRe",
      name == "Major Episodes of Contention" ~ "MEC",
      T ~ name
    ),
    medium_name = case_when(
      name == "Dynamics of Collective Action" ~ "DoCA",
      name == "Conflict and Peace Data Bank" ~ "COPDAB",
      name == "NSPE" ~ "National Study of Protest Events",
      T ~ name
    )
  )

tl_df

tl_df <- tl_df[!tl_df$name == "SPEED",]

# factor on name - manually order by levels of coverage
tl_df <- tl_df %>% 
  mutate(
    name_fct = factor(short_name, 
                      levels = c(
                        # REGIONAL
                        "SCAD",
                        # GLOBAL W/O U.S.
                        "MM", "MMAD", "WiRe",
                        # U.S.
                        "CCC", "CL", "DoCA",
                        "NSPE",
                        # GLOBAL W/ U.S.
                        "ACLED", "COPDAB", "GDELT", "GPT", "NAVCO"),
                      labels = seq(1, 13, 1)),
    name_fct = as.numeric(name_fct),
    # factor based on original coverage for color
    color_fct = factor(coverage,
                       levels = c("Regional", "Global without U.S.",
                                  "U.S. Only", "Global with U.S.")))

figure1 = tl_df %>% 
  arrange(name_fct) %>% 
  ggplot(., aes(x = start_year, y = reorder(short_name, -name_fct), group = color_fct)) +
  geom_point(aes(x = start_year, y = reorder(short_name, -name_fct), color = color_fct), size = 2.25) +
  geom_point(aes(x = end_year, y = reorder(short_name, -name_fct), color = color_fct), size = 2.25) +
  geom_segment(aes(x = start_year, xend = end_year,
                   y = reorder(short_name, -name_fct), yend = short_name,
                   color = color_fct),
               size = 3) +
  coord_cartesian(xlim = c(1900, 2021)) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  #scale_colour_grey() +
  scale_color_manual(values = c("black", "grey50", "red", "grey")) +
  #scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")) +
  labs(
    x = "Years",
    y = "Dataset",
    color = "Coverage"
  ) + 
  theme_minimal()

figure1

#ggsave(paste0(pathGraphics, "figure1_color.pdf"), plot = figure1, dpi=350, width = 8, height=5)
