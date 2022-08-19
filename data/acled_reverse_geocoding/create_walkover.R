# purpose: walkover between ACLED lat/lon and FIPS code
library(tidyverse)
library(socviz)
library(sp)

# cd user paths
if(Sys.info()['user'] %in% c('dorffc')){
  pathGit = '~/ProjectsGit/c4_protestData/'
  pathDrop = '~/Dropbox/Research/c4_protestData/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathDrop, 'graphics/')
}

# ak user paths
if(Sys.info()['user'] %in% c('Amanda')){
  pathGit = '~/Documents/Vanderbilt/c4_research_lab/c4_protestData/'
  pathDrop = '~/Dropbox/c4_protestData/'
  pathData = paste0(pathGit, 'data/')
  pathGraphics = paste0(pathDrop, 'graphics')
}

## MAP ZIP CODE TO FIPS ###########################################################################
# note that this isn't correct bc zipcodes can be in multiple counties, so relationship between
#  fips and zip is not 1:1

# Step 1: combine batches 1 & 2 of lat/lon to zipcode. keep only necessary columns
acled_1 <- read_csv("./data/acled_reverse_geocoding/acled_batch1_geocodio.csv") %>%
  select(latitude, longitude, City, State, County, Zip) %>% 
  janitor::clean_names()

acled_2 <- read_csv("./data/acled_reverse_geocoding/acled_batch2_geocodio.csv") %>%
  select(latitude, longitude, City, State, County, Zip) %>% 
  janitor::clean_names()

acled_1 %>% rbind(acled_2) %>% write_csv("./data/acled_reverse_geocoding/acled_coords.csv")

acled_zips <- acled_1 %>% rbind(acled_2) %>% rename(zip_code = zip) %>% 
  distinct(latitude, longitude, city, state, county, zip_code)

# Step 2: read in FIPS code data
zip_fips <- readxl::read_xlsx("./data/acled_reverse_geocoding/ZIP_COUNTY_122010.xlsx") %>% 
  janitor::clean_names() %>% 
  select(zip_code = zip, fips_code = county) %>% 
  distinct(zip_code, fips_code)

# Step 3: Match data by zip code, save first if multiple matches
acled_fips <- acled_zips %>% left_join(., zip_fips, by = "zip_code") %>% 
  distinct(latitude, longitude, .keep_all = TRUE)

# Step 4: Save
acled_fips %>% write_csv(., "./data/acled_reverse_geocoding/acled_zip_fips.csv")

