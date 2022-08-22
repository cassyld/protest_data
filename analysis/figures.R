####################################################################
# Replication Figures
# Paper Title:
# Authors:
####################################################################

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

# Figure 1: Spatial and temporal coverage, 13 datasets
source(figure1.R)

# Figure 2: Temporal coverage across CCC and ACLED.
source(figure2.R)

# Figure 3: Social media as a source in ACLED and CCC
source(figure3.R)

# Figure 4 & 5 (Maps) Spatial distribution of protests, ACLED & CCC
# need figure 5 still, ccc map / fips codes
source(figure4_5.R) 

# Figure 6: Predicting Chemical Agent Deployment, CCC + ACLED

# Figure 7: Predicting Arrests, CCC + ACLED 