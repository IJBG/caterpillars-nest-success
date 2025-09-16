#Yeah, so, a lot of this is that we're going to need to pull code from caterpillars_analysis_public I think!
source("code/0.caterpillars_count_analysis_functions.r")
library(stringr)
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(ggspatial)
library(viridis)

# Function for calculating and displaying arthropod phenology by week
meanDensityByWeek

# Function for calculating summary stats about survey effort at individual sites
siteEffortSummary

# Criteria for inclusion (records refers to survey events)
siteSummary

# Function for extracting %, density, and biomass during different specified windows
#   (30-day window starting from solstice, certain window past greenup, peak period)
phenoSummary

# Split up long site names across two lines by introducing \n in the middle at a space break
siteNameForPlotting

#get caterpillars count locations
cc_locations <- read.csv("data/caterpillars-count/CaterpillarsCountData.csv") %>%
  mutate(Year = str_extract(.$LocalDate, "[0-9]{4}")) %>%
  distinct(SiteName, Latitude, Longitude, Year) %>%
  mutate(roundLat = round(Latitude, 0),
         roundLon = round(Longitude, 0),
         Year = as.integer(Year))

#nestwatch locations
nest_locations <- read.csv("data/nestwatch/attempts_locs_20250617.csv") %>%
  distinct(Attempt.ID, Latitude, Longitude, Year) %>%
  mutate(roundLat = round(Latitude, 0),
         roundLon = round(Longitude, 0))

#how much to scale the nestwatch points, we'll have them just with the rounded lat/lon.. maybe we won't use this idk
nest_pch <- nest_locations %>%
  group_by(roundLat, roundLon, Year) %>%
  summarize(n_nest_attempts = n()) %>%
  mutate(scale_pch = log(n_nest_attempts)+.5)

cc_nest_scaled <-
  cc_locations %>%
  left_join(nest_pch, by = c("roundLat", "roundLon", "Year")) %>%
  mutate(n_nest_attempts = ifelse(is.na(n_nest_attempts) == TRUE, 0, n_nest_attempts))
table(cc_nest_scaled$n_nest_attempts >= 20)
#172 cc sites with less than 20 nests attempts within 100km in that year
#421 cc sites with at least 20 nest attempts within 100km in that year.
  #could take this a step further, and like, color code each location by how many cc! years we have data for? b/c otherwise there's 24 years of data we'd be making maps for. hm...
#but also like, not rlly a problem to make 24 maps just put it on a loop. And that way we don't have to resize the data.

######################## mapping

#get state boundaries for context
usa_map <- ggplot2::map_data("state")

cc_points <- st_as_sf(cc_locations, coords = c("Longitude", "Latitude"), crs = 4326)
nw_points <- st_as_sf(nest_pch, coords = c("roundLat", "roundLon"), crs = 4326)

ggplot() +
  geom_sf(data = cc_points, )


#could left join the nest and cc data by year + round lat round lon and plot for each year of CC data how many nestboxes are around it. :)
