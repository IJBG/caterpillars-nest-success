#Yeah, so, a lot of this is that we're going to need to pull code from caterpillars_analysis_public I think!
source("code/0.caterpillars_count_analysis_functions.r")
library(stringr)
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(ggspatial)
library(viridis)
library(tidyverse)
library(tmap)

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
  #select only sites that are within the US and Canada
  filter(!Region %in% c("GB", "India", "LB")) %>%
  distinct(SiteName, Latitude, Longitude, Year) %>%
  mutate(roundLat = round(Latitude, 0),
         roundLon = round(Longitude, 0),
         roundhalfLat = round(Latitude*2)/2,
         roundhalfLon = round(Longitude*2)/2,
         roundquarterLat = round(Latitude*4)/4,
         roundquarterLon = round(Longitude*4)/4,
         Year = as.integer(Year)) %>%
  ungroup()

#nestwatch locations
nest_locations <- read.csv("data/nestwatch/attempts_locs_20250617.csv") %>%
  distinct(Attempt.ID, Latitude, Longitude, Year) %>%
  mutate(roundLat = round(Latitude, 0),
         roundLon = round(Longitude, 0),
         roundhalfLat = round(Latitude*2)/2,
         roundhalfLon = round(Longitude*2)/2,
         roundquarterLat = round(Latitude*4)/4,
         roundquarterLon = round(Longitude*4)/4)

#how much to scale the nestwatch points, we'll have them just with the rounded lat/lon.. maybe we won't use this idk
nest_pch_degree_full <- nest_locations %>%
  group_by(roundLat, roundLon, Year) %>%
  summarize(n_nest_attempts_degree_full = n()) %>%
  mutate(scale_pch_degree_full = log(n_nest_attempts_degree_full)+.5) 

nest_pch_degree_half <- 
  nest_locations %>%
       group_by(roundhalfLat, roundhalfLon, Year) %>%
       summarize(n_nest_attempts_degree_half = n()) %>%
       mutate(scale_pch_degree_half = log(n_nest_attempts_degree_half)+.5)

nest_pch_degree_quarter <- 
  nest_locations %>%
       group_by(roundquarterLat, roundquarterLon, Year) %>%
       summarize(n_nest_attempts_degree_quarter = n()) %>%
       mutate(scale_pch_degree_quarter = log(n_nest_attempts_degree_quarter)+.5)


cc_nest_scaled <-
  cc_locations %>%
  left_join(nest_pch_degree_full, by = c("roundLat", "roundLon", "Year")) %>%
  left_join(nest_pch_degree_half, by = c("roundhalfLat", "roundhalfLon", "Year")) %>%
  left_join(nest_pch_degree_quarter, by = c("roundquarterLat", "roundquarterLon", "Year")) %>%
  #replace all NAs with 0s
  mutate(across(everything(), ~ replace_na(., 0)))
table(cc_nest_scaled$n_nest_attempts_degree_full >= 20)
#172 cc sites with less than 20 nests attempts within 100km in that year
#421 cc sites with at least 20 nest attempts within 100km in that year.
  #could take this a step further, and like, color code each location by how many cc! years we have data for? b/c otherwise there's 24 years of data we'd be making maps for. hm...
#but also like, not rlly a problem to make 24 maps just put it on a loop. And that way we don't have to resize the data.
table(cc_nest_scaled$n_nest_attempts_degree_half >= 20)
#306 with less than 20 nest attempts within 50km in that year
#284 with at least 20 nest attempts within 50km in that year
table(cc_nest_scaled$n_nest_attempts_degree_quarter >= 20)
#405 with less than 20 nest attempts within 25km in that year
#185 with at least 20 nest attempts within 25km in that year


#summarize how many nests (can be double counted if they close to multiple sites) are within each year
summarize_cc_nests <- cc_nest_scaled %>%
  group_by(Year) %>%
  summarize(n_cc_sites = n(),
         n_nesting_attempts_degree_full = sum(n_nest_attempts_degree_full),
         n_nesting_attempts_degree_half = sum(n_nest_attempts_degree_half),
         n_nesting_attempts_degree_quarter = sum(n_nest_attempts_degree_quarter)) 
write.csv(summarize_cc_nests, "data/n_datapoints_per_year.csv", row.names = FALSE)
#one note of caution is this doesn't account at all for the fact that some sites are close to one another. BUT, I think that will just be something I incorporate into my model?

######################## mapping

#get state boundaries for context
# states <- c("NC", "MA", "MD", "CT", "MI", "ON", "VA", "SC", "TN", "NY",
#             "GA", "CA", "OR", "UT", "ME", "OH", "WI", "DC", "TX", "RI", 
#             "PA", "MO", "IA", "MN", "AK", "AR", "WV", "IN", "NJ", "KY", 
#             "AB", "NV", "AL", "NH", "NM", "IL", "KS", "LA")

states <- c("AB", "AL", "CA", 'CT', "DC", 'GA', "IA", "IL", 'KS', 
            'LA', 'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'NC', 'NH',
            'NM', 'NV', 'NY', 'OH', 'OK', 'ON', 'PA', 'RI', 'SC',
            'TN', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV')

NAmap <- read_sf("data/maps", "ne_50m_admin_1_states_provinces_lakes") %>%
  filter(sr_adm0_a3 %in% c("USA", "CAN")) %>%
  mutate(cc = as.factor(ifelse(postal %in% states, 1, 0))) %>%
  st_transform(crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35")

#make caterpillar counts points
cc_points <- st_as_sf(cc_locations, coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>%
             st_transform(crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35")


cc_nest_scaled_points <-
  st_as_sf(cc_nest_scaled, 
           coords = c("Longitude", "Latitude"), 
           crs = 4326) %>%
  st_transform(crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35")

nw_points <- st_as_sf(nest_pch, coords = c("roundLat", "roundLon"),
                      crs = 4326)
                      #crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=35")

#set colors for states with/without CC! site
cols <- c("gray95", rgb(93/255, 156/255, 47/255))

#get bounding box coordinates
st_bbox(cc_points)
#xmin       ymin       xmax       ymax 
#-122.60038   13.46673   75.69135   55.42597 

year = 2024


ggplot() +
  #add basemap of states and CAN provinces with CC sides except for Alaska
  geom_sf(data = NAmap[NAmap$cc == 1 & NAmap$postal != "AK",]) +
  geom_sf(data = cc_nest_scaled_points[cc_nest_scaled_points$Year == year,], 
          aes(size = scale_pch),
          shape = 1
          ) +
  annotate("text", 
           x = -Inf, 
           y = Inf, 
           label = 
             paste0(
               year,
               ": ",
               nrow(cc_nest_scaled_points[cc_nest_scaled_points$Year == year,]),
               " Sites, ",
               sum(cc_nest_scaled_points[cc_nest_scaled_points$Year == year,]$n_nest_attempts),
               " Nesting Attempts"),
               hjust = -0.1, vjust = 1.1, size = 4.5, color = "black")
  #geom_point(data = cc_points,
  #           aes(geometry = geometry),
  #           stat = "sf_coordinates")

#same plot but for alaska, which we can later inset into the map.
ggplot() +
  geom_sf(data = NAmap[NAmap$cc == 1 & NAmap$postal == "AK",])



#could left join the nest and cc data by year + round lat round lon and plot for each year of CC data how many nestboxes are around it. :)
