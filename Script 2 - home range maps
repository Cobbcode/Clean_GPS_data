# PART 2: Mapping the clean data ----

# Load packages ----
library(adehabitatHR)
library(tidyverse)
library(ggmap)
library(measurements)
library(stringr)
library(sf) # for conversion of spatial to sf 
library(lwgeom) # for sf_distance
library(lubridate)
library(rgeos)
library(cowplot) # for side by side ggplots

# For ggpattern, may need to use the following to install it first:
# install.packages("remotes")
# remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

# Notes ----
# For 2018 season, stargazers not enough waypoints for a homerange - so removed it from plotting and data

# Functions to be used later ----
## Create list of separated groups ----
create_group_list <- function(waypoints) {
  list_of_groups <- waypoints %>%
    group_by(Group) %>%
    group_split(.keep = TRUE)
  group_names <- sort(unique(waypoints$Group))
  list_of_groups <- setNames(list_of_groups,group_names)
}

## get kernel of group from set of coordinates ----
# Two inputs: set of coordinates, and a number for homerange % e.g. 95 or 50
get_kernel <- function(coordinates,percent) {
  kernel <- kernelUD(coordinates)
  homerange <- getverticeshr(kernel,percent = percent)
  homerange <- st_as_sf(homerange) # using sf library
  homerange <- st_set_crs(homerange, 4326) # set Coordinate reference system as WGS84 / 4326
  homerange # output
}

# Read and format data ----
final_waypoints <- read.csv("cleaned_waypoints.csv",header = TRUE)
final_waypoints <- as_tibble(final_waypoints)
final_waypoints <- final_waypoints %>%
  mutate(Group = factor(Group),  # ensure group is a factor
           Date = as_date(Date), # format date correctly
           Season = factor(Season)) # convert character to factor

lead_events <- read.csv("lead_events_with_coordinates.csv",header=TRUE) 
lead_events <- as_tibble(lead_events) %>%
  mutate(Group = factor(Group),  # ensure group is a factor
         Date = as_date(Date), # format date correctly
         Season = factor(Season)) %>% # convert character to factor
  distinct(Lead_number,.keep_all = TRUE) %>% # keep only 1 entry per lead event
  mutate(Season = str_replace(Season,"Non_breeding","Non-breeding")) # Correct hyphen in text


# Get Sorabi map ----
# Register API key to use google maps with ggmap
# See here: https://cran.r-project.org/web/packages/ggmap/readme/README.html 
# Google API key: ####
register_google(key = ####)

# get map for Sorabi region
sorabi <- c(lon = 30.7825 ,lat = -24.205) # centered area quite well

# get the map from coordinates above
sorabi_map <- get_map(location = sorabi, zoom = 15, scale = 2,maptype = "satellite")

# Plot all groups to double check outliers ----
#ggmap(sorabi_map) +
#  geom_point(aes(Longitude,Latitude,color=Group),alpha=1,size=1.4,data=final_waypoints) +
#  facet_wrap(~Group)

#  Function: split_mongoose_data ----

## Input 1: Either a year (e.g. 2013) if you want Non-breeding season
# OR a date interval if you want breeding season, in the format interval(ymd("2012-10-01"),ymd("2013-01-28")) 
## Input 2: waypoints, in our case the Final_waypoints tibble
split_mongoose_data <- function(date_interval,waypoints,lead_events) {
  
  if (is.interval(date_interval)) { # if input is date interval...
     waypoints <- waypoints %>% filter(Date %within% date_interval) # get waypoints during breeding season
    lead_waypoints <- lead_events %>% filter(Date %within% date_interval) # get lead waypoints during breeding season
  } else if (!is.interval(date_interval)) { # else if not an interval... 
    waypoints <- waypoints %>% filter(year(Date) == date_interval & Season =="Non-breeding") # get non-breeding waypoints in that year
    lead_waypoints <- lead_events %>% filter(year(Date) == date_interval & Season =="Non-breeding") # same for lead waypoints
    #waypoints <- waypoints %>% filter(year(Date) == date_interval & month(Date) >= 4 & month(Date) < 10) # if using 5 month set time intervals
  }
  
  # Split waypoint data into separate groups, storing it in a list
  list_of_groups <- create_group_list(waypoints)
  
  # for each group within the list, apply another custom function, to extract coordinates as spatial points into a new list
  list_spatial_points <- lapply(list_of_groups, function(x) {SpatialPoints(cbind(x$Longitude,x$Latitude))})
  
  # Avoid error by disabling s2
  sf_use_s2(FALSE)
  
  # for each group, get the 95% and 50% kernels, store this in a list
  group_ranges_95 <- lapply(list_spatial_points,get_kernel,95)
  group_ranges_50 <- lapply(list_spatial_points,get_kernel,50)
  
  # store our newly created lists into a final list: we can then access these outside of this function
  final_list <- list(waypoints,list_of_groups,list_spatial_points,group_ranges_95,group_ranges_50,lead_waypoints)
  final_list <- setNames(final_list, c("waypoints","list_of_groups","list_spatial_points","group_ranges_95","group_ranges_50",
                                       "lead_waypoints"))
}

# Breeding season dates ----

# Original dates that match with earlier model averaging analysis
breeding_season_list <- list(
  interval(ymd("2012-10-01"),ymd("2013-01-28")),
  interval(ymd("2013-08-20"),ymd("2014-05-14")),
  interval(ymd("2014-09-01"),ymd("2015-03-21")),
  interval(ymd("2015-09-08"),ymd("2016-04-14")),
  interval(ymd("2016-10-24"),ymd("2017-04-02")),
  interval(ymd("2017-08-10"),ymd("2018-03-02")),
  interval(ymd("2018-08-02"),ymd("2019-04-19")),
  interval(ymd("2019-08-19"),ymd("2020-04-07")),
  interval(ymd("2020-08-28"),ymd("2021-04-21"))
)

## For 2012 non breeding and 2012-2013 breeding, not plotting as not enough data ----
# final_waypoints %>% filter(year(Date) == 2012 & Season == "Non-breeding") %>%
#   nrow() # only 38 rows of data
# 
# final_waypoints %>% filter(Date %within% breeding_season_list[[1]]) %>%
#   nrow() # only 67 rows of data for 2012-2013 breeding season

# Theme for plots ----
fig_theme <- ggplot2::theme(legend.text = element_text(size=13,colour="black"),
                            legend.key = element_rect(fill = "white", colour = "black"),
                            legend.key.size = unit(1,"line"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title = element_text(size = 13))

# Create group colour variables ----
group_colours <- c(Bookworms = "blue",Fish_eagles = "yellow",Giraffe = "green",
                   Half_pints = "orange",Jelly_babies = "red",Little_britains = "purple",
                   Looney_toons = "magenta", Scallywags = "grey", Shakespeares = "white",
                   Stargazers = "maroon",Teaspoons = "pink", Terranova = "violet",
                   West_enders = "cyan")

# Plot map function ----
# 1 input: the breeding season we create from previous function
plot_map <- function(breeding_season) {
  
  ## Convert home ranges list into dataframe for plotting ----
  df_home_ranges_95 <- do.call(rbind.data.frame, breeding_season[["group_ranges_95"]])
  df_home_ranges_95 <- rownames_to_column(df_home_ranges_95,"Group")
  # Same for 50 % ranges
  df_home_ranges_50 <- do.call(rbind.data.frame, breeding_season[["group_ranges_50"]])
  df_home_ranges_50 <- rownames_to_column(df_home_ranges_50,"Group")
  
  ## Get overlapping regions ----
  all_intersections <- data.frame() # create empty dataframe before loop
  for (i in 1:nrow(df_home_ranges_95)) { # for each row of the homerange df...
    current_group <- df_home_ranges_95[i,] # get current group name
    compare_to <- df_home_ranges_95[-i,] # remove current group from df, to avoid self-comparison
    
    # Get the index values where the current group intersects with other groups
    index_intersects <- st_intersects(current_group,compare_to) 
    index_intersects <- index_intersects[[1]] # Extract vector
    
    # Get geometry of overlaps, using index above which tells us which groups overlap
    intersections <- st_intersection(current_group,compare_to[index_intersects,])
    
    # bind pre-existing dataframe with current group overlaps
    all_intersections <- rbind(all_intersections,intersections)
  }
  
  # Store lead waypoints as spatial coordinates that can be compared against polygons
  lead_waypoints <- breeding_season[["lead_waypoints"]]
  spatial_lead_waypoints <- SpatialPoints(cbind(lead_waypoints$Longitude,lead_waypoints$Latitude)) # Get spatial waypoints
  spatial_lead_waypoints <- st_as_sf(spatial_lead_waypoints) %>% st_set_crs(4326) # set Coordinate reference system as WGS84 / 4326

  # If lead waypoint is within an overlap zone or core, return TRUE, else FALSE
  is_in_overlap <- st_within(spatial_lead_waypoints,all_intersections) %>% lengths > 0
  is_in_core <- st_within(spatial_lead_waypoints,df_home_ranges_50) %>% lengths > 0
  
  # Add three level factor to data: is lead event in overlap_and_core, overlap or core
  # And add two level factor: overlap and core, where if in both, it is just overlap
  lead_waypoints <- lead_waypoints %>% 
    mutate(in_overlap = is_in_overlap,
           in_core = is_in_core) %>%
    mutate(territory_three_levels = ifelse(in_overlap == TRUE & in_core == TRUE,"overlap_and_core",
                                           ifelse(in_overlap == TRUE & in_core == FALSE,"overlap",
                                                  ifelse(in_overlap == FALSE & in_core == TRUE,"core",NA))),
           territory_two_levels = ifelse(in_overlap == TRUE, "overlap",
                                         ifelse(in_core == TRUE,"core",NA)))
           
 
  # Get distance from each lead event to each group 95 % boundary
  # Note: if within own boundary, returns ZERO
  distance_to_group <- st_distance(spatial_lead_waypoints,df_home_ranges_95)
  colnames(distance_to_group) <- df_home_ranges_95$Group # change column names to each group
  units(distance_to_group) <- NULL # Remove the meter unit of measurement from data
  distance_to_group <- as_tibble(distance_to_group)
  distance_to_group <- bind_cols(Group = lead_waypoints$Group,distance_to_group) %>%
    mutate(dist_to_own_group = NA)

  # Get distance from each lead event to 95 % boundary, but this time,
  # if inside its own boundary, instead of zero, returns a distance - which we make NEGATIVe later
  reformat_homerange <- df_home_ranges_95 %>% st_cast(to = "MULTILINESTRING")
  distance_to_group_reformatted <- st_distance(spatial_lead_waypoints,reformat_homerange)
  colnames(distance_to_group_reformatted) <- df_home_ranges_95$Group # change column names to each group
  units(distance_to_group_reformatted) <- NULL # Remove the meter unit of measurement from data
  distance_to_group_reformatted <- as_tibble(distance_to_group_reformatted)
  distance_to_group_reformatted <- bind_cols(Group = lead_waypoints$Group,distance_to_group_reformatted) %>%
  mutate(dist_to_own_group = NA)

  # Find each lead event's own group to match 
   index <- match(distance_to_group$Group,colnames(distance_to_group))
   distance_to_own_group <- as_tibble(distance_to_group$Group) %>%
     mutate(distance_to_border = NA) # create new column to use in loop below
  
  for(i in 1:length(index)) { # for each value of index...
    if (!is.na(index[i])) { # if not an NA value...
      if (distance_to_group[i,index[i]] != 0) { # if distance to own boundary does not equal ZERO...
        distance_to_own_group[i,2] <- distance_to_group[i,index[i]]*(-1) # Multiply by negative one to transform to negative value - as it's outside of boundary
      }
      if (distance_to_group[i,index[i]] == 0) { # If distance IS A ZERO... (ie, it's within its own territory).... 
        distance_to_own_group[i,2] <- distance_to_group_reformatted[i,index[i]] # get distance from within own territory to boundary, instead of a zero
      }
     }
  }
  
  # Select single column, the distance to its own border
   distance_to_own_group <- distance_to_own_group %>%
     select(distance_to_border)

  # Bind lead waypoints with the new distance column
  lead_waypoints <- cbind(lead_waypoints,distance_to_own_group) # bind to lead data
  
  # If we have burrow GPS coordinates, but no waypoints for a given group, there 
  # is no homerange to compare burrow coordinates to - so change to NA instead
  
  lead_groups <- lead_waypoints %>% distinct(Group) %>%
    sapply(as.character) # get group names of lead events
  waypoint_groups <- breeding_season[["waypoints"]] %>% distinct(Group) %>% 
    sapply(as.character) # get group names from waypoints
  
  # Which groups are in lead events, but NOT in waypoints?
  group_index <- which(!lead_groups %in% waypoint_groups)
  groups_to_remove <- lead_groups[group_index]
  
  # If there are groups to remove, change these group overlap values to NA
  if (!is_empty(groups_to_remove)) { 
  lead_waypoints <- lead_waypoints %>% mutate(in_overlap = ifelse(Group %in% groups_to_remove,NA,in_overlap),
                          in_core = ifelse(Group %in% groups_to_remove,NA,in_core))
  }
  
  
  ## Create map ----
  # Note - currently plotting all burrows. May want to remove where overlap is NA
  map <- ggmap(sorabi_map) +
    geom_point(aes(Longitude,Latitude,colour = Group),alpha=0.3,size=1,data = breeding_season[["waypoints"]]) + # raw waypoint data from GPS
    geom_point(aes(Longitude,Latitude,fill = Group),alpha=1,shape=23,stroke=2,size=3,show.legend = FALSE,
               data = lead_waypoints)  + # Burrow locations
    
    scale_colour_manual(values=group_colours[names(breeding_season[["list_of_groups"]])],
                        labels = str_replace_all(c(names(breeding_season[["list_of_groups"]])),"_"," ")) + # Set the colours to manual list of colours
    
    scale_fill_manual(values=group_colours[names(breeding_season[["list_of_groups"]])],
                      labels = str_replace_all(c(names(breeding_season[["list_of_groups"]])),"_"," ")) + # Set fill colour to manual list of colours
    
    geom_sf(data = df_home_ranges_95,mapping = aes(colour= Group),linetype = "dashed", lwd = 1.7,inherit.aes = FALSE,alpha = 0.0,show.legend=FALSE) + # plot 95 % home range
    
    geom_sf(data = df_home_ranges_50,mapping = aes(colour= Group), lwd = 1.7,inherit.aes = FALSE,alpha = 0.4,show.legend=FALSE) +# plot 50 % home range
    
    geom_sf_pattern(data = all_intersections,mapping = aes(),pattern="stripe",
                    pattern_fill = "black",inherit.aes = FALSE,
                    pattern_alpha = 0.6,pattern_density = 0.2,alpha= 0,
                    pattern_spacing = 0.02,show.legend=FALSE,line_width = 1) + # plot overlapping regions
    
    guides(colour = guide_legend(override.aes = list(size=6,alpha=1))) + # set legend 
    xlab("Longitude") + # change x axis label
    ylab("Latitude") + # change y axis label
    fig_theme # set manual theme from earlier
  
  # linetype = blank if want pattern blank
  
 output <- list(map,lead_waypoints)
 output <- setNames(output,c("map","lead_waypoints"))
}

# Split data for each season ----
nonbreeding_2013 <- split_mongoose_data(2013,final_waypoints,lead_events)
breeding_2013_2014 <- split_mongoose_data(breeding_season_list[[2]],final_waypoints,lead_events)
nonbreeding_2014 <- split_mongoose_data(2014,final_waypoints,lead_events)
breeding_2014_2015 <- split_mongoose_data(breeding_season_list[[3]],final_waypoints,lead_events)
nonbreeding_2015 <- split_mongoose_data(2015,final_waypoints,lead_events)
breeding_2015_2016 <- split_mongoose_data(breeding_season_list[[4]],final_waypoints,lead_events)
nonbreeding_2016 <- split_mongoose_data(2016,final_waypoints,lead_events)
breeding_2016_2017 <- split_mongoose_data(breeding_season_list[[5]],final_waypoints,lead_events)
nonbreeding_2017 <- split_mongoose_data(2017,final_waypoints,lead_events)
breeding_2017_2018 <- split_mongoose_data(breeding_season_list[[6]],final_waypoints,lead_events)
nonbreeding_2018 <- split_mongoose_data(2018,final_waypoints,lead_events)
breeding_2018_2019 <- split_mongoose_data(breeding_season_list[[7]],final_waypoints,lead_events)
nonbreeding_2019 <- split_mongoose_data(2019,final_waypoints,lead_events)
breeding_2019_2020 <- split_mongoose_data(breeding_season_list[[8]],final_waypoints,lead_events)
nonbreeding_2020 <- split_mongoose_data(2020,final_waypoints,lead_events)
breeding_2020_2021 <- split_mongoose_data(breeding_season_list[[9]],final_waypoints,lead_events)

## Put seasons into a list ----
breeding_seasons <- list(nonbreeding_2013, breeding_2013_2014,
                                 nonbreeding_2014, breeding_2014_2015,
                                 nonbreeding_2015, breeding_2015_2016,
                                 nonbreeding_2016, breeding_2016_2017,
                                 nonbreeding_2017, breeding_2017_2018,
                                 nonbreeding_2018, breeding_2018_2019,
                                 nonbreeding_2019, breeding_2019_2020,
                                 nonbreeding_2020, breeding_2020_2021)

## Create names for the list ----
breeding_seasons_names <- c("nonbreeding_2013", "breeding_2013_2014",
                                    "nonbreeding_2014", "breeding_2014_2015",
                                    "nonbreeding_2015", "breeding_2015_2016",
                                    "nonbreeding_2016", "breeding_2016_2017",
                                    "nonbreeding_2017", "breeding_2017_2018",
                                    "nonbreeding_2018", "breeding_2018_2019",
                                    "nonbreeding_2019", "breeding_2019_2020",
                                    "nonbreeding_2020", "breeding_2020_2021")

# Get map for each season ----
figures_breeding_seasons <- lapply(breeding_seasons,plot_map)
figures_breeding_seasons <- setNames(figures_breeding_seasons,breeding_seasons_names)

## Recombine each season to lead events ----
merged_lead_waypoints <- tibble()
length(figures_breeding_seasons)
for (i in 1:length(figures_breeding_seasons)) {
  extracted_lead_waypoints <- figures_breeding_seasons[[i]][["lead_waypoints"]] %>%
    mutate(season_year = names(figures_breeding_seasons)[i])
  merged_lead_waypoints <- rbind(merged_lead_waypoints,extracted_lead_waypoints)
}

# Summarise lead event data ----
waypoints_per_season <- lapply(breeding_seasons,function(x){nrow(x[["waypoints"]])})
waypoints_per_season <- setNames(waypoints_per_season,breeding_seasons_names)
waypoints_per_season <- as_tibble(waypoints_per_season)
waypoints_per_season <- waypoints_per_season %>%
  pivot_longer(everything(),names_to="Season") %>%
  rename(No_map_waypoints = value)

# First ensure order of season is preserved (otherwise count function reorders) 
merged_lead_waypoints$season_year <- factor(merged_lead_waypoints$season_year,levels=breeding_seasons_names)

## No. lead events per season ----
number_of_lead_events <- merged_lead_waypoints %>%
  group_by(season_year) %>%
  count() %>%
  ungroup() %>%
  select(n) %>%
  rename(Number_lead_events = n)

## No. lead events in overlap/core/both ----
territories <- c("overlap","core","overlap_and_core")
store_summary_data <- list()
for (i in 1:length(territories)) {
  summary_data <- merged_lead_waypoints %>%
    group_by(season_year,.drop = FALSE) %>%
    filter(territory_three_levels == territories[i]) %>%
    count() %>%
    ungroup %>%
    select(n)
  store_summary_data[[i]] <- summary_data
}

# Convert the list into dataframe
summary_of_territory <- do.call(cbind,store_summary_data)
colnames(summary_of_territory) <- territories # change names of columns

# Bind all data rows, add total count to last row
summary_of_lead_events <- cbind(waypoints_per_season, number_of_lead_events, summary_of_territory) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric),sum), # if numeric, sum the column
                      across(where(is.character),~"Total"))) # for character, add "Total"


# Update lead events data ----
# NOTE: few lead events not plotted in our breeding seasons (e.g. 2012, 2021), so these will be NA
# If not in core / overlap for territory categorical facor, they may be in the distance to border predictor

# Read in lead events again, this time NOT removing followers from data
lead_events_to_update <- read.csv("lead_events_with_coordinates.csv",header=TRUE) 
lead_events_to_update <- as_tibble(lead_events_to_update) %>%
  mutate(Date = as_date(Date)) %>% # format date correctly
  mutate(Season = str_replace(Season,"Non_breeding","Non-breeding"))

# Extract data from current lead events, update whole data with followers too
index <- match(lead_events_to_update$Lead_number,merged_lead_waypoints$Lead_number)
lead_events_to_update <- lead_events_to_update %>% 
  mutate(territory_two_levels = merged_lead_waypoints$territory_two_levels[index],
         territory_three_levels = merged_lead_waypoints$territory_three_levels[index],
         season_year = merged_lead_waypoints$season_year[index],
         distance_to_border = merged_lead_waypoints$distance_to_border[index])

# Insert new column: dominance and sex as 4 level factor in case use for analysis
lead_events_to_update <- lead_events_to_update %>%
  mutate(Status_sex = paste(Dominance_status,"_",Sex,sep=""))
lead_events_to_update$Status_sex

# Select only columns you want for analysis + extra info
lead_events_to_update <- lead_events_to_update %>%
  select(Lead_number,Date,Season,season_year,Group,Group_size,Individual,Sex,Lead_or_follow,Dominance_status,Status_sex,IGI_day_before,
         distance_to_border,territory_two_levels,territory_three_levels,Weight_PM_before,Weight_AM_day_of_move,Raw_weight_loss,Percent_weight_loss)

# Export data as csv
write.csv(lead_events_to_update,"lead_events_with_homerange.csv",row.names = FALSE)
