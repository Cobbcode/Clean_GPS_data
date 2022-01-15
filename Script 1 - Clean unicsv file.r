# Mongoose home range data ----

#PART 1: Cleaning data ----

# load packages ----
library(adehabitatHR)
library(tidyverse)
library(ggmap)
library(measurements)
library(stringr)
library(sf) # for conversion of spatial to sf 
library(lubridate)
library(rgeos)
library(cowplot)

# Read unicsv file ---- 
all_waypoints <- read.csv("merged_waypoints.unicsv", header = F,
                          col.names=paste0("V",seq_len(10)),fill=TRUE,na.strings=c(""))
# note:
# raw data is multiple unicsv files merged into one: each file may have a different no. of columns
# above we have specified column numbers to 10, to ensure all columns are red in without splitting rows
# we also replace any blank cells with NA values

# Clean data ----

all_waypoints <- as_tibble(all_waypoints) # convert data frame to tibble (which is similar to a data frame)
all_waypoints            # note each column is character, probably due to missing values, random text etc. in columns

# change the 10th column to the last value in each row of data, ie change column 10
# to the filename of the original file 

# on each row, apply the custom function - remove the NAs,then get the last entry of the row, which will always
# be the filename due to the way the merged unicsv file was created
all_waypoints[,10] <- apply(all_waypoints, 1, function(x) tail(na.omit(x), 1))

# now select first 4 columns, and the last 10th column
all_waypoints_cleaned <- all_waypoints %>% select(1:4,10)
colnames(all_waypoints_cleaned) <- c("Number","Latitude","Longitude","Waypoint_ID","Filename")

# now we need to get rid of rows that contain the headers of old excel files
# can just convert lat and long columns to numeric - if not numeric, it will turn into an NA

## Convert lat and long to numeric 
all_waypoints_cleaned <- all_waypoints_cleaned %>% mutate(Latitude = as.numeric(Latitude)) %>%
  mutate(Longitude = as.numeric(Longitude))

# use na.omit to remove rows with NA
all_waypoints_cleaned <- na.omit(all_waypoints_cleaned)
which(is.na(all_waypoints_cleaned)) # check NAs removed - should be "integer(0)"

# How many map files have been lost in conversion?
# number of gdb files in your maps folder: change path to wherever they are stored
path <-  "C:/Users/Ben/OneDrive - University of Bristol/PhD/A Chapter 2 and 3 - mongooses/Chapter 3 - long term data/MAPS 2021 - 09-18 - from Meg hdd/All groups for conversion"
original_file_number <- length(list.files(pattern = "*.gdb",path))

# How many have been lost reading in files?
original_file_number
unique_files <- unique(all_waypoints_cleaned$Filename)
number_of_files <- length(unique_files) #
original_file_number - number_of_files # how many files lost in conversion

# Remove extension of filenames that were converted in Windows
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  mutate(Filename = str_replace(Filename,"_converted.unicsv",""))


## Cleaning waypoint IDs ----
original_waypoint_number <- nrow(all_waypoints_cleaned)

all_waypoints_cleaned$Waypoint_ID # look at waypoint ID column

# Remove the characters within the square brackets in gsub: spaces, brackets, colons, question marks, full stops, commas, apostrophes
all_waypoints_cleaned$Waypoint_ID <- gsub("[ ()-/:?.,']","", all_waypoints_cleaned$Waypoint_ID)
all_waypoints_cleaned$Waypoint_ID

# create new column in data, by extracting string with the format: 4 numbers, followed by between 1 and 2 alphabetical characters
# the double \\ is needed, otherwise \ is read as normal text
# \\d = number
# [A-z] is letters a-z, both upper and lowercase included
# {1,2} indicates match between 1 and 2 letters after a given character specified
# using (SB|F|M) searches for SB OR F OR M to match
all_waypoints_cleaned$cleanedstring <- str_extract(all_waypoints_cleaned$Waypoint_ID, regex("\\d\\d\\d\\d(SB|F|M|I|L|SM|IGI)",ignore_case = TRUE))

# Then, do the same but this time only for 4 numbers - lots of waypoints don't have letter at the end but are still valid waypoints
all_waypoints_cleaned$cleanedstring2 <- str_extract(all_waypoints_cleaned$Waypoint_ID, regex("\\d\\d\\d\\d",ignore_case = TRUE))

# merge into cleanedstring column, if first waypoint ID search hasn't found anything
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  mutate(cleanedstring = if_else(!is.na(cleanedstring),cleanedstring,cleanedstring2)) %>%
  mutate(Time = str_extract(cleanedstring, regex("\\d\\d\\d\\d",ignore_case = TRUE))) %>% # create column with numbers only to later filter duplicates
  select(!cleanedstring2) # remove second column of cleaned waypoints

# remove any rows where a waypoint ID does not match the correct format, so where there is an NA value
all_waypoints_cleaned <- all_waypoints_cleaned %>% filter(!is.na(cleanedstring)) 

rows_of_data_pre_duplicate <- nrow(all_waypoints_cleaned) # number of rows in cleaned data so far
original_waypoint_number
rows_of_data_pre_duplicate


## Remove waypoint ID duplicates ----

# because of the way GPSBabel reads in gdb files, lots of duplicates between waypoints and waypoints in routes themselves
# take a look at rows with duplicate waypoint IDs WITHIN each filename:
duplicate_waypoints <- all_waypoints_cleaned %>% group_by(Filename) %>% # group by filename
  arrange(Number,.by_group = TRUE) %>% # Still within each filename, order by waypoint ID (so e.g. 0657F is first, 0657F1 is second, 0657F2 is third)
  filter(duplicated(Time)|duplicated(Time,fromLast=TRUE)) # filter based off the cleaned waypoint names - SHOW duplicates

# see duplicated waypoints
duplicate_waypoints

# Now we want to remove duplicate waypoint IDs within each filename, leaving one unique waypoint ID
all_waypoints_cleaned <- all_waypoints_cleaned %>% group_by(Filename) %>% # group by filename
  arrange(Number,.by_group = TRUE) %>% # Still within each filename, order by arbitrary waypoint number
  distinct(Time,Filename,.keep_all = TRUE) %>% # remove rows where the waypoint name is the same WITHIN a file, retaining the first row (hence sorting above)
  ungroup() # ungroup data again

rows_of_data_pre_duplicate # number of waypoints before most of cleaning
rows_of_data_removed_dups <- nrow(all_waypoints_cleaned) # check no. rows again
rows_of_data_removed_dups # removed lots of duplicates

# Remove files where there is only one  waypoint - assuming some are due to error
all_waypoints_cleaned <- all_waypoints_cleaned %>% group_by(Filename) %>%
  filter(n() > 1) %>%
  ungroup()

# Remove rows of data where our cleaned string column did not get a full match with a time and letter(s)
# AND where the number of characters of the original waypoint is more than 5 - tend to be bad waypoint names
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  mutate(remove_column = if_else(nchar(cleanedstring) == 4 & nchar(Waypoint_ID) > 5,"Remove","keep")) %>%
  filter(remove_column == "keep") %>%
  select(!remove_column) # remove select column


# Extract date from filename, correct wrong filenames

# Remove blank spaces from name
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename," ","") 

# Extract dates - 8 numbers in the name
all_waypoints_cleaned$Date <- str_extract(all_waypoints_cleaned$Filename, "\\d{8}") 

# check for filenames where the date hasn't been extracted properly ie where name is in an incorrect format
wrong_filenames <- all_waypoints_cleaned %>% arrange(Filename) %>% # check for filenames where date not extracted
  filter(is.na(Date)) %>% # filter NAs
  distinct(Filename) # filter duplicates out

wrong_filenames # look at wrong file names

# not too many wrong filenames - let's find them in the original untidy raw data
# and see if we can find the correct dates. Otherwise, find original gdb files in your folders, open the track, check date
all_waypoints %>% filter(str_detect(V10,wrong_filenames$Filename)) %>%
  distinct(V10,.keep_all=TRUE)

# replace old filenames with corrected dates, where possible
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"HP2020810BC-PM","HP20200810BC-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"LB2017008AL-AM","LB20170908AL-AM") 
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"LB2017008AL-PM","LB20170908AL-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"LB2019123GB-PM","LB20191123GB-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"LB2020715BC-AM","LB20200715BC-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SC140211AB-AM", "SC20140211AB-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SC2015058EA-AM","SC20150508EA-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SH201804S8JA-AM","SH20180428JA-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SH2019122ML-AM", "SH20191222ML-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"TS2020022ML-AM", "TS20200222ML-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"JB2000219BC-AM", "JB20200219BC-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SH2021073UPN-PM", "SH20210731UPN-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SH2020502LF-PM", "SH20210502LF-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SH2020610WG-PM", "SH20200610WG-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"TS2020519WG-PM", "TS20200519WG-PM")

# remove entries if unsure of date of GPS file
wrong_filenames # in this case, no diary of group visit that could correspond to "SC140204AB-PM", so removing entries with this filename
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!str_detect(Filename,"SC140204AB-PM")) %>%
  filter(!str_detect(Filename,"Waypoints_"))


# re run date extraction - should be no wrong filenames now
all_waypoints_cleaned$Date <- str_extract(all_waypoints_cleaned$Filename, "\\d{8}")
which(is.na(all_waypoints_cleaned$Date)) # should be zero NAs now - shows integer(0)

# convert date to correct date format
all_waypoints_cleaned$newdate <- as_date(all_waypoints_cleaned$Date)

# find dates that won't correctly parse - 46 for me
wrong_filenames <- all_waypoints_cleaned %>% arrange(Filename) %>% # check for filenames where date not extracted
  filter(is.na(newdate)) %>% # filter NAs
  distinct(Filename) # filter duplicates out

wrong_filenames # have a look

# let's filter raw data again, see if we can get correct dates from here
all_waypoints %>% filter(str_detect(V10,wrong_filenames$Filename))

# get just distinct values to make clearer
all_waypoints %>% filter(str_detect(V10,wrong_filenames$Filename))%>%
  distinct(V10,.keep_all=TRUE)

# replace filenames with corrections
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"BW20160931CG-AM","BW20160930CG-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"HP20141501HM-PM","HP20140115HM-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"HP20141601HM-AM","HP20140116HM-AM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"HP20141601HM-PM","HP20140116HM-PM")
all_waypoints_cleaned$Filename <- str_replace(all_waypoints_cleaned$Filename,"SG20181924EG-AM","SG20181024EG-AM")

# Remove this file: later on when filtering outliers, realise it contains wrong waypoints too:
all_waypoints_cleaned <- all_waypoints_cleaned %>% filter(Filename != "SC20142603HM-AM")

# re run date extraction
all_waypoints_cleaned$Date <- str_extract(all_waypoints_cleaned$Filename, "\\d{8}")
which(is.na(all_waypoints_cleaned$Date)) # should be zero NAs now

# convert date to correct date format again
all_waypoints_cleaned$newdate <- as_date(all_waypoints_cleaned$Date)
# note if any warnings - should be zero now if you've corrected all the bad filenames

# now extract time from cleaned waypoint column, convert to time format
all_waypoints_cleaned$Time <- as.POSIXct(all_waypoints_cleaned$Time,format="%H%M")
all_waypoints_cleaned$Time <- format(all_waypoints_cleaned$Time,"%H:%M")

# remove waypoints with times earlier than 5am or later than 7pm

all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(Time > "05:00" & Time < "19:00")


# Get group name for each row
all_waypoints_cleaned$Group <- str_extract(all_waypoints_cleaned$Filename,"^.{2}") # extract first two characters of filename - should be group initials
all_waypoints_cleaned$Group <- toupper(all_waypoints_cleaned$Group) # ensure all uppercase

# check group name initials:
levels(as.factor(all_waypoints_cleaned$Group))

# now replace those two characters starting filename with full group name
# we'll create a function for this so we can use it later on when cleaning the maps too
get_group_names <- function(data) {
  data <- data %>%
    mutate(Group = str_replace(Group,"BW","Bookworms")) %>%
    mutate(Group = str_replace(Group,"FE","Fish_eagles")) %>%
    mutate(Group = str_replace(Group,"GI","Giraffe")) %>%
    mutate(Group = str_replace(Group,"HP","Half_pints")) %>%
    mutate(Group = str_replace(Group,"JB","Jelly_babies")) %>%
    mutate(Group = str_replace(Group,"LB","Little_britains")) %>%
    mutate(Group = str_replace(Group,"LT","Looney_toons")) %>%
    mutate(Group = str_replace(Group,"SC","Scallywags")) %>%
    mutate(Group = str_replace(Group,"SG","Stargazers")) %>%
    mutate(Group = str_replace(Group,"SH","Shakespeares")) %>%
    mutate(Group = str_replace(Group,"TN","Terranova")) %>%
    mutate(Group = str_replace(Group,"TS","Teaspoons"))  %>%
    mutate(Group = str_replace(Group,"WE","West_enders"))
}

# use above function, get group_names from file initials
all_waypoints_cleaned <- get_group_names(all_waypoints_cleaned)

# check names again. Don't want to convert group name to a factor just yet
levels(as.factor(all_waypoints_cleaned$Group)) 

# Select columns, rename some
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  select(Number,Latitude,Longitude,cleanedstring,newdate,Time,Group,Filename) %>%
  rename(Waypoint_number = Number,Waypoint_name = cleanedstring, Date = newdate)
  
all_waypoints_cleaned %>% count(Group) # take a look at number of unique rows for each group

# Create list of intervals between breeding season dates
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

# if date is between a breeding season date, then returns TRUE, else FALSE if not
# so quickly tells you if breeding or non breeding season
all_waypoints_cleaned$Season <- all_waypoints_cleaned$Date %within% breeding_season_list 

# replace TRUE with breeding and FALSE with non breeding
all_waypoints_cleaned$Season <- ifelse(all_waypoints_cleaned$Season == TRUE,"Breeding","Non-breeding")
all_waypoints_cleaned

# check variables
str(all_waypoints_cleaned)
head(all_waypoints_cleaned)


# change variables to correct format
all_waypoints_cleaned$Waypoint_number <- as.numeric(all_waypoints_cleaned$Waypoint_number)
all_waypoints_cleaned$Season <- as.factor(all_waypoints_cleaned$Season)


## Cleaning GPS points ----

# Register API key to use google maps with ggmap
# Google API key:     ###################
register_google(key = ###################

# get kernel function from set of coordinates
get_kernel <- function(coordinates) {
  kernel <- kernelUD(coordinates)
  homerange_95 <- getverticeshr(kernel,percent = 95)
  homerange_sf_95 <- st_as_sf(homerange_95) # using sf library
  homerange_sf_95
}

# get map for Sorabi region
sorabi <- c(lon = 30.7825 ,lat = -24.205) # centered area quite well

# get the map from coordinates above
sorabi_map <- get_map(location = sorabi, zoom = 15, scale = 2,maptype = "satellite")




# Checking maps by year for outliers ----

# Function to re-get group names, and split all waypoint data into separate groups, storing in a single list
# this is useful for using after correcting outliers in the maps
split_waypoint_data <- function(waypoints) {
  waypoints$Group <- str_extract(waypoints$Filename,"^.{2}") # extract first two characters of filename - should be group initials
  waypoints$Group <- toupper(waypoints$Group) # ensure all uppercase
  waypoints <- get_group_names(waypoints)
  list_of_groups <- waypoints %>%
    group_by(Group) %>%
    group_split(.keep = TRUE)
  group_names <- sort(unique(waypoints$Group))
  list_of_groups <- setNames(list_of_groups,group_names)
}

# create a list of each group separated
list_of_groups <- split_waypoint_data(all_waypoints_cleaned)

#Remove_outliers function ---- 

## Function inputs:
# Input 1: A given group's tibble (dataframe)
# Input 2: number of outliers you want to remove

## Function outputs: one list, with three objects:
# "Figure" - a plot of before and after removing outliers
# "Updated_group" - the new dataframe/tibble, with outliers removed
# "Outliers" - a new dataframe/tibble, storing the outliers, allowing you to look at the file names
remove_outliers <- function(group_list,number_outliers) {
  
  # plot the group's map before removing outliers
  original_fig <- ggmap(sorabi_map) +
    geom_point(aes(Longitude,Latitude),color="Yellow",alpha=1,size=2,data=group_list)
  
  # create a mean coordinate from latitude and longitude
  mean_coordinate <- c(mean(group_list$Longitude),mean(group_list$Latitude))
  
  # Create distance column: get the distance from each coordinate to the mean coordinate
  group_list$distance_from_mean <- spDistsN1(cbind(group_list$Longitude,group_list$Latitude), pt = mean_coordinate)
  
  # Store the outliers in separate tibble
  store_outliers <- group_list %>%
    arrange(desc(distance_from_mean)) %>%
    filter(row_number() <= number_outliers) %>%
    select(any_of(c("Filename","Date","Time","Waypoint_name","Group","Burrow"))) %>%
    arrange(Date)
  
  # Remove outliers from the original tibble, remove the distance column
  group_list <- group_list %>% 
    arrange(desc(distance_from_mean)) %>%
    filter(row_number() > number_outliers) %>%
    select(!distance_from_mean) 

  # Create final figure after removing outliers
  final_fig <- ggmap(sorabi_map) +
    geom_point(aes(Longitude,Latitude),color="Yellow",alpha=1,size=2,data=group_list)

  # Plot figures side by side ,using library(cowplot)
  comparison_fig <- plot_grid(original_fig,final_fig)
  
  # Final output: a list with the figure, and new tibble
  output_list <- list(comparison_fig, group_list,store_outliers)
  output_list <- setNames(output_list,c("Figure","Updated_group","Outliers"))
}

#Use the remove_outlier function to check each group for outliers ----

# Check group names
names(list_of_groups)

# Outliers example: Bookworms ----

# Run the function, adding in how many outliers you want to remove: start with zero
output <- remove_outliers(list_of_groups[["Bookworms"]],0)
output # run this - see how we have stored 3 objects within one list, called "output"

# to access each object within a list, use [[]]
# let's access each of the objects in the list
output[["Figure"]]
output[["Updated_group"]]
output[["Outliers"]]

# This time let's remove 10 outliers:
output <- remove_outliers(list_of_groups[["Bookworms"]],5)
output[["Figure"]] 

# Note, with some coordinates, they might actually be correct coordinates - groups 
# sometimes do go out their territory as we know
# SO, we can check the outliers that was stored in the function with:
output[["Outliers"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE) # if you want to remove duplicate filenames, use this

## let's check these filenames - find them in your maps folder and/or check the date
# in Access - is there a diary of group visit? Did they mislabel the GPS file?
# BW20170721AL-PM - just two waypoints are wrong looking at the gdb file - can remove these
# BW20170705EE-AMPM - only one waypoint (0920) is wrong, checked the gdb file - remove this one
# BW20161025AMB-PM - unsure about this - REMOVE
# BW20170613EE-AMPM - one waypoint wrong - remove this
# BW20181017EG-AM - SEEMS OK - LEAVE THIS ONE IN
# BW20200729BCWG-AM - only one waypoint looks wrong on gdb file - remove
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!(Filename=="BW20170721AL-PM" & (Waypoint_name == "1425SB"|Waypoint_name =="1631SB"))) %>% # Remove two waypoints
  filter(!(Filename == "BW20170705EE-AMPM" & Waypoint_name == "0920F")) %>% # remove the one waypoint
  filter(Filename != "BW20161025AMB-PM") %>%
  filter(!(Filename == "BW20170613EE-AMPM" & Waypoint_name == "0910M")) %>% # remove the one waypoint
  filter(!(Filename == "BW20200729BCWG-AM" & Waypoint_name == "1004F")) %>%
  mutate(Filename = str_replace(Filename,"BW20200608WG-PM","SH20200608WG-PM"))
  
# Ok, let's re run our outlier function with zero outliers - does it look ok now?

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Bookworms"]],0)

output[["Figure"]] # looks good - note the two bottom left ones are NOT outliers as we checked earlier
output[["Updated_group"]]
output[["Outliers"]]


# Fish eagles outliers ----
output <- remove_outliers(list_of_groups[["Fish_eagles"]],24)
output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE)

## Files to check
# FE20140715SLB-AMPM - only one waypoint wrong - remove 1240SB
# FE20141126BD-PM - one waypoint wrong. Remove 1513F
# FE20150604AMD-PM - should be SC
# FE20161109AC-AM - can't find Diary of group entry for this - REMOVE
# FE20170307EE-PM - should be Bookworms - change filename
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!(Filename == "FE20140715SLB-AMPM" & Waypoint_name =="1240SB")) %>%
  filter(!(Filename == "FE20141126BD-PM" & Waypoint_name =="1513F")) %>%
  mutate(Filename = str_replace(Filename,"FE20150604AMD-PM","SC20150604AMD-PM")) %>%
  filter(Filename != "FE20161109AC-AM") %>% # REMOVED
  mutate(Filename = str_replace(Filename,"FE20170307EE-PM","BW20170307EE-PM"))
  
list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Fish_eagles"]],0)
output[["Figure"]] # looks good!

# Giraffe outliers ----
output <- remove_outliers(list_of_groups[["Giraffe"]],112) # Giraffe has a lot of wrong files...
output[["Figure"]]
output[["Outliers"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE) # if you want to remove duplicate filenames, use this

## Wrong filenames:
# GI20151014CC-AM is actually a JB route - but this already exists in Jellie's data - REMOVE
# GI20161005CG-AM unsure where this is from - REMOVE
# All of AF filenames here should be Shakespeares, not Giraffe (DoGV entry, misnamed files)
# GI20190929NT-AM actually seems ok - group split this day, not too unusual an outlier

all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(Filename != "GI20151014CC-AM") %>% # REMOVED this file
  filter(Filename != "GI20161005CG-AM") %>% # REMOVED this file
  mutate(Filename = str_replace_all(Filename,c(
                  "GI20190731AF-AM" = "SH20190731AF-PM",
                  "GI20190731AF-PM" = "SH20190731AF-PM",
                  "GI20190801AF-AM" = "SH20190801AF-PM",
                  "GI20190801AF-PM" = "SH20190802AF-AM",
                  "GI20190802AF-AM" = "SH20190802AF-AM",
                  "GI20190802AF-PM" = "SH20190802AF-PM")))

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Giraffe"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good!

# Half Pints outliers ---- 
output <- remove_outliers(list_of_groups[["Half_pints"]],135) # 135 ish potential outliers - probably too strict, we'll check files anyway
output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE) %>%
  print(n=nrow(output$Outliers)) # print all rows

## File names
# HP20140117AB-AM - seems ok?
# HP20141127BD-AM - sleeping burrow waypoint seems wrong, 1km from rest
# HP20151103AL-PM - wrong waypoints, REMOVE
# HP20151125AB-PM - wrong location, all at base - REMOVE
# HP20160408AB-AM - seems ok
# HP20160818AB-AMPM - waypoint 09:05 wrong, at base
# HP20161206AMB-AM - waypoint 0649SB wrong, at base
# HP20170622JK-AMPM - waypoint wrong, at base
# HP20170624JK-AMPM - waypoint wrong, at base
# HP20170630HS-PM - half waypoints wrong, REMOVE
# HP20181009HR-AMPM - seems ok
# HP20181117EG-PM - remove 1515I wrong waypoint
# HP20181122KH-AM - 0700F wrong waypoint, remove
# HP20190514JL-AMPM - seems ok
# HP20190530NT-AMPM - seems ok
# HP20190802IS-PM - seems ok
# HP20190803IS-PM - toward the fence, but seems ok
# HP20190803IS-AM - toward fence, but seems ok again
# HP20190804IS-AM - toward fence, again seems ok
# HP20191009NT-AM - seems ok
# HP20191011NT-PM - seems ok
# HP20191014NT-PM - seems ok
# HP20191018GB-AM - seems ok
# HP20191018GB-PM - seems ok
# HP20200512BC-PM - seems ok
# HP20200808BC-AMPM - seems ok
# HP20210107SN-AM - changed to JB
# HP20210331SN-AM - at base - remove.
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!(Filename == "HP20141127BD-AM" & Waypoint_name == "0630SB")) %>%
  filter(!Filename == "HP20151103AL-PM") %>%
  filter(!Filename =="HP20151125AB-PM") %>%
  filter(!(Filename == "HP20160818AB-AMPM" & Waypoint_name == "0905SB")) %>%
  filter(!(Filename == "HP20161206AMB-AM" & Waypoint_name == "0649SB"))  %>%
  filter(!(Filename == "HP20170622JK-AMPM" & Waypoint_name == "0907SB"))  %>%
  filter(!(Filename == "HP20170624JK-AMPM" & Waypoint_name == "0907SB"))  %>%
  filter(!Filename =="HP20170630HS-PM") %>%
  filter(!(Filename =="HP20181117EG-PM" & Waypoint_name == "1515I")) %>%
  filter(!(Filename == "HP20181122KH-AM" & Waypoint_name == "0700F")) %>%
  filter(!(Filename == "HP20210331SN-AM" & Waypoint_name == "0736F")) %>%
  mutate(Filename = str_replace(Filename,"HP20210107SN-AM","JB20210107SN-AM"))
  
  
  
list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Half_pints"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good! 

  
# Jelly Babies ----
output <- remove_outliers(list_of_groups[["Jelly_babies"]],65) # 60 ish
output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE)

# JB20140118KR-PM wrong location - remove
# JB20140422AB-PM should be Shakespeares
# JB20140528JK-AM should be Shakespeares
# JB20140711PL-AMPM - waypoints before 1300 are wrong - remove
# JB20170315EH-PM - few wrong - REMOVE
# JB20171211TM-AM - should be Bookworms
# JB20171211TM-PM - should be Bookworms
# JB20180418ER-PM - seems ok

# Format new timtime to remove certain waypoints

all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!Filename == "JB20140118KR-PM") %>%
  mutate(Filename = str_replace(Filename,"JB20140422AB-PM","SH20140422AB-PM")) %>%
  mutate(Filename = str_replace(Filename,"JB20140528JK-AM","SH20140528JK-AM")) %>%
  filter(!(Filename == "JB20140711PL-AMPM" & Time < "13:00")) %>%
  filter(!Filename == "JB20170315EH-PM") %>%
  mutate(Filename = str_replace(Filename,"JB20171211TM-AM","BW20171211TM-AM")) %>%
  mutate(Filename = str_replace(Filename,"JB20171211TM-PM","BW20171211TM-PM"))

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Jelly_babies"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good! 


# Little Britains outliers ---- 
output <- remove_outliers(list_of_groups[["Little_britains"]],35) # filter 7 outliers
output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE)

# LB20171108EG-AM - 0600 waypoint wrong
# LB20200717BC-AM - waypoints after 10:30 are wrong - remove these
# LB20171012TM-PM - is fine
# LB20201210AD-PM - is both BW and LB waypoints in same file - remove
# LB20201210AD-AM
all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!(Filename == "LB20171108EG-AM" & Waypoint_name == "0600SB")) %>%
  filter(!(Filename == "LB20200717BC-AM" & Time > "10:30")) %>%
  filter(Filename != "LB20201210AD-PM") %>%
  filter(Filename != "LB20201210AD-AM") %>%
  filter(!(Filename == "LB20210529LW-PM" & Waypoint_name == "1200I"))

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Little_britains"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good! 


# Looney toons outliers ---- 
output <- remove_outliers(list_of_groups[["Looney_toons"]],20) # 20 outliers
output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE)

# LT20131119CC-PM should  be Jelly babies
# LT20131119CC-AM should be Jelly babies
# LT20131120CC-AM should be Jelly babies

all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!Filename == "JB20140118KR-PM") %>%
  mutate(Filename = str_replace(Filename,"LT20131119CC-PM","JB20131119CC-PM")) %>%
  mutate(Filename = str_replace(Filename,"LT20131119CC-AM","JB20131119CC-AM")) %>%
  mutate(Filename = str_replace(Filename,"LT20131120CC-AM","JB20131120CC-AM"))

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Looney_toons"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good! 

# Scally wags outliers ----
output <- remove_outliers(list_of_groups[["Scallywags"]],78)
output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE)

## Wrong files
# SC20140731JK-AMPM - should be Shakespeares
# SC20150926PM-JK - should be Jelly Babies
# SC20150625JK-PM - should be Shakespeares
# SC20150627JK-AM - should be Shakespeares
# SC20150630KR-AMPM - should be Shakespeares

all_waypoints_cleaned <- all_waypoints_cleaned %>%
  filter(!Filename == "JB20140118KR-PM") %>%
  mutate(Filename = str_replace(Filename,"SC20140731JK-AMPM","SH20140731JK-AMPM")) %>%
  mutate(Filename = str_replace(Filename,"SC20150926PM-JK","JB20150926PM-JK")) %>%
  mutate(Filename = str_replace(Filename,"SC20150625JK-PM","SH20150625JK-PM")) %>%
  mutate(Filename = str_replace(Filename,"SC20150627JK-AM","SH20150627JK-AM")) %>%
  mutate(Filename = str_replace(Filename,"SC20150630KR-AMPM","SH20150630KR-AMPM"))
  
  
list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Scallywags"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good! 


# Shakespeares outliers ----
output <- remove_outliers(list_of_groups[["Shakespeares"]],127) # 127 obvious outliers
output[["Figure"]]
output[["Outliers"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE) # if you want to remove duplicate filenames, use this

## Shakespeare wrong filenames
# SH20130325EB-AM doesn't match up to dogv - REMOVE
# SH20140918JK-AM - some waypoints wrong - Correct
# SH20141001JK-PM - is actually from Scally Wags SC - CORRECT
# SH20151007CC-PM - is also a Scally Wags SC track - CORRECT
# SH20160414JB-PM - unsure correct group - REMOVE
# SH20171003TM-AM - unsure correct group - REMOVE
# SH20180611ER-AMPM - should  be Jelly Babies - CORRECT
# SH20180908HR-AMPM - only a few waypoints are wrong - Correct
# SH20190325MH-AM - should be Star Gazers - CORRECT
# SH20200731BC-PM - half of waypoints wrong - correct

all_waypoints_cleaned <- all_waypoints_cleaned %>% 
  filter(Filename != "SH20130325EB-AM") %>% 
  filter(!(Filename == "SH20140918JK-AM" & Longitude > 30.78)) %>%
  filter(Filename != "SH20160414JB-PM") %>%
  filter(Filename != "SH20171003TM-AM") %>%
  filter(!(Filename == "SH20180908HR-AMPM" & Longitude > 30.78)) %>%
  filter(!(Filename == "SH20200731BC-PM" & Longitude > 30.78)) %>%
  mutate(Filename = str_replace(Filename,"SH20141001JK-PM","SC20141001JK-PM")) %>% # replacing filenames
  mutate(Filename = str_replace(Filename,"SH20151007CC-PM","SC20151007CC-PM")) %>%
  mutate(Filename = str_replace(Filename,"SH20180611ER-AMPM","JB20180611ER-AMPM")) %>%
  mutate(Filename = str_replace(Filename,"SH20190325MH-AM","SG20190325MH-AM"))

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Shakespeares"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good!

# Star gazers outliers ----
output <- remove_outliers(list_of_groups[["Stargazers"]],1) # 1 obvious outlier
output[["Figure"]]
output[["Outliers"]]

## Wrong waypoints
# SG20190303RT-AM - only one waypoint wrong: 1000
all_waypoints_cleaned <- all_waypoints_cleaned %>% 
  filter(!(Filename == "SG20190303RT-AM" & Waypoint_name == "1000"))

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Stargazers"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good!


# Teaspoons outliers ---- 
output <- remove_outliers(list_of_groups[["Teaspoons"]],55) # check the edges too
output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE)

## Files
# TS20180108EG-PM should be Jelly Babies. AM already exists, PM doesn't
# TS20181024FB-PM should be Shakespeares
# TS20181026EG-AM should be Star Gazers
# TS20190629AMD-PM - seems ok
# TS20190924IS-AM should be Shakespeares
# TS20190924IS-PM should be Shakespeares
# TS20210329SN-PM should be Half Pints

all_waypoints_cleaned <- all_waypoints_cleaned %>% 
  mutate(Filename = str_replace(Filename,"TS20180108EG-PM","JB20180108EG-PM")) %>%
  mutate(Filename = str_replace(Filename,"TS20181024FB-PM","SH20181024FB-PM")) %>%
  mutate(Filename = str_replace(Filename,"TS20181026EG-AM","SG20181026EG-AM")) %>%
  mutate(Filename = str_replace(Filename,"TS20190924IS-AM","SH20190924IS-AM")) %>%
  mutate(Filename = str_replace(Filename,"TS20190924IS-PM","SH20190924IS-PM")) %>%
  mutate(Filename = str_replace(Filename,"TS20210329SN-PM","HP20210329SN-PM"))

list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Teaspoons"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good!


# Terranova outliers ---- 
output <- remove_outliers(list_of_groups[["Terranova"]],5) # check 5 outliers
output[["Figure"]]
output[["Outliers"]]

all_waypoints_cleaned <- all_waypoints_cleaned %>% 
  mutate(Filename = str_replace(Filename,"TN20201126AD-AM","HP20201126AD-AM"))

# Re check map with updated data
list_of_groups <- split_waypoint_data(all_waypoints_cleaned) # split groups with updated data
output <- remove_outliers(list_of_groups[["Terranova"]],0) # replot data, remove zero outliers 
output[["Figure"]] # looks good!
  
  
# West enders outliers ---- 
output <- remove_outliers(list_of_groups[["West_enders"]],10) # check a few extremities

output[["Figure"]]
output[["Outliers"]] %>% distinct(Filename,.keep_all = TRUE)

## Any wrong data?
# WE20130204JK-AM looks fine
# WE20130320JK-AM looks fine


# Re get group names for the waypoints data
all_waypoints_cleaned$Group <- str_extract(all_waypoints_cleaned$Filename,"^.{2}") # extract first two characters of filename - should be group initials
all_waypoints_cleaned$Group <- toupper(all_waypoints_cleaned$Group) # ensure all uppercase
all_waypoints_cleaned <- get_group_names(all_waypoints_cleaned)

# Plot all groups across years ---- 
fig_all_groups <- ggmap(sorabi_map) +
  geom_point(aes(Longitude,Latitude),color="Yellow",alpha=1,size=1.2,data=all_waypoints_cleaned) +
  facet_wrap(~Group)
fig_all_groups



# Export cleaned waypoints into csv file ----
# REMEMEBER: If you ever use excel to open, it removes the leading zeros from burrow IDs e.g. "0650" to "650"
write.csv(all_waypoints_cleaned,"Cleaned_waypoints.csv",row.names = FALSE)

# PART 2: cleaning all waypoints map ----

# Can run part 2 independently of above

# load packages ----
library(adehabitatHR)
library(tidyverse)
library(ggmap)
library(measurements)
library(stringr)
library(sf) # for conversion of spatial to sf 
library(lubridate)
library(rgeos)
library(cowplot)

# Read and format data ----
final_waypoints <- read.csv("cleaned_waypoints.csv",header = TRUE)
final_waypoints <- as_tibble(final_waypoints)
final_waypoints <- final_waypoints %>%
  mutate(Group = factor(Group)) %>% # ensure group is a factor
  mutate(Date = as_date(Date)) %>% # format date correctly
  mutate(Season = factor(Season))

# Read in waypoints data - all burrow 
waypoint_map <- read.csv("waypoint_maps.unicsv",header=TRUE)
waypoint_map <- as_tibble(waypoint_map)

# Select first 4 columns, and the last 10th column, name them
waypoint_map <- waypoint_map %>% select(2:4,12)
colnames(waypoint_map) <- c("Latitude","Longitude","Waypoint_name","Filename")

# We converted three gdb files into one; so it will contain several column headers in the data
# We want to remove these rows:
# Convert lat, lon, and waypoint name to numeric; anything else will become NA
waypoint_map <- waypoint_map %>% 
  mutate(Latitude = as.numeric(Latitude),
                                        Longitude = as.numeric(Longitude),
                                        Waypoint_name = as.integer(Waypoint_name)) %>%
  distinct(Waypoint_name,.keep_all = TRUE) # Remove duplicate waypoint numbers

# use na.omit to remove rows with NA
waypoint_map <- na.omit(waypoint_map)
which(is.na(waypoint_map)) # check NAs removed - should be "integer(0)"


# Read in, format lead events data ----
lead_events <- read.table("dogv_updated_in_r.txt",header = TRUE)
lead_events <- as_tibble(lead_events)
lead_events <- lead_events %>% 
  mutate(Group = str_to_sentence(Group), # change Group to sentence case
         Longitude = NA,
         Latitude = NA,
         Longitude2 = NA,
         Latitude2 = NA)


# Create new tibble of SB waypoints in the morning
SB_waypoints <- final_waypoints %>%
  filter(str_detect(Waypoint_name,regex("SB",ignore_case = TRUE)) & Time < "12:00")

# If a given lead event is missing a burrow name, search our morning SBs data: if a group + date matches a morning SB,
# extract that waypoint's long and latitude
lead_events <- lead_events %>%
  mutate(temp = is.na(Burrow), # Create new temporary column "temp" - if burrow is an NA value, returns TRUE
         Longitude = ifelse(temp,match(paste(Date,Group),paste(SB_waypoints$Date,SB_waypoints$Group)), Longitude), # If TRUE (missing burrow)
         Latitude = ifelse(temp,match(paste(Date,Group),paste(SB_waypoints$Date,SB_waypoints$Group)), Latitude)) # match lead events to SB waypoints data

# where there is a match which returns the position of the match, index the SB data to extract long and latitude
lead_events <- lead_events %>%
  mutate(Longitude = ifelse(temp,SB_waypoints$Longitude[lead_events$Longitude],Longitude),
         Latitude = ifelse(temp,SB_waypoints$Latitude[lead_events$Latitude],Latitude))


# Repeat, but this time for data where there is a burrow number: extract sleeping burrow long and latitude, into lead event data
lead_events <- lead_events %>% mutate(
  Longitude2 = ifelse(temp,Longitude2,match(paste(Burrow),paste(waypoint_map$Waypoint_name))),
  Latitude2 = ifelse(temp,Latitude2,match(paste(Burrow),paste(waypoint_map$Waypoint_name))))
  
lead_events <- lead_events %>%
  mutate(Longitude2 = ifelse(temp,Longitude2,waypoint_map$Longitude[lead_events$Longitude2]),
         Latitude2 = ifelse(temp,Latitude2,waypoint_map$Latitude[lead_events$Latitude2]))

lead_events <- lead_events %>%
  mutate(Longitude = ifelse(is.na(Longitude),Longitude2,Longitude),
         Latitude = ifelse(is.na(Latitude),Latitude2,Latitude)) %>%
           select(!c("Longitude2","Latitude2","temp"))

# Check for any missing longitude / latitude values
Missing_burrows <- lead_events %>% filter(is.na(Longitude)|is.na(Latitude)) %>%
  distinct(Lead_number,.keep_all = TRUE)
View(Missing_burrows)

View(lead_events)

# Plot burrow, check for outliers?

# Split data
split_lead_data <- function(lead_events) {
lead_event_groups <- lead_events %>%
  group_by(Group) %>%
  group_split(.keep = TRUE)

group_names <- sort(unique(lead_events$Group))
lead_event_groups <- setNames(lead_event_groups,group_names)
lead_event_groups
}

# Check outliers for burrow lead events ----
lead_event_groups <- split_lead_data(lead_events)
names(lead_event_groups)

# Plot one by one, each group
figure_list <- list()
for (i in 1:length(names(lead_event_groups))) {
  output <- remove_outliers(lead_event_groups[[i]],0)
  figure_list[[i]] <- output[["Figure"]]
  print(output[["Figure"]])
}
names(lead_event_groups)
# Doesn't seem to be any outliers
figure_list

## Export lead events with coordinates
write.csv(lead_events,"lead_events_with_coordinates.csv",row.names = FALSE)
