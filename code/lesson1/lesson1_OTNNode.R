# Intro to R for Telemetry Summaries --------

# Installs and Setup --------
library(tidyverse)# really neat collection of packages! https://www.tidyverse.org/
library(lubridate)
library(readxl)
library(viridis)
library(plotly)
library(ggmap)

setwd('YOUR/PATH/TO/data/otn') #set folder you're going to work in
getwd() #check working directory

# Creating Summary Reports: Importing --------

## Tag Matches ----

nsbs_matched_2021 <- read_csv("nsbs_matched_detections_2021.zip") #Import 2021 detections
nsbs_matched_2022 <- read_csv("nsbs_matched_detections_2022.zip") # Import 2022 detections
nsbs_matched_full <- rbind(nsbs_matched_2021, nsbs_matched_2022) #Now join the two dataframes
# release records for animals often appear in >1 year, this will remove the duplicates
nsbs_matched_full <- nsbs_matched_full %>% distinct() # Use distinct to remove duplicates. 

## Array Matches ----
hfx_qual_2021 <- read_csv("hfx_qualified_detections_2021_workshop.csv")
hfx_qual_2022 <- read_csv("hfx_qualified_detections_2022_workshop.csv") 
hfx_qual_21_22_full <- rbind(hfx_qual_2021, hfx_qual_2022) 

## Tagging and Deployment Metadata  ----

# Deployment Metadata
hfx_deploy <- read_excel("hfx_sample_deploy_metadata_export.xlsx", skip=3) #can also do argument "sheet = XXX" if needed
View(hfx_deploy)

# Tag metadata
nsbs_tag <- read_excel("nsbs_sample_tag_metadata_export.xlsx")
View(nsbs_tag)

#keep in mind the timezone of the columns

# Creating Summary Reports: Array Operators --------

## Array Map - Static ----

library(ggmap)

names(hfx_deploy)

base <- get_stamenmap(
  bbox = c(left = min(hfx_deploy$DEPLOY_LONG), 
           bottom = min(hfx_deploy$DEPLOY_LAT), 
           right = max(hfx_deploy$DEPLOY_LONG), 
           top = max(hfx_deploy$DEPLOY_LAT)),
  maptype = "toner-lite", 
  crop = FALSE,
  zoom = 5)

#filter for stations you want to plot - this is very customizable

hfx_deploy_plot <- hfx_deploy %>% 
  mutate(deploy_date=ymd_hms(`DEPLOY_DATE_TIME   (yyyy-mm-ddThh:mm:ss)`)) %>% #make a datetime
  mutate(recover_date=ymd_hms(`RECOVER_DATE_TIME (yyyy-mm-ddThh:mm:ss)`)) %>% #make a datetime
  filter(!is.na(deploy_date)) %>% #no null deploys
  filter(deploy_date > '2020-07-03' | recover_date < '2022-01-11') %>% #only looking at certain deployments, can add start/end dates here
  group_by(STATION_NO) %>% 
  summarise(MeanLat=mean(DEPLOY_LAT), MeanLong=mean(DEPLOY_LONG)) #get the mean location per station, in case there is >1 deployment


#add your stations onto your basemap

hfx_map <- 
  ggmap(base, extent='panel') + 
  ylab("Latitude") +
  xlab("Longitude") +
  geom_point(data = hfx_deploy_plot, #filtering for recent deployments
             aes(x = MeanLong,y = MeanLat), #specify the data, colour = STATION_NO is also neat here
             shape = 19, size = 2) #lots of aesthetic options here!

#view your receiver map!
hfx_map

#save your receiver map into your working directory

ggsave(plot = hfx_map, filename = "hfx_map.tiff", units="in", width=15, height=8) 

#can specify location, file type and dimensions

## Array Map - Interactive ----
#set your basemap

geo_styling <- list(
  scope = 'nova scotia',
  fitbounds = "locations", visible = TRUE, #fits the bounds to your data!
  showland = TRUE,
  showlakes = TRUE,
  lakecolor = toRGB("blue", alpha = 0.2), #make it transparent
  showcountries = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray85")
)

#decide what data you're going to use. Let's use hfx_deploy_plot, which we created above for our static map.

hfx_map_plotly <- plot_geo(hfx_deploy_plot, lat = ~MeanLat, lon = ~MeanLong)  

#add your markers for the interactive map

hfx_map_plotly <- hfx_map_plotly %>% add_markers(
  text = ~paste(STATION_NO, MeanLat, MeanLong, sep = "<br />"),
  symbol = I("square"), size = I(8), hoverinfo = "text" 
)

#Add layout (title + geo stying)

hfx_map_plotly <- hfx_map_plotly %>% layout(
  title = 'HFX Deployments<br />(> 2020-07-03)', geo = geo_styling 
)

#View map

hfx_map_plotly

## Summary of Animals Detected ----
# How many of each animals did we detect from each collaborator, by species, per station

hfx_qual_summary <- hfx_qual_21_22_full %>% 
  filter(datecollected > '2021-06-01') %>% #select timeframe, stations etc.
  group_by(trackercode, station, tag_contact_pi, tag_contact_poc) %>% 
  summarize(count = n()) %>% 
  dplyr::select(trackercode, tag_contact_pi, tag_contact_poc, station, count)

#view our summary table

view(hfx_qual_summary) 

#export our summary table

write_csv(hfx_qual_summary, "hfx_summary.csv", col_names = TRUE)

## Summary of Detections ----

# number of detections per month/year per station 

hfx_det_summary  <- hfx_qual_21_22_full  %>% 
  mutate(datecollected=ymd_hms(datecollected))  %>% 
  group_by(station, year = year(datecollected), month = month(datecollected)) %>% 
  summarize(count =n())

hfx_det_summary 

# Create a new data product, det_days, that give you the unique dates that an animal was seen by a station
stationsum <- hfx_qual_21_22_full %>% 
  group_by(station) %>%
  summarise(num_detections = length(datecollected),
            start = min(datecollected),
            end = max(datecollected),
            uniqueIDs = length(unique(fieldnumber)), 
            det_days=length(unique(as.Date(datecollected))))
View(stationsum)


## Plot of Detections ----

hfx_qual_21_22_full %>%  
  mutate(datecollected=ymd_hms(datecollected)) %>% #make datetime
  mutate(year_month = floor_date(datecollected, "months")) %>% #round to month
  group_by(year_month) %>% #can group by station, species etc.
  summarize(count =n()) %>% #how many dets per year_month
  ggplot(aes(x = (month(year_month) %>% as.factor()), 
             y = count, 
             fill = (year(year_month) %>% as.factor())
  )
  )+ 
  geom_bar(stat = "identity", position = "dodge2")+ 
  xlab("Month")+
  ylab("Total Detection Count")+
  ggtitle('HFX Animal Detections by Month')+ #title
  labs(fill = "Year") #legend title


# Creating Summary Reports: Taggers --------

## New Dataframe ----
#optional dataset to use: detections with releases filtered out!

nsbs_matched_full_no_release <- nsbs_matched_full  %>% 
  filter(receiver != "release")

## Detection/Release Map - Static ----
base <- get_stamenmap(
  bbox = c(left = min(nsbs_matched_full_no_release$longitude),
           bottom = min(nsbs_matched_full_no_release$latitude), 
           right = max(nsbs_matched_full_no_release$longitude), 
           top = max(nsbs_matched_full_no_release$latitude)),
  maptype = "toner-lite", 
  crop = FALSE,
  zoom = 5)


#add your releases and detections onto your basemap

nsbs_map <- 
  ggmap(base, extent='panel') +
  ylab("Latitude") +
  xlab("Longitude") +
  geom_point(data = nsbs_matched_full_no_release, 
             aes(x = longitude,y = latitude), #specify the data
             colour = 'blue', shape = 19, size = 2) #lots of aesthetic options here!

#view your tagging map!

nsbs_map

## Detection/Release Map - Interactive ----
#set your basemap

geo_styling <- list(
  fitbounds = "locations", visible = TRUE, #fits the bounds to your data!
  showland = TRUE,
  showlakes = TRUE,
  lakecolor = toRGB("blue", alpha = 0.2), #make it transparent
  showcountries = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray85")
)

#decide what data you're going to use

detections_map_plotly <- plot_geo(nsbs_matched_full_no_release, lat = ~latitude, lon = ~longitude) 

#add your markers for the interactive map
detections_map_plotly <- detections_map_plotly %>% add_markers(
  text = ~paste(catalognumber, commonname, paste("Date detected:", datecollected), 
                paste("Latitude:", latitude), paste("Longitude",longitude), 
                paste("Detected by:", detectedby), paste("Station:", station), 
                paste("Project:",collectioncode), sep = "<br />"),
  symbol = I("square"), size = I(8), hoverinfo = "text" 
)

#Add layout (title + geo stying)

detections_map_plotly <- detections_map_plotly %>% layout(
  title = 'NSBS Detections', geo = geo_styling
)

#View map
detections_map_plotly

## Summary of Tagged Animals ----

# summary of animals you've tagged

nsbs_tag_summary <- nsbs_tag %>% 
  mutate(UTC_RELEASE_DATE_TIME = ymd_hms(UTC_RELEASE_DATE_TIME)) %>% 
  #filter(UTC_RELEASE_DATE_TIME > '2016-06-01') %>% #select timeframe, specific animals etc.
  group_by(year = year(UTC_RELEASE_DATE_TIME), COMMON_NAME_E) %>% 
  summarize(count = n(), 
            Meanlength = mean(`LENGTH (m)`, na.rm=TRUE), 
            minlength= min(`LENGTH (m)`, na.rm=TRUE), 
            maxlength = max(`LENGTH (m)`, na.rm=TRUE), 
            MeanWeight = mean(`WEIGHT (kg)`, na.rm = TRUE)) 


#view our summary table

View(nsbs_tag_summary)

## Detection Attributes ----
# Average location of each animal, without release records

nsbs_matched_full_no_release %>% 
  group_by(catalognumber) %>% 
  summarize(NumberOfStations = n_distinct(station),
            AvgLat = mean(latitude),
            AvgLong =mean(longitude))

#Now lets try to join our metadata and detection extracts.
#First we need to make a tagname column in the tag metadata (to match the Detection Extract), and figure out the enddate of the tag battery.

nsbs_tag <- nsbs_tag %>% 
  mutate(enddatetime = (ymd_hms(UTC_RELEASE_DATE_TIME) + days(EST_TAG_LIFE))) %>% #adding enddate
  mutate(tagname = paste(TAG_CODE_SPACE,TAG_ID_CODE, sep = '-')) #adding tagname column

#Now we join by tagname, to the detection dataset (without the release information)

tag_joined_dets <- left_join(x = nsbs_matched_full_no_release, y = nsbs_tag, by = "tagname") #join!

#make sure any redeployed tags have matched within their deployment period only

tag_joined_dets <- tag_joined_dets %>% 
  filter(datecollected >= UTC_RELEASE_DATE_TIME & datecollected <= enddatetime)

View(tag_joined_dets)

#Lets use this new joined dataframe to make summaries!
#Avg length per location

nsbs_tag_det_summary <- tag_joined_dets %>% 
  group_by(detectedby, station, latitude, longitude)  %>%  
  summarise(AvgSize = mean(`LENGTH (m)`, na.rm=TRUE))

View(nsbs_tag_det_summary)

#export our summary table as CSV
write_csv(nsbs_tag_det_summary, "detections_summary.csv", col_names = TRUE)

# count detections per transmitter, per array

nsbs_matched_full_no_release %>% 
  group_by(catalognumber, station, detectedby, commonname) %>% 
  summarize(count = n()) %>% 
  dplyr::select(catalognumber, commonname, detectedby, station, count)

# list all arrays each fish was seen on, and a number_of_arrays column too

arrays <- nsbs_matched_full_no_release %>% 
  group_by(catalognumber) %>% 
  mutate(arrays = (list(unique(detectedby)))) %>% #create a column with a list of the stations
  dplyr::select(catalognumber, arrays)  %>% #remove excess columns
  distinct_all() %>% #keep only one record of each
  mutate(number_of_arrays = sapply(arrays, length)) %>% #sapply: applies a function across a List - in this case we are applying length()
  as.data.frame() 

View(arrays)

# number of stations visited, start and end dates, and track length

animal_id_summary <- nsbs_matched_full_no_release %>% 
  group_by(catalognumber) %>%
  summarise(dets = length(catalognumber),
            stations = length(unique(station)),
            min = min(datecollected), 
            max = max(datecollected), 
            tracklength = max(datecollected)-min(datecollected))

View(animal_id_summary)

## Plot of Detection Counts ----

nsbs_matched_full_no_release  %>% 
  mutate(datecollected=ymd_hms(datecollected)) %>% #make datetime
  mutate(year_month = floor_date(datecollected, "months")) %>% #round to month
  group_by(year_month) %>% #can group by station, species etc.
  summarize(count =n()) %>% #how many dets per year_month
  ggplot(aes(x = (month(year_month) %>% as.factor()), 
             y = count, 
             fill = (year(year_month) %>% as.factor())
  )
  )+ 
  geom_bar(stat = "identity", position = "dodge2")+ 
  xlab("Month")+
  ylab("Total Detection Count")+
  ggtitle('NSBS Detections by Month (2021-2022)')+ #title
  labs(fill = "Year") #legend title

# Other Example Plots ----
#Use the color scales in this package to make plots that are pretty, 
#better represent your data, easier to read by those with colorblindness, and print well in grey scale.
library(viridis)

# an easy abacus plot!

abacus_animals <- 
  ggplot(data = nsbs_matched_full, aes(x = datecollected, y = catalognumber, col = detectedby)) +
  geom_point() +
  ggtitle("Detections by animal") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_color_viridis(discrete = TRUE)

abacus_animals

abacus_arrays <- 
  ggplot(data = nsbs_matched_full,  aes(x = datecollected, y = detectedby, col = catalognumber)) +
  geom_point() +
  ggtitle("Detections by Array") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_color_viridis(discrete = TRUE)

abacus_arrays #might be better with just a subset, huh??

# track movement using geom_path!!

nsbs_subset <- nsbs_matched_full %>%
  dplyr::filter(catalognumber %in% c('NSBS-Nessie', 'NSBS-1250981-2019-09-06', 
                                     'NSBS-1393342-2021-08-10', 'NSBS-1393332-2021-08-05'))

View(nsbs_subset)

movMap <- 
  ggmap(base, extent = 'panel') + #use the BASE we set up before
  ylab("Latitude") +
  xlab("Longitude") +
  geom_path(data = nsbs_subset, aes(x = longitude, y = latitude, col = commonname)) + #connect the dots with lines
  geom_point(data = nsbs_subset, aes(x = longitude, y = latitude, col = commonname)) + #layer the stations back on
  #scale_colour_manual(values = c("red", "blue"), name = "Species")+ #for more than one species
  facet_wrap(~catalognumber, nrow=2, ncol=2)+
  ggtitle("Inferred Animal Paths")

#to size the dots by number of detections you could do something like: size = (log(length(animal)id))?

movMap

# monthly latitudinal distribution of your animals (works best w >1 species)
nsbs_matched_full %>%
  group_by(m=month(datecollected), catalognumber, scientificname) %>% #make our groups
  summarise(mean=mean(latitude)) %>% #mean lat
  ggplot(aes(m %>% factor, mean, colour=scientificname, fill=scientificname))+ #the data is supplied, but no info on how to show it!
  geom_point(size=3, position="jitter")+   # draw data as points, and use jitter to help see all points instead of superimposition
  #coord_flip()+   #flip x y, not needed here
  scale_colour_manual(values = "blue")+ #change the colour to represent the species better!
  scale_fill_manual(values = "grey")+ 
  geom_boxplot()+ #another layer
  geom_violin(colour="black") #and one more layer


#There are other ways to present a summary of data like this that we might have chosen. 
#geom_density2d() will give us a KDE for our data points and give us some contours across our chosen plot axes.

nsbs_matched_full %>% 
  group_by(month=month(datecollected), catalognumber, scientificname) %>%
  summarise(meanlat=mean(latitude)) %>%
  ggplot(aes(month, meanlat, colour=scientificname, fill=scientificname))+
  geom_point(size=3, position="jitter")+
  scale_colour_manual(values = "blue")+
  scale_fill_manual(values = "grey")+
  geom_density2d(linewidth=7, lty=1) #this is the only difference from the plot above 

#anything you specify in the aes() is applied to the actual data points/whole plot, 
#anything specified in geom() is applied to that layer only (colour, size...)

# per-individual density contours - lots of plots: called facets!

nsbs_matched_full %>%
  ggplot(aes(longitude, latitude))+
  facet_wrap(~catalognumber)+ #make one plot per individual
  geom_violin()
#Warnings going on above.
