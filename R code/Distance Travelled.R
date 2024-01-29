###########################################
#Prep fpr following code - folowing is copied and pasted from BOQMusky r code 
library(remotes)
library(glatos)
library(dplyr)
library(ggplot2)
library(rgdal)
library(sf)
library(terra)
library(raster)
library(gganimate)
library(sp)
library(raster)
library(data.table)
library(ATT)
library(geosphere)
library(purrr)
library(tidyr)
mytheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 16, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black"),
  axis.title = element_text(size = 18, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1))

data <- read_glatos_detections("C:/Users/rowal/OneDrive - Government of Ontario/Desktop/Special Projects/BOQMusky/BOQMusky/OldMusky_cloned/BQMUS_detectionsWithLocs_20231030_132619.csv")
data <- false_detections(data, tf = 3600, show_plot = TRUE) #The filter identified 2198 (0.41%) of 533352 detections as potentially false.
data <- data[data$passed_filter == 1,] #934340 detections for my study

#change the detections into est instead of utc
data$detection_timestamp_est <- format(data$detection_timestamp_utc, tz = 'est', usetz = TRUE) #detection timestampts were in UTC
data$Year <- as.Date(data$detection_timestamp_est)
data$YearFormat <- format(as.Date(data$Year, format="%d/%m/%Y"),"%Y")
data$JulianDay <- format(as.Date(data$Year, format="%d/%m/%Y"),"%j")
data$Month <- format(as.Date(data$Year, format="%d/%m/%Y"),"%B")

#REceivers
rec <- read_glatos_receivers("./GLATOS_receiverLocations_20231025_203940.csv")
#
data <- data %>% 
  mutate(
    animal_id = as.integer(ifelse(animal_id == "BQMUS_001", 1, 
                                  ifelse(animal_id == "BQMUS_002", 2, 
                                         ifelse(animal_id == "BQMUS_003", 3, 4)))), 
    Trent = ifelse(station %in% c("TNT-001", "TRR-001"), "Trent", "Out"), 
    season = ifelse(Month %in% c("December", "January", "February"), "Winter", 
                    ifelse(Month %in% c("March", "April", "May"), "Spring",
                           ifelse(Month %in% c("June", "July", "August"), "Summer", "Fall"))))

data$Year <- as.Date(data$detection_timestamp_est)
data$YearFormat <- format(as.Date(data$Year, format="%d/%m/%Y"),"%Y")
data$JulianDay <- format(as.Date(data$Year, format="%d/%m/%Y"),"%j")
hist(as.numeric(data$JulianDay))
####  Shape File and transition Layer ####
Lake_O <- sf::st_read("./Shapefiles/Lake_Ontario.shp")
#### Casts shape file into polygon
Lake_O <- st_cast(Lake_O, "POLYGON")
#### Transition Layer with given resolution
Lake_O_t <- make_transition3(Lake_O, res = c(0.01, 0.01))

#JUmp to below to use DAILY position esitmates instead (more accepted in the literature)
#Daily Position estimates
#hourly position estimates
Daily_Position <- interpolate_path(data,
                                   trans = Lake_O_t$transition,
                                   int_time_stamp=86400,
                                   lnl_thresh=0.9)
int_vs_actual <- Daily_Position %>% 
  group_by(record_type, animal_id) %>% 
  summarise(nDet = n()) %>% 
  group_by(animal_id) %>% 
  reframe(sum = sum(nDet),
          nDet = nDet,
          type = record_type,
            Perc = nDet / sum *100)
Daily_Position2 <- Daily_Position %>% 
  group_by(animal_id, bin_timestamp) %>% 
  summarise(mean_lat = mean(latitude), 
            mean_long = mean(longitude))
##########################################################################################################################################################################
Daily_Position2$Year <- as.Date(Daily_Position2$bin_timestamp)
Daily_Position2$YearFormat <- format(as.Date(Daily_Position2$Year, format="%d/%m/%Y"),"%Y")
Daily_Position2$Month <- format(as.Date(Daily_Position2$Year, format="%d/%m/%Y"),"%B")
Daily_Position2 <- Daily_Position2 %>% 
  mutate( season = ifelse(Month %in% c("December", "January", "February"), "Winter", 
                          ifelse(Month %in% c("March", "April", "May"), "Spring",
                                 ifelse(Month %in% c("June", "July", "August"), "Summer", "Fall")))
    
  )

poss_dist2 <- possibly(geosphere::distm, otherwise = NA)

pos4_range2 <- Daily_Position2 %>%
  nest(coords=c(mean_long, mean_lat)) %>%
  group_by(animal_id) %>%
  mutate(prev_coords = lag(coords)) %>%
  ungroup() %>%
  mutate(distance = map2_dbl(coords, prev_coords, poss_dist2))

DistTravel2 <- pos4_range2 %>% 
  group_by(animal_id, YearFormat) %>% 
  summarise(totalm=sum(distance,na.rm=T)) 
DistTravel2$totalkm<-DistTravel2$totalm/1000;DistTravel2

DistTravel2 %>% 
  ggplot(aes(x = YearFormat, y = totalkm, fill = as.factor(animal_id))) +
  geom_col(position = position_dodge()) +
  mytheme +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,120, 30), limits = c(0,120), name = "Distance Travelled (Km)") +
  labs(fill = "Animal ID") +  # Set legend title for fill
  theme(legend.title = element_text("Animal ID", size = 25), 
        legend.text = element_text(size = 20))  # Adjust your theme as needed

#view with season
DistTravel3 <- pos4_range2 %>% 
  group_by(animal_id, YearFormat, season) %>% 
  summarise(totalm=sum(distance,na.rm=T)) 

DistTravel3$totalkm<-DistTravel3$totalm/1000;DistTravel3

DistTravel3$season <- factor(DistTravel3$season, levels = c("Spring", "Summer", "Fall", "Winter"))
fig <- list()

DistTravel3 %>% 
  ggplot(aes(x = YearFormat, y = totalkm, fill = season)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ animal_id, scales = 'free', ncol = 2) +  # Use facet_wrap with ncol
  mytheme +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 60, 30), limits = c(0, 60), name = "Distance Travelled (Km)") +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 25), 
        strip.text = element_text(size = 20, face = "bold"),
        strip.background = element_blank())
########################################################################################################################
#identify reliance on the trent river 
#study duration for each fish 
NumDay <- data %>% 
  group_by(animal_id) %>% 
  summarise(day1 = min(detection_timestamp_est), 
            finalday = max(detection_timestamp_est), 
            timedif = as.Date(finalday) - as.Date(day1))

get <- data %>%
  group_by(animal_id, Year, YearFormat, Month) %>%
  summarise(Trent_count = sum(Trent == "Trent")) %>%
  filter(Trent_count > 0) %>%
  ungroup() %>% 
  group_by(animal_id, YearFormat, Month) %>% 
  summarise(unique_trent_days = n_distinct(Year)) %>% 
  mutate(date = ymd(paste(YearFormat, Month, "1", sep = "-")),
         days_in_month = days_in_month(date), 
         perc = unique_trent_days / days_in_month *100)

get %>% 
  group_by(Month) %>% 
  summarise(mean = mean(perc))

########################################################################################################################
# Assuming your data frame is named Daily_Position
# Convert bin_timestamp to POSIXct
Daily_Position$bin_timestamp <- as.POSIXct(Daily_Position$bin_timestamp)


# Create sf object
sf_data <- st_as_sf(Daily_Position, coords = c("mean_long", "mean_lat"))

# Connect minimum distance points to make paths
sf_lines <- st_cast(sf_data, "LINESTRING")

# Calculate path distances
sf_lines$distance <- st_length(sf_lines)

# Plot using ggplot
ggplot(sf_lines) +
  geom_sf(aes(color = as.factor(animal_id), size = distance)) +
  scale_size_continuous(range = c(0.5, 3)) +
  theme_minimal() +
  labs(title = "Minimum Distance Paths",
       subtitle = "Interpolated paths between observed detections",
       caption = "Source: Your Study")
#############################################################################################################################################################################
#hourly position estimates
Hourly_Position <- interpolate_path(data,
                                    trans = Lake_O_t$transition,
                                    int_time_stamp=3600,
                                    lnl_thresh=0.9)
Hourly_Position <- Hourly_Position %>% 
  group_by(animal_id, bin_timestamp) %>% 
  summarise(mean_lat = mean(latitude), 
            mean_long = mean(longitude))
##########################################################################################################################################################################
Hourly_Position$Year <- as.Date(Hourly_Position$bin_timestamp)
Hourly_Position$YearFormat <- format(as.Date(Hourly_Position$Year, format="%d/%m/%Y"),"%Y")

poss_dist <- possibly(geosphere::distm, otherwise = NA)

pos4_range <- Hourly_Position %>%
  nest(coords=c(mean_long, mean_lat)) %>%
  group_by(animal_id) %>%
  mutate(prev_coords = lag(coords)) %>%
  ungroup() %>%
  mutate(distance = map2_dbl(coords, prev_coords, poss_dist))

DistTravel <- pos4_range %>% 
  group_by(animal_id, YearFormat) %>% 
  summarise(totalm=sum(distance,na.rm=T)) 
DistTravel$totalkm<-DistTravel$totalm/1000;DistTravel

mytheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 16, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black"),
  axis.title = element_text(size = 18, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1))

DistTravel %>% 
  ggplot(aes(x = YearFormat, y = totalkm, fill = as.factor(animal_id))) +
  geom_col(position = position_dodge()) +
  mytheme +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(expand = c(0, 0), name = "Distance Travelled (Km)") +
  labs(fill = "Animal ID") +  # Set legend title for fill
  theme(legend.title = element_text("Animal ID", size = 25), 
        legend.text = element_text(size = 20))  # Adjust your theme as needed

?st_length()

##########################################################################################################################################################################
