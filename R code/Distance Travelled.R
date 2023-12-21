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

data <- read_glatos_detections("./BQMUS_detectionsWithLocs_20231030_132619.csv")
data <- false_detections(data, tf = 3600, show_plot = TRUE) #The filter identified 2198 (0.41%) of 533352 detections as potentially false.
data <- data[data$passed_filter == 1,] #934340 detections for my study

#change the detections into est instead of utc
data$detection_timestamp_est <- format(data$detection_timestamp_utc, tz = 'est', usetz = TRUE) #detection timestampts were in UTC

summarize_detections(data)
detection_events <- glatos::detection_events(data)
residence_index(detection_events, group_col = "animal_id" )
#REceivers
rec <- read_glatos_receivers("./GLATOS_receiverLocations_20231025_203940.csv")
#
data <- data %>% 
  mutate(animal_id = as.integer(ifelse(animal_id == "BQMUS_001", 1, 
                                       ifelse(animal_id == "BQMUS_002", 2, 
                                              ifelse(animal_id == "BQMUS_003", 3, 4)))))

data$Year <- as.Date(data$detection_timestamp_est)
data$YearFormat <- format(as.Date(data$Year, format="%d/%m/%Y"),"%Y")
data$JulianDay <- format(as.Date(data$Year, format="%d/%m/%Y"),"%j")

####  Shape File and transition Layer ####
Lake_O <- sf::st_read("./Shapefiles/Lake_Ontario.shp")
#### Casts shape file into polygon
Lake_O <- st_cast(Lake_O, "POLYGON")
#### Transition Layer with given resolution
Lake_O_t <- make_transition3(Lake_O, res = c(0.01, 0.01))
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
summary(DistTravel2)

##########################################################################################################################################################################
#Daily Position estimates
#hourly position estimates
Daily_Position <- interpolate_path(data,
                                    trans = Lake_O_t$transition,
                                    int_time_stamp=86400,
                                    lnl_thresh=0.9)
Daily_Position <- Daily_Position %>% 
  group_by(animal_id, bin_timestamp) %>% 
  summarise(mean_lat = mean(latitude), 
            mean_long = mean(longitude))
Daily_Position$Year <- as.Date(Daily_Position$bin_timestamp)
Daily_Position$YearFormat <- format(as.Date(Daily_Position$Year, format="%d/%m/%Y"),"%Y")

poss_dist2 <- possibly(geosphere::distm, otherwise = NA)

pos4_range2 <- Daily_Position %>%
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
  scale_y_continuous(expand = c(0, 0), name = "Distance Travelled (Km)") +
  labs(fill = "Animal ID") +  # Set legend title for fill
  theme(legend.title = element_text("Animal ID", size = 25), 
        legend.text = element_text(size = 20))  # Adjust your theme as needed

summary(Daily_Position)


#######################################################################################################################

DistTravel2 <- DistTravel2 %>% 
  dplyr::select(animal_id, YearFormat, totalkm) %>% 
  rename(Animal_ID = animal_id, Year = YearFormat, Dist_TravKM = totalkm) %>% 
  mutate(Dist_TravKM = round(Dist_TravKM, 2)) 

write.xlsx(DistTravel2, "DistTrav_DailyPos.xlsx")
write.xlsx(DistTravel, "DistTrav_HourlyPos2.xlsx")
