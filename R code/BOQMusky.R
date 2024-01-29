
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

#Must specify and include direct path to detection file in local computer - file too large to upload into github
data <- read_glatos_detections("C:/Users/rowal/OneDrive - Government of Ontario/Desktop/Special Projects/BOQMusky/BOQMusky/OldMusky_cloned/BQMUS_detectionsWithLocs_20231030_132619.csv")
data <- false_detections(data, tf = 3600, show_plot = TRUE) #The filter identified 2198 (0.41%) of 533352 detections as potentially false.
data <- data[data$passed_filter == 1,] #531154 detections for my study

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
tmp <- as.Date("16Jun10", format = "%d%b%y")
data$JulianDay <- format(as.Date(data$Year, format="%d/%m/%Y"),"%j")
#Abacus plots 
animals <- seq(1,4,1)
mytheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 16, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black"),
  axis.title = element_text(size = 18, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1))

tiff(filename = paste0("AnimalID_", 4, ".tif"),  width = 750, height = 480, units = "px")
  data %>%
    filter(animal_id == "4" & station != "MPT-001") %>%
    ggplot(aes(x = as.numeric(JulianDay), y = station, colour = as.factor(YearFormat))) +
    scale_y_discrete(name = "Station") +
    #scale_x_continuous(breaks = seq(0,400, 25), limits = c(0,400)) +
    scale_x_continuous(breaks = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349), 
                       labels = c("Jan" ,"Feb" ,"Mar", "Apr", "May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct", "Nov", "Dec"), 
                       name = "\n Month") +
    geom_point(data = filter(data, animal_id == "4" & YearFormat == '2018'), position = position_nudge(y = 0.3)) +
    geom_point(data = filter(data, animal_id == "4" & YearFormat == '2019'), position = position_nudge(y = 0.2)) +
    geom_point(data = filter(data, animal_id == "4" & YearFormat == '2020'), position = position_nudge(y = 0.1)) +
    geom_point(data = filter(data, animal_id == "4" & YearFormat == '2021' & station != "MPT-001"), position = position_nudge(y = 0)) +
    geom_point(data = filter(data, animal_id == "4" & YearFormat == '2022'), position = position_nudge(y = -0.1)) +
    geom_point(data = filter(data, animal_id == "4" & YearFormat == '2023'), position = position_nudge(y = -0.2)) +
    mytheme +
    theme(legend.title = element_blank(), 
          legend.text = element_text(size = 20), 
          legend.box.background = element_blank(), 
          legend.key = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    ggtitle(label = "Animal ID 4")
dev.off()

as.numeric(start_date)
########
start_date <- combined_temp %>%
  filter(AVGtemp >= 13.2 & AVGtemp <= 18.1) %>%
  slice_min(order_by = Date_day) %>%
  pull(Date_day)

end_date <- combined_temp %>%
  filter(AVGtemp >= 13.2 & AVGtemp <= 18.1) %>%
  slice_max(order_by = Date_day) %>%
  pull(Date_day)

start_date2 <- combined_temp %>%
  filter(AVGtemp >= 7 & AVGtemp <= 17 & Date_day > "2020-04-20") %>%
  slice_min(order_by = Date_day) %>%
  pull(Date_day)

end_date2 <- combined_temp %>%
  filter(AVGtemp >= 7 & AVGtemp <= 17 & Date_day > "2020-04-20") %>%
  slice_max(order_by = Date_day) %>%
  pull(Date_day)

start_date$JulianDay <- format(as.Date(start_date, format="%Y-%m-%d"),"%j")
end_date$JulianDay <- format(as.Date(end_date, format="%Y-%m-%d"),"%j")

start_date2$JulianDay <- format(as.Date(start_date2, format="%Y-%m-%d"),"%j")
end_date2$JulianDay <- format(as.Date(end_date2, format="%Y-%m-%d"),"%j")

tiff(filename = paste0("AnimalID_Spawning", 1, ".tif"),  width = 750, height = 480, units = "px")
data %>%
  filter(animal_id == "1" & station != "MPT-001") %>%
  ggplot(aes(x = as.numeric(JulianDay), y = station, colour = as.factor(YearFormat))) +
  scale_y_discrete(name = "Station") +
  scale_x_continuous(breaks = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     name = "\n Month") +
  geom_rect(data = NULL, aes(xmin = as.numeric(start_date2$JulianDay), xmax = as.numeric(end_date2$JulianDay), ymin = -Inf, ymax = Inf),
            fill = "darkgrey", alpha = 0.3) +
 # geom_rect(data = NULL, aes(xmin = as.numeric(start_date$JulianDay), xmax = as.numeric(end_date$JulianDay), ymin = -Inf, ymax = Inf),
 #           fill = "green", alpha = 0.3) +
  
  geom_point(data = filter(data, animal_id == "1" & YearFormat == '2018'), position = position_nudge(y = 0.3)) +
  geom_point(data = filter(data, animal_id == "1" & YearFormat == '2019'), position = position_nudge(y = 0.2)) +
  geom_point(data = filter(data, animal_id == "1" & YearFormat == '2020'), position = position_nudge(y = 0.1)) +
  geom_point(data = filter(data, animal_id == "1" & YearFormat == '2021' & station != "MPT-001"), position = position_nudge(y = 0)) +
  geom_point(data = filter(data, animal_id == "1" & YearFormat == '2022'), position = position_nudge(y = -0.1)) +
  geom_point(data = filter(data, animal_id == "1" & YearFormat == '2023'), position = position_nudge(y = -0.2)) +  
  mytheme +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.box.background = element_blank(),
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ggtitle(label = "Animal ID 1")
dev.off()
#####
#############################################################################################################
###################### Make Animations 

####  Shape File and transition Layer ####
Lake_O <- sf::st_read("C:/Users/rowal/OneDrive - Government of Ontario/Desktop/Special Projects/BOQMusky/BOQMusky/Lake_Ontario.shp")
#### Casts shape file into polygon
Lake_O <- st_cast(Lake_O, "POLYGON")
#### Transition Layer with given resolution
Lake_O_t <- make_transition3(Lake_O, res = c(0.01, 0.01))
#### Interpolate path
Hourly_Position <- interpolate_path(data,
                                    trans = Lake_O_t$transition,
                                    int_time_stamp=3600,
                                    lnl_thresh=0.9)
Hourly_Position <- Hourly_Position %>% 
  mutate(colour = ifelse(animal_id == "BQMUS_001", "red", 
                         ifelse(animal_id == "BQMUS_002", "blue",
                                ifelse(animal_id == "BQMUS_003", "green", "yellow"))))

myDir <- paste0(getwd(),"/frames2")

make_frames(Hourly_Position, recs = rec,  
            out_dir=myDir, animate = TRUE,
            col = Hourly_Position$colour,
            preview = FALSE,
            background_xlim = c(-77.7, -76.8), 
            background_ylim = c(44, 44.3),
            ani_name = "BOQMusky.mp4", pch = 16, cex = 3, 
            tail_dur=5, frame_delete = TRUE,
            overwrite = TRUE)


##########################
## animation for daily position estimates 

Daily_Position <- interpolate_path(data,
                                    trans = Lake_O_t$transition,
                                    int_time_stamp=86400,
                                    lnl_thresh=0.9)
Daily_Position <- Daily_Position %>% 
  mutate(colour = ifelse(animal_id == "BQMUS_001", "red", 
                         ifelse(animal_id == "BQMUS_002", "blue",
                                ifelse(animal_id == "BQMUS_003", "green", "yellow"))))
myDir <- paste0(getwd(),"/DailyPosition")

make_frames(Daily_Position, recs = rec,  
            out_dir=myDir, animate = TRUE,
            col = Daily_Position$colour,
            preview = FALSE,
            background_xlim = c(-77.7, -76.8), 
            background_ylim = c(44, 44.3),
            ani_name = "BOQMuskyDaily.mp4", pch = 16, cex = 3, 
            tail_dur=5, frame_delete = TRUE,
            overwrite = TRUE)
str(great_lakes_polygon)
str(Lake_O)

Lake_O_sf <- sf::st_as_sf(Lake_O, coords = c("geometry"))


make_frames(Daily_Position, recs = rec,  
            out_dir=myDir, animate = FALSE,
            bg_map = Lake_O_t,
            col = Daily_Position$colour,
            preview = TRUE,
            background_xlim = c(-77.7, -76.8), 
            background_ylim = c(44, 44.3),
            ani_name = "BOQMuskyDaily2.mp4", pch = 16, cex = 3, 
            tail_dur=5, frame_delete = TRUE,
            overwrite = TRUE)

make_frames(Daily_Position, recs = rec,  
            out_dir = myDir, animate = TRUE,
            bg_map = Lake_O$geometry,
            col = Daily_Position$colour,
            preview = FALSE,
            ani_name = "BOQMuskyDaily2.mp4",
            background_xlim = c(-77.7, -76.8), 
            background_ylim = c(44, 44.3),
            tail_dur = 5, frame_delete = TRUE,
            overwrite = TRUE)

make_frames_AR(Daily_Position, recs = rec,  
            out_dir = myDir, animate = TRUE,
            bg_map = Lake_O$geometry,
            col = Daily_Position$colour,
            preview = FALSE,
            ani_name = "BOQMuskyDaily2.mp4",
            background_xlim = c(-77.7, -76.8), 
            background_ylim = c(44, 44.3),
            tail_dur = 5, frame_delete = TRUE,
            overwrite = TRUE)
##########################################################################
####creating MCP for Musky 
##################### Function for MCP
#data 
data <- data %>% 
  mutate(animal_id = as.integer(ifelse(animal_id == "BQMUS_001", 1, 
                             ifelse(animal_id == "BQMUS_002", 2, 
                                    ifelse(animal_id == "BQMUS_003", 3, 4)))))

meanMCP_95 <- function(data){
  sum_working_data <- data
  #sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6")
  att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
  summer_ATT <- COA(ATTdata = att_data_summer)
  attr(summer_ATT, "class") <- c("data.frame")
  cord.dec = SpatialPoints(cbind(summer_ATT$Longitude.coa, summer_ATT$Latitude.coa), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  summer_ATT <- summer_ATT %>%
    mutate(Latitude.coa = cord.UTM$coords.x2, Longitude.coa = cord.UTM$coords.x1)
  coordinates(summer_ATT) <- summer_ATT[, c('Longitude.coa', 'Latitude.coa')]
  summer_ATT <- na.omit(summer_ATT)
  
  Summer_cp2 <- mcp((summer_ATT)[,1], percent = 95, unin = c("m"),
                    unout = c("km2"))
  writeOGR(Summer_cp2, dsn =".", "MCP_Musky95", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  Summer_cp2 <- st_as_sf(Summer_cp2)
  meanMCP <- Summer_cp2 %>%
    summarise(mean = mean(area))
  return(Summer_cp2)
  plot(Summer_cp2$area~as.factor(Summer_cp2$id))
}
output95 <- meanMCP_95(data)

meanMCP_50 <- function(data){
  sum_working_data <- data
  #sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6")
  att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
  summer_ATT <- COA(ATTdata = att_data_summer)
  attr(summer_ATT, "class") <- c("data.frame")
  cord.dec = SpatialPoints(cbind(summer_ATT$Longitude.coa, summer_ATT$Latitude.coa), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  summer_ATT <- summer_ATT %>%
    mutate(Latitude.coa = cord.UTM$coords.x2, Longitude.coa = cord.UTM$coords.x1)
  coordinates(summer_ATT) <- summer_ATT[, c('Longitude.coa', 'Latitude.coa')]
  summer_ATT <- na.omit(summer_ATT)
  
  Summer_cp2 <- mcp((summer_ATT)[,1], percent = 50, unin = c("m"),
                    unout = c("km2"))
  writeOGR(Summer_cp2, dsn =".", "Shape_MCP_40", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  Summer_cp2 <- st_as_sf(Summer_cp2)
  meanMCP <- Summer_cp2 %>%
    summarise(mean = mean(area))
  return(Summer_cp2$area~as.factor(Summer_cp2$id))
}
output50 <- meanMCP_50(data)

meanMCP_50 <- function(data){
  sum_working_data <- data
  #sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6")
  att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
  summer_ATT <- COA(ATTdata = att_data_summer)
  
  Hourly_Position
  
  attr(Hourly_Position, "class") <- c("data.frame")
  cord.dec = SpatialPoints(cbind(Hourly_Position$longitude, Hourly_Position$latitude), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  Hourly_Position <- Hourly_Position %>%
    mutate(Latitude.coa = cord.UTM$coords.x2, Longitude.coa = cord.UTM$coords.x1)
  coordinates(Hourly_Position) <- Hourly_Position[, c('Longitude.coa', 'Latitude.coa')]

  Summer_cp2 <- mcp((Hourly_Position)[,1], percent = 95, unin = c("m"),
                    unout = c("km2"))
  writeOGR(Summer_cp2, dsn =".", "Shape_MCP_95", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  Summer_cp2 <- st_as_sf(Summer_cp2)
  meanMCP <- Summer_cp2 %>%
    summarise(mean = mean(area))
  return(Summer_cp2$area~as.factor(Summer_cp2$id))
}
output50 <- meanMCP_50(data)

#testing new abacus plot 
data %>%
  filter(animal_id == "2" & station != "MPT-001") %>%
  ggplot(aes(x = as.numeric(JulianDay), y = Trent, colour = as.factor(YearFormat))) +
  scale_y_discrete(name = "Station") +
  #scale_x_continuous(breaks = seq(0,400, 25), limits = c(0,400)) +
  scale_x_continuous(breaks = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349), 
                     labels = c("Jan" ,"Feb" ,"Mar", "Apr", "May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct", "Nov", "Dec"), 
                     name = "\n Month") +
  geom_point(data = filter(data, animal_id == "2" & YearFormat == '2018'), position = position_nudge(y = 0.3)) +
  geom_point(data = filter(data, animal_id == "2" & YearFormat == '2019'), position = position_nudge(y = 0.2)) +
  geom_point(data = filter(data, animal_id == "2" & YearFormat == '2020'), position = position_nudge(y = 0.1)) +
  geom_point(data = filter(data, animal_id == "2" & YearFormat == '2021' & station != "MPT-001"), position = position_nudge(y = 0)) +
  geom_point(data = filter(data, animal_id == "2" & YearFormat == '2022'), position = position_nudge(y = -0.1)) +
  geom_point(data = filter(data, animal_id == "2" & YearFormat == '2023'), position = position_nudge(y = -0.2)) +
  mytheme +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 20), 
        legend.box.background = element_blank(), 
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ggtitle(label = "Animal ID 1")
