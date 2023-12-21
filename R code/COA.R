#Center of Activity Measures 
#remotes::install_github("vinayudyawer/ATT")

#steps 
#   get COA for just my 40 acre fish 
#   subset only coordinate columns - separate - with animal ID 
#   convert coordinates into the correct format 
#   compute MCP


#Calculate MCPs


corelation between total disstnace traveled and extent of river used....
###############################################################################################
###############################################################################################
###############################################################################################
#Only select 40 Acre fish 

acre_40 <- mydata_master %>% 
  filter(capture_location ==  "40 Acres"
& detection_timestamp_est2 > as.POSIXct("2021-08-24 023:59:00 EST", tz="EST") 
& detection_timestamp_est2 < as.POSIXct("2022-10-22 023:59:00 EST", tz="EST"))

#reassingn the season and diel period - below doesnt match season 


#working_data_40$season <- acre_40$season
working_data_40$diel_period <- acre_40$diel_period

working_data_40$New_ID <- acre_40$New_ID
working_data_40 <- working_data_40 %>% 
  dplyr::select(!animal_id) 
working_data_40 <- dplyr::rename(working_data_40, animal_id = New_ID)
working_data_40$Date <- as_date(working_data_40$detection_timestamp_est)

working_data_40 <- working_data_40 %>% 
  mutate(season = if_else(Date >= as_date("2021-10-17") & Date < as_date("2021-12-20"), "Fall",
                          if_else(Date >= as_date("2021-12-20") & Date < as_date("2022-04-25"), "Winter", 
                                  if_else(Date >= as_date("2022-04-25") & Date < as_date("2022-05-21"), "Spring",
                                          if_else(Date >= as_date("2020-10-06") & Date < as_date("2020-12-26"), "Fall",
                                                  if_else(Date >= as_date("2020-12-26") & Date < as_date("2021-04-26"), "Winter",
                                                          if_else(Date >= as_date("2021-04-26") & Date < as_date("2021-06-19"), "Spring", "Summer")))))))

##################### Function for MCP 
meanMCP <- function(seas_on){
  sum_working_data <- filter(working_data_40, season == seas_on)
  #sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6")
  att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
  summer_ATT <- COA(ATTdata = att_data_summer)
  attr(summer_ATT, "class") <- c("data.frame")
  cord.dec = SpatialPoints(cbind(summer_ATT$Longitude.coa, summer_ATT$Latitude.coa), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  summer_ATT <- summer_ATT %>%
    mutate(Latitude.coa = cord.UTM$coords.x2, Longitude.coa = cord.UTM$coords.x1)
  coordinates(summer_ATT) <- summer_ATT[, c('Longitude.coa', 'Latitude.coa')]
  Summer_cp2 <- mcp((summer_ATT)[,1], percent = 95, unin = c("m"),
                    unout = c("km2"))
  Summer_cp2 <- st_as_sf(Summer_cp2)
  meanMCP <- Summer_cp2 %>%
    summarise(mean = mean(area))
  return(Summer_cp2)
  plot(Summer_cp2$area~as.factor(Summer_cp2$id))
}



#function to compute MCP without ID6
meanMCP_no6 <- function(seas_on){
  sum_working_data <- filter(working_data_40, season == seas_on & animal_id != "6" & animal_id != "7" & animal_id != "1")
  #sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6")
  att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
  summer_ATT <- COA(ATTdata = att_data_summer)
  attr(summer_ATT, "class") <- c("data.frame")
  cord.dec = SpatialPoints(cbind(summer_ATT$Longitude.coa, summer_ATT$Latitude.coa), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  summer_ATT <- summer_ATT %>%
    mutate(Latitude.coa = cord.UTM$coords.x2, Longitude.coa = cord.UTM$coords.x1)
  coordinates(summer_ATT) <- summer_ATT[, c('Longitude.coa', 'Latitude.coa')]
  Summer_cp2 <- mcp((summer_ATT)[,1], percent = 95, unin = c("m"),
                    unout = c("km2"))
  Summer_cp2 <- st_as_sf(Summer_cp2)
  meanMCP <- Summer_cp2 %>%
    summarise(mean = mean(area))
  return(Summer_cp2)
  plot(Summer_cp2$area~as.factor(Summer_cp2$id))
}
#NOTE - TAG ID 2 AND 13 DURING SPRING ONLY GET DETECTED ON 2 RECEVIVERS EACH SO IT ONLY CREATES A LINE - THEREFORE NO AREA 
#Note that I will be using the no6 formula for my actual estimates because it allows for comparisons between seasons --- 
## SHOULD INCLUDE THE RAW VALUES for discussion 

SummerMCP <- meanMCP("Summer")
mean(SummerMCP$area)
WinterMCP <- meanMCP("Winter") #Not enough relocations to work
mean(WinterMCP)
SpringMCP <- meanMCP("Spring")
mean(SpringMCP$area)
FallMCP <- meanMCP("Fall") #Not enough detections for this to run 
SummerMCP_no6 <- meanMCP_no6("Summer")
mean(SummerMCP_no6$area)
WinterMCP_no6 <- meanMCP_no6("Winter")
mean(WinterMCP_no6$area)
SpringMCP_no6 <- meanMCP_no6("Spring")
mean(SpringMCP_no6$area)
FallMCP_no6 <- meanMCP_no6("Fall")
mean(FallMCP_no6$area)
#summer without 6 is 16.98 and winter is 3.78 

######################################################################
##################Explore fish id 6 
# this fish over wintered outside eab 10 in the lake and then in the early spring made the migration to hickory island - inflating the MCP
# for all fish during this time period

fishID6 <- filter(working_data_40, season == "Winter" & animal_id == "6")
ggplot(fishID6, aes(x = as_date(detection_timestamp_est), y = station)) +
  geom_point() +
  scale_x_date(name = "Date") +
  scale_y_discrete(name = "Station") +
  mytheme


#####################################Join all area estimates and add a column desginating season
#using only the files with fish ID 6 removed to be consistent 
SummerMCP_no6$Season <- "Summer"
WinterMCP_no6$Season <- "Winter"
area_estimates <- rbind(SummerMCP_no6, WinterMCP_no6)





###########paired t test for season 
t.test(area~Season, data = area_estimates, paired = TRUE)
#p = 0.03123, t = 2.5493, df = 9 -------- therefore there is a signifigant difference between seasons 
# remove fish id 17 
area_estimates_no17 <- subset(area_estimates, id!=17)
t.test(area~Season, data = area_estimates_no17, paired = TRUE)

ggplot(area_estimates, aes(x = id, y = area, fill = Season)) +
  geom_col( position="dodge")


######################################################################################################################################################
######################################################################################################################################################
#############################################################################
######################################################################################################################################################
######################################################################################################################################################
###################################################50 percent MCP##############




##################### Function for MCP 
meanMCP_50 <- function(seas_on){
  sum_working_data <- filter(working_data_40, season == seas_on)
  #sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6")
  att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
  summer_ATT <- COA(ATTdata = att_data_summer)
  attr(summer_ATT, "class") <- c("data.frame")
  cord.dec = SpatialPoints(cbind(summer_ATT$Longitude.coa, summer_ATT$Latitude.coa), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  summer_ATT <- summer_ATT %>%
    mutate(Latitude.coa = cord.UTM$coords.x2, Longitude.coa = cord.UTM$coords.x1)
  coordinates(summer_ATT) <- summer_ATT[, c('Longitude.coa', 'Latitude.coa')]
  Summer_cp2 <- mcp((summer_ATT)[,1], percent = 50, unin = c("m"),
                    unout = c("km2"))
  Summer_cp2 <- st_as_sf(Summer_cp2)
  meanMCP <- Summer_cp2 %>%
    summarise(mean = mean(area))
  return(Summer_cp2)
  plot(Summer_cp2$area~as.factor(Summer_cp2$id))
}


#function to compute MCP without ID6
meanMCP_no6_50 <- function(seas_on){
  sum_working_data <- filter(working_data_40, season == seas_on & animal_id != "6" & animal_id != "7" & animal_id != "1")
  #sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6")
  att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
  summer_ATT <- COA(ATTdata = att_data_summer)
  attr(summer_ATT, "class") <- c("data.frame")
  cord.dec = SpatialPoints(cbind(summer_ATT$Longitude.coa, summer_ATT$Latitude.coa), proj4string = CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  summer_ATT <- summer_ATT %>%
    mutate(Latitude.coa = cord.UTM$coords.x2, Longitude.coa = cord.UTM$coords.x1)
  coordinates(summer_ATT) <- summer_ATT[, c('Longitude.coa', 'Latitude.coa')]
  Summer_cp2 <- mcp((summer_ATT)[,1], percent = 50, unin = c("m"),
                    unout = c("km2"))
  Summer_cp2 <- st_as_sf(Summer_cp2)
  meanMCP <- Summer_cp2 %>%
    summarise(mean = mean(area))
  return(Summer_cp2)
  plot(Summer_cp2$area~as.factor(Summer_cp2$id))
}
#NOTE - TAG ID 2 AND 13 DURING SPRING ONLY GET DETECTED ON 2 RECEVIVERS EACH SO IT ONLY CREATES A LINE - THEREFORE NO AREA 
#Note that I will be using the no6 formula for my actual estimates because it allows for comparisons between seasons --- 
## SHOULD INCLUDE THE RAW VALUES for discussion 


SummerMCP_no6_50 <- meanMCP_no6_50("Summer")
mean(SummerMCP_no6_50$area)
WinterMCP_no6_50 <- meanMCP_no6_50("Winter")
mean(WinterMCP_no6_50$area)
#summer without 6 is 16.98 and winter is 3.78 


#####################################Join all area estimates and add a column desginating season
#using only the files with fish ID 6 removed to be consistent 
SummerMCP_no6_50$Season <- "Summer"
WinterMCP_no6_50$Season <- "Winter"
area_estimates_50 <- rbind(SummerMCP_no6_50, WinterMCP_no6_50)


###########paired t test for season CORE HOME RANGE ESTIAMTES 
t.test(area~Season, data = area_estimates_50, paired = TRUE)
#t = 3.3637, df = 9, p-value = 0.008339 -------- therefore there is a signifigant difference between seasons 

#test whether core is different than total #THIS TEST INDICATES THAT THE 95% MCP IS LARGER THAN THE 50
WinterMCP_no6$HRRef <- "Ninety5"
WinterMCP_no6_50$HRRef <- "fifty"

wint_area <- rbind(WinterMCP_no6, WinterMCP_no6_50)
t.test(area~HRRef, data = wint_area, paired = TRUE)


SummerMCP_no6$HRRef <- "Ninety5"
SummerMCP_no6_50$HRRef <- "fifty"

sum_area <- rbind(SummerMCP_no6, SummerMCP_no6_50)
t.test(area~HRRef, data = sum_area, paired = TRUE)













#Centrality estimate for location of each fish during each season 
sum_working_data <- filter(working_data_40, season == "Summer" & animal_id != "6" & animal_id != "7" & animal_id != "1")
att_data_summer <- convert_glatos_to_att(sum_working_data, rec)
summer_ATT <- COA(ATTdata = att_data_summer)
attr(summer_ATT, "class") <- c("data.frame")

Winter_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6" & animal_id != "7" & animal_id != "1")
Winter_data_summer <- convert_glatos_to_att(Winter_working_data, rec)
winter_ATT <- COA(ATTdata = Winter_data_summer)
attr(winter_ATT, "class") <- c("data.frame")

centrality_summer <- summer_ATT %>% 
  group_by(Tag.ID) %>% 
  summarise(centrality_Lat = mean(Latitude.coa), 
         centrality_Long = mean(Longitude.coa))
Summer_centrality <- cbind(SummerMCP_no6, centrality_summer$centrality_Lat, centrality_summer$centrality_Long)
Summer_centrality <- dplyr::rename(Summer_centrality, Long = centrality_summer.centrality_Long , Lat = centrality_summer.centrality_Lat)

centrality_winter <- winter_ATT %>% 
  group_by(Tag.ID) %>% 
  summarise(centrality_Lat = mean(Latitude.coa), 
            centrality_Long = mean(Longitude.coa)) 
  #dplyr::rename(Long = centrality_Long, Lat = centrality_Lat)

Winter_centrality <- cbind(WinterMCP_no6, centrality_winter$centrality_Lat, centrality_winter$centrality_Long)
Winter_centrality <- dplyr::rename(Winter_centrality, Long = centrality_winter.centrality_Long , Lat = centrality_winter.centrality_Lat)


#Combine datasets 
centrality_Full <- rbind(Winter_centrality, Summer_centrality)


##############################test for differences in centrality 
t.test(Lat~Season, data = centrality_Full, paired = TRUE) #t = 1.0489, df = 9, p-value = 0.3216
t.test(Long~Season, data = centrality_Full, paired = TRUE) #t = -0.26573, df = 9, p-value = 0.7964

############different way to tesst for centrality - distance between summer and winter mean lat and longs 

centrality_Distance <- data.frame(Tag.ID = NA, Distance_btw_season = NA)

seq <- c(2,5,8:10, 13,15,17,18,20)
index <- 0

for(i in seq){
  index <- index + 1
  test <- dplyr::filter(centrality_Full, id == i)
  dist = gdist(test$Long[1], test$Lat[1], test$Long[2], test$Lat[2], units = "km")
  centrality_Distance[index, 2] <- dist
  centrality_Distance[index,1] <- test$id[1]
}

ggplot(centrality_Distance, aes(x = Tag.ID, y = Distance_btw_season)) +
  geom_col() +
  mytheme

t.test(centrality_Distance$Distance_btw_season, mu = 0, alternative = "greater") #greater because we are interested if the distances are larger than 0 
#t = 3.3276, df = 9, p-value = 0.004416


