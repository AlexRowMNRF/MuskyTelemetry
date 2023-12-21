###########################################################################################################
###########  MASTER FILE -- uSE THIS TO R CODE TO LOAD ALL DATA IN PREP FOR ANY OTHER CODE ################
###########################################################################################################


#load packages
library(car)
library(lme4)
library(dplyr)
library(ggplot2)
library(glatos)
library(lubridate)
library(grid)
library(cowplot)
library(gdistance)
library(sp)
library(raster)
library(tidyverse)
library(suncalc)
library(plotly)
library(gapminder)
library(ATT)
library(tidyverse)
library(adehabitatHR)
library(sf)
library(rgdal)
library(terra)
library(MuMIn)
library(mgcv)
library(emmeans)
library(nlme)
library(lmtest)
library(gratia)
library(tidymv)
library(devtools)
library(remotes)
library(nlcor)

#Create my theme for each graph 
mytheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 16, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black"),
  axis.title = element_text(size = 18, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1))

#Load data
mydata_master <- read.csv("C:/Users/Tufts Lab/OneDrive - Queen's University/Desktop/Alex Masters/R_work/Glatos_files/Master_detections.csv")
max(mydata_master$detection_timestamp_est)

test <- mydata_master %>% 
  filter(capture_location == "40 Acres" & sensor_units == "Meters") %>% 
  group_by(sensor_value_real >= 20)

test_deep  <- mydata_master %>% 
  filter(capture_location == "40 Acres" & sensor_value_real > 20 & sensor_units == "Meters") 

test_deep  %>% 
  group_by(season) %>% 
  summarise(n = n())
(78/56186)*100
(10143/56186)*100
(45965/56186) * 100

ggplot(test_deep, aes (x = season )) +
  geom_histogram(stat = "count")

test_shallow  <- mydata_master %>% 
  filter(capture_location == "40 Acres" & sensor_value_real <= 20 & sensor_units == "Meters") 

(nrow(test_deep)/nrow(test))*100
(nrow(test_shallow)/nrow(test))*100

#my_data_all <- filter(my_data_all, detection_timestamp_est < as.POSIXct("2022-10-22 023:59:00 EST", tz="EST"))
# READ ME
#           mydata_master is the file with all detections -- filtered, removed dead fish, added diel period, added season, added sensor values


#raw detection file for all detections 
detections <- read_glatos_detections("C:/Users/Tufts Lab/OneDrive - Queen's University/Desktop/Alex Masters/R_work/Glatos_files/ELOBB_20221116/ELOBB_detectionsWithLocs_20221116_232049.csv")
#all receivers 
rec <- read_glatos_receivers("C:/Users/Tufts Lab/OneDrive - Queen's University/Desktop/Alex Masters/R_work/Glatos_files/ELOBB_20221116/GLATOS_receiverLocations_20221116_151733.csv")
#animal workbook to look at descriptive data
workbook_Animal <- read_glatos_workbook(file.choose()) #"C:/Users/Tufts Lab/Desktop/Alex Masters/R_work/Glatos_files/Tag_Upload/ELOBB_GLATOS_20221116(1)/ELOBB_GLATOS_20221116.xlsm"
#only keep the animal portion of the workbook
Animal <- workbook_Animal$animals
#load tag specs for the 40 acres 

  Tag_Specs40 <- read_vemco_tag_specs("C:/Users/Tufts Lab/OneDrive - Queen's University/Desktop/Alex Masters/R_work/Glatos_files/TagSheet_32167.xls")
#load in spec sheet for the MD fish 
Tag_SpecsMD <- read_vemco_tag_specs("C:/Users/Tufts Lab/OneDrive - Queen's University/Desktop/Alex Masters/R_work/Glatos_files/TagSheet.MD.xls")

#number of detections in the 40 Acres 
Num40det <- detections %>%
  filter(capture_location == "40 Acres", 
         utc_release_date_time < ymd_hms("2022-01-01 21:00:00") & utc_release_date_time > ymd_hms("2020-08-06 21:00:00"))

#filter for the 40 Acre fish and maloytown and MD 
Detections_filtered <- detections %>%
  filter(capture_location == "40 Acres" | 
           capture_location == "Mallorytown" | 
           utc_release_date_time ==  ymd_hms("2020-08-07 21:00:00") |
           utc_release_date_time ==  ymd_hms("2020-08-13 21:00:00"), 
         utc_release_date_time < ymd_hms("2022-01-01 21:00:00") & utc_release_date_time > ymd_hms("2020-08-06 21:00:00"))


#############False Detections
Detections_filtered <- false_detections(Detections_filtered, tf = 3600, show_plot = FALSE) #1.74 % detections fALSE DETECTIONS from all MD, 40 and Malloryt
Detections_filtered <- Detections_filtered[Detections_filtered$passed_filter == 1,] #934340 detections for my study

#change the detections into est instead of utc
Detections_filtered$detection_timestamp_est <- format(Detections_filtered$detection_timestamp_utc, tz = 'est', usetz = TRUE) #detection timestampts were in UTC





#-- Note that i need to fix the typo in the animal ID code (1399293 and 1392993 are mixed) -- then create a dataset that is with the dead fish and one without the dead fish 



Detections_filtered <- Detections_filtered %>%
  mutate(New_ID = case_when(
    transmitter_id == "14421" ~ "1",
    transmitter_id == "14422" ~ "1",
    transmitter_id == "14423" ~ "2",
    transmitter_id == "14424" ~ "2",
    transmitter_id == "14425" ~ "3",
    transmitter_id == "14426" ~ "3",
    transmitter_id == "14427" ~ "4",
    transmitter_id == "14428" ~ "4",
    transmitter_id == "14429" ~ "5",
    transmitter_id == "14430" ~ "5",
    transmitter_id == "14431" ~ "6",
    transmitter_id == "14432" ~ "6",
    transmitter_id == "14433" ~ "7",
    transmitter_id == "14434" ~ "7",
    transmitter_id == "14435" ~ "8",
    transmitter_id == "14436" ~ "8",
    transmitter_id == "14437" ~ "9",
    transmitter_id == "14438" ~ "9",
    transmitter_id == "14439" ~ "10",
    transmitter_id == "14440" ~ "10",
    transmitter_id == "14441" ~ "11",
    transmitter_id == "14442" ~ "11",
    transmitter_id == "14443" ~ "12",
    transmitter_id == "14444" ~ "12",
    transmitter_id == "14445" ~ "13",
    transmitter_id == "14446" ~ "13",
    transmitter_id == "14447" ~ "14",
    transmitter_id == "14448" ~ "14",
    transmitter_id == "14449" ~ "15",
    transmitter_id == "14450" ~ "15",
    transmitter_id == "14451" ~ "16",
    transmitter_id == "14452" ~ "16",
    transmitter_id == "14453" ~ "17",
    transmitter_id == "14454" ~ "17",
    transmitter_id == "14455" ~ "18",
    transmitter_id == "14456" ~ "18",
    transmitter_id == "14457" ~ "19",
    transmitter_id == "14458" ~ "19",
    transmitter_id == "14459" ~ "20",
    transmitter_id == "14460" ~ "20",
    transmitter_id == "21142" ~ "1",
    transmitter_id == "27031" ~ "2",
    transmitter_id == "58527" ~ "3",
    transmitter_id == "58528" ~ "4",
    transmitter_id == "58529" ~ "5",
    transmitter_id == "58530" ~ "6",
    transmitter_id == "58531" ~ "7",
    transmitter_id == "58532" ~ "8",
    transmitter_id == "58533" ~ "9",
    transmitter_id == "58534" ~ "10",
    transmitter_id == "58535" ~ "11",
    transmitter_id == "58536" ~ "12",
    transmitter_id == "58537" ~ "13",
    transmitter_id == "58538" ~ "14",
    transmitter_id == "58539" ~ "15",
    transmitter_id == "58540" ~ "16",
    transmitter_id == "58541" ~ "17",
    transmitter_id == "58542" ~ "18",
    transmitter_id == "58543" ~ "19",
    transmitter_id == "58544" ~ "20", 
    transmitter_id == "11838" ~ "1", 
    transmitter_id == "11839" ~ "1", 
    transmitter_id == "11840" ~ "2", 
    transmitter_id == "11841" ~ "2", 
    transmitter_id == "11836" ~ "3", 
    transmitter_id == "11837" ~ "3", 
    transmitter_id == "11834" ~ "4", 
    transmitter_id == "11835" ~ "4", 
    transmitter_id == "11848" ~ "5", 
    transmitter_id == "11849" ~ "5", 
    transmitter_id == "11846" ~ "6", 
    transmitter_id == "11847" ~ "6", 
    transmitter_id == "11844" ~ "7", 
    transmitter_id == "11845" ~ "7", 
    transmitter_id == "11842" ~ "8", 
    transmitter_id == "11843" ~ "8", 
    transmitter_id == "11856" ~ "9", 
    transmitter_id == "11857" ~ "9", 
    transmitter_id == "11854" ~ "10", 
    transmitter_id == "11855" ~ "10", 
    transmitter_id == "11852" ~ "11", 
    transmitter_id == "11853" ~ "11", 
    transmitter_id == "11850" ~ "12", 
    transmitter_id == "11851" ~ "12",
    transmitter_id == "11864" ~ "13", 
    transmitter_id == "11865" ~ "13", 
    transmitter_id == "11862" ~ "14", 
    transmitter_id == "11863" ~ "14",
    transmitter_id == "11860" ~ "15", 
    transmitter_id == "11861" ~ "15", 
    transmitter_id == "11858" ~ "16", 
    transmitter_id == "11859" ~ "16",
    transmitter_id == "11872" ~ "17", 
    transmitter_id == "11873" ~ "17", 
    transmitter_id == "11870" ~ "18", 
    transmitter_id == "11871" ~ "18",
    transmitter_id == "11868" ~ "19", 
    transmitter_id == "11869" ~ "19", 
    transmitter_id == "11866" ~ "20", 
    transmitter_id == "11867" ~ "20",
  ))

#remove dead fish for all locations-- still need to remove MD
working_data_all <- Detections_filtered %>% 
  filter(transmitter_id != "14425",
         transmitter_id != "14426",
         transmitter_id != "14427",
         transmitter_id != "14428",
         transmitter_id != "14441",
         transmitter_id != "14442",
         transmitter_id != "14447",
         transmitter_id != "14448",
         transmitter_id != "14451",
         transmitter_id != "14452", 
         transmitter_id != "14457",
         transmitter_id != "14458", 
         transmitter_id != "11834", #dead fish MD
         transmitter_id != "11835", 
         transmitter_id != "11844" , #dead fish MD
         transmitter_id != "11845", 
         transmitter_id != "11842" , #dead fish MD
         transmitter_id != "11843",
         transmitter_id != "11852" , #dead fish MD
         transmitter_id != "11853",
         transmitter_id != "11862" , #Dead fish MD
         transmitter_id != "11863" ,
         transmitter_id != "11860" , #dead fish MD
         transmitter_id != "11861" ,
         transmitter_id != "11858" , #dead fish MD
         transmitter_id != "11859" ,
         transmitter_id != "11868" , #dead fish MD
         transmitter_id != "11869" ,
         transmitter_id != "11866", 
         transmitter_id != "11867" )

######################################################################################################################
########################  40 acre prep ###############################################################################
#all detections from 40 acres with the sensor values converted
Det_40_all <- Detections_filtered  %>% 
  filter(capture_location ==  "40 Acres")
Det_40_all  <- real_sensor_values(Det_40_all, Tag_Specs40$specs) #add sensor values to the all 40 acre detections 


#detections of alive fish from the 40 acres
working_data_40 <- working_data_all  %>% 
  filter(capture_location ==  "40 Acres"
         & detection_timestamp_est > as.POSIXct("2021-08-24 023:59:00 EST", tz="EST") 
         & detection_timestamp_est < as.POSIXct("2022-10-22 023:59:00 EST", tz="EST"))
working_data_40 <- real_sensor_values(working_data_40, Tag_Specs40$specs) 

#add sensor values to 40 acre fish
#working_data_40 <- filter(working_data_40,detection_timestamp_est > as.POSIXct("2021-08-24 023:59:00 EST", tz="EST"))

######################################################################################################################
########################  Main duck prep ###############################################################################

#all detections from MD - no dead fish removed
det_MD <- Detections_filtered %>% 
  filter(capture_location == "Main Ducks")
#add sensor values
det_MD <- real_sensor_values(det_MD, Tag_SpecsMD$specs)

workin_data_MD <- working_data_all %>% 
  filter(capture_location == "Main Ducks")
#add sensor values
workin_data_MD <- real_sensor_values(workin_data_MD, Tag_SpecsMD$specs)



######################################################################################################################
########################  Mallorytown prep ###############################################################################
det_mal <- Detections_filtered %>%
  filter(capture_location == "Mallorytown") 
det_mal <- det_mal %>% 
  dplyr::select(transmitter_id, animal_id, deploy_lat, deploy_long, capture_location, 
                utc_release_date_time, station, detection_timestamp_est, New_ID)
det_mal$sensor_units <- NA
det_mal$sensor_value_real <- NA

########################################################################################################################
####################    Create the dataset with all locations in it with the sensor values ###########################


# first join the md file to the 40 acre files -- then mALLORYtown file 

my_data_all <- rbind(working_data_40, workin_data_MD)

my_data_all <- my_data_all %>% 
  dplyr::select(transmitter_id, animal_id, deploy_lat, deploy_long, capture_location, 
         utc_release_date_time, station, detection_timestamp_est, New_ID, sensor_units, 
         sensor_value_real)

#all data - with sensor info 
my_data_all <- rbind(my_data_all, det_mal)
my_data_all$Date_day <- as_date(my_data_all$detection_timestamp_est)







gdist <-
  function(lon.1, lat.1, lon.2, lat.2, units = 'nm', a = 6378137.0, b = 6356752.3142, verbose = FALSE){
    #
    # Calculate geodesic distance (in nm) between two points specified by latitude/longitude
    # using Vincenty inverse formula for ellipsoids
    # Reference: Direct and inverse solutions of geodesics on the ellipsoid with application
    #  of nested equations.  Survey Review XXII, 176, April 1975.
    #
    # Inspired by: http://www.movable-type.co.uk/scripts/LatLongVincenty.html
    #
    #   DATE WRITTEN:  10 January 2005      LAST REVISED:   04 January 2010
    #   AUTHOR:  John R. Wallace: Imap.for.R@gmail.com
    #
    #
    if(any(!is.finite(c(lon.1, lat.1, lon.2, lat.2))))
      return(NA)
    
    # a, b = major & minor semiaxes of the ellipsoid in meters
    # flat = flattening (a-b)/a
    # lat.1, lat.2 = geodetic latitude
    # L = difference in longitude
    
    
    rad <- pi/180
    lon.1 <- lon.1 * rad
    lat.1 <- lat.1 * rad
    lon.2 <- lon.2 * rad
    lat.2 <- lat.2 * rad
    
    flat <- (a - b)/a
    L <- lon.1 - lon.2
    U1 <- atan((1 - flat) * tan(lat.1))
    U2 <- atan((1 - flat) * tan(lat.2))
    
    lamda <- L
    lamda.old <- 2 * pi
    if(verbose)
      cat("\nStarting lamda =", lamda, "\n\n")
    
    i <- 1
    while(abs(lamda - lamda.old) > 1e-011) {
      sin.sigma <- sqrt((cos(U2) * sin(lamda))^2 + (cos(U1) * sin(U2) - sin(U1) * cos(U2) * cos(lamda))^2)
      cos.sigma <- sin(U1) * sin(U2) + cos(U1) * cos(U2) * cos(lamda)
      sigma <- atan2(sin.sigma, cos.sigma)
      sin.alpha <- (cos(U1) * cos(U2) * sin(lamda))/ifelse(sin(sigma) == 0, 1e-025, sin(sigma))
      cos2.alpha <- 1 - sin.alpha^2
      cos2.sigma.m <- cos(sigma) - (2 * sin(U1) * sin(U2))/ifelse(cos2.alpha == 0, 1e-025, cos2.alpha)
      C. <- (flat/16) * (cos2.alpha * (4 + flat * (4 - 3 * cos2.alpha)))
      if(verbose) {
        cat("sin.sigma =", sin.sigma, "\n")
        cat("cos.sigma =", cos.sigma, "\n")
        cat("sigma =", sigma, "\n")
        cat("sin.alpha =", sin.alpha, "\n")
        cat("cos2.alpha =", cos2.alpha, "\n")
        cat("cos2.sigma.m =", cos2.sigma.m, "\n")
        cat("C =", C., "\n")
        cat("lamda diff =", lamda - lamda.old, "\n")
      }
      lamda.old <- lamda
      lamda <- L + (1 - C.) * flat * sin.alpha * (sigma + C. * sin.sigma * (cos2.sigma.m + C. * cos.sigma * (-1 + 2 * cos2.sigma.m^2)))
      if(verbose)
        cat("New lamda =", lamda, "\n\n")
      if(i > 20) {
        warning("lamda did not converge")
        return(NA)
      }
      i <- i + 1
    }
    
    u2 <- (cos2.alpha * (a^2 - b^2))/b^2
    A <- 1 + (u2/16384) * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
    B <- (u2/1024) * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
    delta.sigma <- B * sin(sigma) * (cos2.sigma.m + (B/4) * (cos(sigma) * (-1 + 2 * cos2.sigma.m^2) - (B/6) * cos2.sigma.m * (-3 + 4 * sin(sigma)^2) * (
      -3 + 4 * cos2.sigma.m^2)))
    
    if(verbose) {
      alpha1 <- atan((cos(U2) * sin(lamda))/(cos(U1) * sin(U2) - sin(U1) * cos(U2) * cos(lamda)))
      cat("\nalpha1 =", alpha1/rad, "\n")
      alpha2 <- atan((cos(U1) * sin(lamda))/( - sin(U1) * cos(U2) + cos(U1) * sin(U2) * cos(lamda)))
      cat("alpha2 =", alpha2/rad, "\n\n")
      ""
      cat("2*sigma.m =", acos(cos2.sigma.m), "\n")
      ""
      cat("b =", b, "\n")
      cat("A =", A, "\n")
      cat("sigma (radians) =", sigma, "\n")
      cat("delta.sigma (radians) =", delta.sigma, "\n")
      cat("Distance: s = b * A * (sigma - delta.sigma)\n\n")
      ""
    }
    
    s <- (b * A * (sigma - delta.sigma))
    
    switch(units, 
           m = s,
           km = s/1000,
           nm = s/1852,
           miles = s/1609.344)
    
    
  }



