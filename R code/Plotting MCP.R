##############Mapping MCPs###################
setwd("C:/Users/Tufts Lab/Desktop/Alex Masters/R_work")

acre_40 <- mydata_master %>% 
  filter(capture_location == "40 Acres") 
working_data_40$season <- acre_40$season
working_data_40$diel_period <- acre_40$diel_period

working_data_40$New_ID <- acre_40$New_ID
working_data_40 <- working_data_40 %>% 
  dplyr::select(!animal_id) 
working_data_40 <- dplyr::rename(working_data_40, animal_id = New_ID)

sum_working_data <- filter(working_data_40, season == "Summer" & animal_id != "6" & animal_id != "7" & animal_id != "1")
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
writeOGR(Summer_cp2, dsn =".", "Shape_MCP_40", driver = "ESRI Shapefile", overwrite_layer = TRUE)

Summer_cp2 <- st_as_sf(Summer_cp2)
meanMCP <- Summer_cp2 %>%
  summarise(mean = mean(area))
##############################################Winter


sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6" & animal_id != "7" & animal_id != "1")
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
writeOGR(Summer_cp2, dsn =".", "Shape_MCP_40_Winter", driver = "ESRI Shapefile", overwrite_layer = TRUE)

Summer_cp2 <- st_as_sf(Summer_cp2)

#################################################CORE
################################################


setwd("C:/Users/Tufts Lab/OneDrive - Queen's University/Desktop/Alex Masters/R_work")

sum_working_data <- filter(working_data_40, season == "Summer" & animal_id != "6" & animal_id != "7" & animal_id != "1")
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
writeOGR(Summer_cp2, dsn =".", "Shape_MCP_40_Core_Summer", driver = "ESRI Shapefile", overwrite_layer = TRUE)

Summer_cp2 <- st_as_sf(Summer_cp2)
meanMCP <- Summer_cp2 %>%
  summarise(mean = mean(area))

#######################WINTER Core
sum_working_data <- filter(working_data_40, season == "Winter" & animal_id != "6" & animal_id != "7" & animal_id != "1")
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
writeOGR(Summer_cp2, dsn =".", "Shape_MCP_40_Core_Winter", driver = "ESRI Shapefile", overwrite_layer = TRUE)

Summer_cp2 <- st_as_sf(Summer_cp2)
meanMCP <- Summer_cp2 %>%
  summarise(mean = mean(area))