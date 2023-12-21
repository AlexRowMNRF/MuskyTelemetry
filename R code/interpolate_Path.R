


data(greatLakesTrLayer)
str(mydata_master)
mydata_master$detection_timestamp_est <- as.POSIXct(mydata_master$detection_timestamp_est, tz = "EST")
mydata_master$New_Date_hour <- strftime(mydata_master$detection_timestamp_est, format = "%H")
mydata_master$New_Date_min <- strftime(mydata_master$detection_timestamp_est, format = "%M")

test <- mydata_master %>% 
  filter(capture_location == "40 Acres") %>% 
  group_by(season, New_ID, Date_day, New_Date_hour) %>% 
  summarise(DeltaTime <- sum(abs(diff(detection_timestamp_est))), 
            n = n())


working_data_40 <- working_data_40[,3:42]
working_data_40$animal_id <- working_data_40$New_ID

pos2 <- interpolate_path(working_data_40, trans=greatLakesTrLayer,
                        int_time_stamp=3600,
                        lnl_thresh=0.9)
str(pos2)
daily24Hposition <- interpolate_path(working_data_40, trans=greatLakesTrLayer,
                                     int_time_stamp=86400,
                                   lnl_thresh=0.9)
pos24Hour <- daily24Hposition %>% 
  group_by(animal_id, bin_timestamp) %>% 
  summarise(mean_lat = mean(latitude), 
            mean_long = mean(longitude))
###
pos3 <- pos2 %>% 
  group_by(animal_id, bin_timestamp) %>% 
  summarise(mean_lat = mean(latitude), 
            mean_long = mean(longitude))


setwd("C:/Users/Tufts Lab/OneDrive - Queen's University/Desktop/Alex Masters/R_work/Glatos_files")

write.csv(pos3, "intPath_hour.csv")

intPath_hour <- read.csv("intPath_hour.csv")
#add day
intPath_hour$Date_day <- as_date(intPath_hour$bin_timestamp)


#add season
intPath_hour <- intPath_hour %>% 
  mutate(season = if_else(Date_day >= as_date("2021-10-17") & Date_day < as_date("2021-12-20"), "Fall",
                          if_else(Date_day >= as_date("2021-12-20") & Date_day < as_date("2022-04-25"), "Winter", 
                                  if_else(Date_day >= as_date("2022-04-25") & Date_day < as_date("2022-05-21"), "Spring",
                                          if_else(Date_day >= as_date("2020-10-06") & Date_day < as_date("2020-12-26"), "Fall",
                                                  if_else(Date_day >= as_date("2020-12-26") & Date_day < as_date("2021-04-26"), "Winter",
                                                          if_else(Date_day >= as_date("2021-04-26") & Date_day < as_date("2021-06-19"), "Spring", "Summer")))))))
intPath_hour2_testRM <- intPath_hour %>% 
  mutate(New_station )

poss_dist <- possibly(geosphere::distm, otherwise = NA)

pos4_range <- intPath_hour %>%
  nest(coords=c(mean_long, mean_lat)) %>%
  group_by(animal_id) %>%
  mutate(prev_coords = lag(coords)) %>%
  ungroup() %>%
  mutate(distance = map2_dbl(coords, prev_coords, poss_dist))

unknown <- pos4_range %>% 
  filter(distance > 0, animal_id == 15)
hist(unknown$distance)


sumdist2017<-pos4_range %>% 
  group_by(animal_id) %>% 
  summarise(totalm=sum(distance,na.rm=T))

sumdist2017_with_season<-pos4_range %>% 
  group_by(animal_id, season) %>% 
  summarise(totalm=sum(distance,na.rm=T))

sumdist2017$totalkm<-sumdist2017$totalm/1000
mean(sumdist2017$totalkm)

meanSeasonTravel <- pos4_range %>% 
  group_by(animal_id, season) %>% 
  summarise(totalm=sum(distance,na.rm=T)) %>% 
  group_by(season) %>% 
  summarise(mean = mean(totalm)/1000)



#dist per day 
dist_per_day <-pos4_range %>% 
  group_by(animal_id, Date_day, season) %>% 
  summarise(distance_per_day = sum(distance)) %>% 
  group_by(animal_id, season) %>% 
  summarise(meanDist_perday = mean(distance_per_day))
  



write.table(sumdist2017, file = "Dist_travel_ID.txt", sep = ",", quote = FALSE, row.names = FALSE)

##I TRIED THE 24 HOUR ONE AND THERE IS NO DIFFERENCE IN RESULTS 

#need to look at tagging location 

#Extent in winter --- need to divide by day... differnt lengths of season 
sumdist_season <- pos4_range %>% 
  group_by(animal_id, season) %>% 
  summarise(totalm=sum(distance,na.rm=T), 
            totalkm = totalm/1000)

sumdist_season_meanperseason <- pos4_range %>% 
  group_by(animal_id, season) %>% 
  summarise(totalm=sum(distance,na.rm=T), 
            totalkm = totalm/1000) %>% 
  group_by(season) %>% 
  summarise(meanSeason = mean(totalkm), 
            sd = sd(totalkm), 
            n = n(), 
            se = sd / sqrt(n), 
            UP = meanSeason + se, 
            DW = meanSeason - se)

sumdist_season$animal_id <- as.factor(sumdist_season$animal_id)
sumdist_season$season <- as.factor(sumdist_season$season)
sumdist_season$log_totalm <- log(sumdist_season$totalm + 1)

sumdist_season 
plot(sumdist_season$totalm ~ total_lin$Lin_Dist )

ggplot(pos4_range, aes(x = season, y = distance)) +
  geom_boxplot()

#Does extent differ across my fish - needs to be a mixed model wth a random int for fish id
#only using winter and summer for modeling using linear mixed effect
TotalMovement_season_LME <- sumdist_season %>% 
  filter(season == "Summer" | season =="Winter")
TotalMovement_season_LME <- TotalMovement_season_LME %>% 
  mutate(length <- case_when(animal_id == 1 ~ 431, 
                             animal_id == 2 ~ 440, 
                             animal_id == 5 ~ 431,
                             animal_id == 6 ~ 420, 
                             animal_id == 7 ~ 395,
                             animal_id == 8 ~ 410,
                             animal_id == 9 ~ 430,
                             animal_id == 10 ~ 411,
                             animal_id == 13 ~ 466,
                             animal_id == 15 ~ 440,
                             animal_id == 17 ~ 403,
                             animal_id == 18 ~ 400,
                             animal_id == 20 ~ 429))
colnames(TotalMovement_season_LME)[6] ="length"

TotalMovement_season_LME$logtotalm <- log(TotalMovement_season_LME$totalm)

TotalMovement_season_LME %>% 
  group_by(season) %>% 
  summarise(mean = mean(log_totalm))


model_see <- lme(totalm ~ season+length, random = ~ 1 | animal_id, data = TotalMovement_season_LME) 
summary(model_see)
summary(aov(model_see))

model <- lme(totalm ~ season*length, random = ~ 1 | animal_id, data = TotalMovement_season_LME) 
e.1_Ex <- resid(model)
f.1_Ex <- fitted(model)
plot(x = f.1_Ex, y = e.1_Ex)
plot(e.1_Ex ~ TotalMovement_season_LME$season)
summary(model)
summary(aov(model))

modelRM_INT <- lme(logtotalm ~ season+length, random = ~ 1 | animal_id, data = TotalMovement_season_LME, method = "ML") #this is the best full model for modeling the total movement per season per animal 
e.1_Ex_RMInt <- resid(modelRM_INT)
f.1_Ex_RMInt <- fitted(modelRM_INT)
plot(x = f.1_Ex_RMInt, y = e.1_Ex_RMInt)
plot(e.1_Ex_RMInt ~ TotalMovement_season_LME$season)
summary(modelRM_INT)
summary(aov(modelRM_INT))

modelRM_season <- lme(logtotalm ~ length, random = ~ 1 | animal_id, data = TotalMovement_season_LME, method = "ML") #this is the best full model for modeling the total movement per season per animal 
anova(modelRM_INT, modelRM_season)


#how i found out the distance traveled decreased in winter 
> exp(9.71)
[1] 16481.6
> exp(9.71 - 2.139)
[1] 1941.08
> 16481.6 -  1941.08
[1] 14540.52
> 14540.52/1000
[1] 14.54052
> 

#modelRM_INTeractin <- lme(logtotalm ~ season*length, random = ~ 1 | animal_id, data = TotalMovement_season_LME, method = "ML") #this is the best full model for modeling the total movement per season per animal 
summary(modelRM_INTeractin)
summary(aov(modelRM_INTeractin))

modelRM_L <- lme(logtotalm ~ season, random = ~ 1 | animal_id, data = TotalMovement_season_LME, method = "ML")
e.1_Ex_RM_L <- resid(modelRM_L)
f.1_Ex_RM_L <- fitted(modelRM_L)
plot(x = f.1_Ex_RM_L, y = e.1_Ex_RM_L)
plot(e.1_Ex_RM_L ~ TotalMovement_season_LME$season)
summary(modelRM_L)
summary(aov(modelRM_L))

anova(model, modelRM_INT)
anova(modelRM_INT, modelRM_L) #therefore length of the fish is not a predictor of total movement in a season 
anova(modelRM_INT, modelRM_INTeractin)

hist(TotalMovement_season_LME$totalm)
modt <- lme(logtotalm ~ season+length, random = ~ 1 | animal_id, weights = varIdent(form = ~1 | season), data = TotalMovement_season_LME, method = "ML") #fals assumption
e.1_Nmod<- resid(modt)
f.1_Nmod <- fitted(modt)
plot(x = f.1_Nmod, y = e.1_Nmod)
plot(e.1_Nmod ~ TotalMovement_season_LME$season)
summary(modt)
summary(aov(modt))
plot(modt)

modt2 <- lme(totalm ~ season, random = ~ 1 | animal_id, weights = varIdent(form = ~1 | season), data = TotalMovement_season_LME, method = "ML")
anova(modt, modt2)

##########the models above that accounted for different weights by season fail the assumptions ###################################################################
''



anova(modt, modelRM_INT)

mean_per_Season_Dist_Trav <- sumdist_season %>% 
  filter(season == "Summer" | season =="Winter") %>%
  group_by(season) %>% 
  summarise(mean = mean(totalm)/1000)


#Distance per day ############################################################################################ NOT USED 
Distance_Rate_Day <- pos4_range %>% 
  group_by(animal_id, Date_day, season) %>% 
  summarise(totalm=sum(distance,na.rm=T), 
            totalKm = totalm/1000, 
            log_totalm = log(totalm + 1), 
            log_totalKm = log(totalKm + 1)) %>% 
  filter(season == "Summer" | season == "Winter")

hist(Distance_Rate_Day$totalm)
hist(Distance_Rate_Day$log_totalKm)
model <- lm(log_totalKm ~ season + animal_id, data = Distance_Rate_Day)
summary(model)
summary(aov(model))
plot(model)

model2 <- lme(log_totalKm ~ season, data = Distance_Rate_Day, random = ~ 1 | animal_id)
summary(model2)
summary(aov(model2))

#per day rate for each season
sumdist_day <- pos4_range %>% 
  group_by(animal_id, season, Date_day) %>% 
  summarise(totalm=sum(distance,na.rm=T), 
            totalKm = totalm/1000) %>% 
  group_by(season) %>% 
  summarise(mean_Day = mean(totalKm))

#Now - total linear distance of the RIver used PER SEASON
total_lin <- intPath_hour %>% 
  group_by(animal_id, season) %>% 
  summarise(max_long = max(mean_long), 
            min_long = min(mean_long), 
            max_lat = max(mean_lat), 
            min_lat = min(mean_lat))

total_lin$Lin_Dist <- NA
total_lin$lin_dist2test <- NA

for(i in 1:nrow(total_lin)){
  dist <- gdist(total_lin$max_long[i], total_lin$max_lat[i], total_lin$min_long[i], total_lin$min_lat[i], units = "km")
  total_lin$Lin_Dist[i] <- dist
  dist2 <- gdist(total_lin$max_long[i], total_lin$min_lat[i], total_lin$min_long[i], total_lin$max_lat[i], units = "km")
  total_lin$lin_dist2test[i] <- dist2
}

#TOTAL FULL YEAR
total_lin2 <- intPath_hour %>% 
  group_by(animal_id) %>% 
  summarise(max_long = max(mean_long), 
            min_long = min(mean_long), 
            max_lat = max(mean_lat), 
            min_lat = min(mean_lat))



total_lin2$Lin_Dist <- NA
total_lin2$lin_dist2test <- NA

for(i in 1:nrow(total_lin2)){
  dist <- gdist(total_lin2$max_long[i], total_lin2$max_lat[i], total_lin2$min_long[i], total_lin2$min_lat[i], units = "km")
  total_lin2$Lin_Dist[i] <- dist
  dist2 <- gdist(total_lin2$max_long[i], total_lin2$min_lat[i], total_lin2$min_long[i], total_lin2$max_lat[i], units = "km")
  total_lin2$lin_dist2test[i] <- dist2
}
EXTENT <- total_lin2 %>% 
  group_by(animal_id) %>% 
  summarise(Riv_extent = (Lin_Dist))

mean(EXTENT$Riv_extent)
########
dIST_TRAV_TOTAL <- pos4_range %>% 
  group_by(animal_id) %>% 
  summarise(totalm=sum(distance,na.rm=T), 
            totalkm = totalm/1000)

MovementTravel <- left_join(EXTENT, dIST_TRAV_TOTAL, by = "animal_id")
Extent_move_plot <- MovementTravel %>% 
  ggplot(aes(x = Riv_extent, y = totalkm)) +
  #geom_smooth(se = FALSE) +
  geom_point(aes(size = 2, colour = as.factor(animal_id))) +
  scale_y_continuous(name = "Total Distance Traveled (Km) \n ", breaks = seq(0,3000,1000), limits = c(0,3000)) +
  scale_x_continuous(name = "\n Extent (Km)", breaks = seq(0,50, 10), limits = c(0,50)) +
  theme(legend.position = "none") +
  mytheme;Extent_move_plot

hist(MovementTravel$Riv_extent)
hist(MovementTravel$totalkm)

cor.test(~ totalkm+Riv_extent, data = MovementTravel)
cor.test(MovementTravel$Riv_extent,MovementTravel$totalm, method = "spearman") #used for non parametric data with no assumptions

plot(MovementTravel$Riv_extent,MovementTravel$totalm)


total_lin_export <- total_lin[,c(1,6)]
write.table(total_lin_export, file = "Dist_used.txt", sep = ",", quote = FALSE, row.names = FALSE)

#model the linear distance of river extent used for summer and winter#######################################################################################
#########################################################################################################################################################
############################################################################################################################################################
#########################################################################################################################################################
############################################################################################################################################################
Extent_season_sumtest <- total_lin %>% 
  filter(season == "Winter" | season == "Summer") %>% 
  group_by(season) %>% 
  summarise(meanDist = mean(Lin_Dist), 
            sd = sd(Lin_Dist), 
            n = n(),
            se = sd / sqrt(n), 
            UP = meanDist + se, 
            DW = meanDist - se)



Extent_season <- total_lin %>% 
  filter(season == "Winter" | season == "Summer")
Extent_season <- Extent_season %>% 
  mutate(length <- case_when(animal_id == 1 ~ 431, 
                             animal_id == 2 ~ 440, 
                             animal_id == 5 ~ 431,
                             animal_id == 6 ~ 420, 
                             animal_id == 7 ~ 395,
                             animal_id == 8 ~ 410,
                             animal_id == 9 ~ 430,
                             animal_id == 10 ~ 411,
                             animal_id == 13 ~ 466,
                             animal_id == 15 ~ 440,
                             animal_id == 17 ~ 403,
                             animal_id == 18 ~ 400,
                             animal_id == 20 ~ 429))
colnames(Extent_season)[9] ="length"

mod_Ex <- lme(Lin_Dist ~ season + length, random = ~ 1 | animal_id, data = Extent_season, method = "ML")
e.1_RD <- resid(mod_Ex)
f.1_RD <- fitted(mod_Ex)
plot(x = f.1_RD, y = e.1_RD)
plot(e.1_RD ~ TotalMovement_season_LME$season)
summary(mod_Ex)
summary(aov(mod_Ex))
plot(mod_Ex)


mod_Ex_rmseason <- lme(Lin_Dist ~ length, random = ~ 1 | animal_id, data = Extent_season, method = "ML")
anova(mod_Ex, mod_Ex_rmseason)

mod_Ex2 <- lme(Lin_Dist ~ season, random = ~ 1 | animal_id, data = Extent_season, method = "ML")
e.1_RD2<- resid(mod_Ex2)
f.1_RD2 <- fitted(mod_Ex2)
plot(x = f.1_RD2, y = e.1_RD2)
plot(e.1_RD2 ~ TotalMovement_season_LME$season)
summary(aov(mod_Ex))

hist(Extent_season$Lin_Dist)

anova(mod_Ex, mod_Ex2) #therfore length is not a signifigant predictor of river extent used in this study - but season is 

eeeeee
mean_totalLin <- total_lin_export %>% 
  summarise(mean = mean(Lin_Dist), 
            sd = sd(Lin_Dist), 
            se = sd/sqrt(13), 
            error = qnorm(0.975)*se,
            CIleft = mean - error,
            CIrght = mean + error)

mean_total_Dist <- sumdist2017 %>% 
  summarise(mean = mean(totalkm), 
            sd = sd(totalkm), 
            se = sd/sqrt(13), 
            error = qnorm(0.975)*se,
            CIleft = mean - error,
            CIrght = mean + error)

ggplot(sumdist_season, aes(x = animal_id, y = totalm, colour = season)) +
  geom_point() +
  mytheme


#Stats for extent 
#Does extent differ across my fish - needs to be a mixed model wth a random int for fish id
sumdist_season <- sumdist_season[c(1:33,35:52),]
model1 <- lm(totalm ~ season+animal_id, data = sumdist_season)
summary(model)
summary(aov(model))
plot(model)

model2 <- lm(log_totalm ~ season+animal_id, data = sumdist_season)
summary(model2)
summary(aov(model2))
plot(model)

model3 <- gls(log_totalm ~ season+animal_id, data = sumdist_season)
summary(model)
summary(aov(model))
plot(model)

#below looks the best - and lowest aic value
model4 <- gls(log_totalm ~ season+animal_id, data = sumdist_season, weights = varIdent(form = ~1 | animal_id))

summary(model4)
summary(aov(model4))
plot(model)
hist(sumdist_season$totalm, breaks = 20)

model6 <- gls(log_totalm ~ season, data = sumdist_season, weights = varIdent(form = ~1 | animal_id | season ))
summary(model)
summary(aov(model))
plot(model)

model7 <- lme(log_totalm ~ season, data = sumdist_season, random = ~ 1 | animal_id)
summary(model)
summary(aov(model))
plot(model)

#####################################################################################
#best model with the most rationale explanation
model8 <- lme(log_totalm ~ season, data = sumdist_season, random = ~ 1 | animal_id)
model_new <- lme(log_totalm ~ season, data = sumdist_season, random = ~ 1 | animal_id, weights = varIdent(form = ~1 | animal_id))
summary(model8)
summary(aov(model8))
summary(model_new)
summary(aov(model_new))
anova(model8,model_new)
r.squaredGLMM(model_new)
emmeans(model_new, pairwise ~ season)
ggplot(sumdist_season, aes(x = season, y = log_totalm)) +
  geom_boxplot()





#model for daily movement





############Looking for indvidual migrations to and from overwintering areas 
Migration <- pos4_range %>% 
  group_by(animal_id, Date_day) %>% 
  summarise(totalm=sum(distance,na.rm=T), 
            totalKm = totalm/1000)
#thought... look at the centrality statistics and see when each fish reaches that centrality for the winter. Then backtrack in time  
g <- ggplot(filter(mydata_master, capture_location == "40 Acres", New_ID == 7
                   ), aes(x = as_date(detection_timestamp_est), y = station)) +
  geom_point() +
  scale_x_date(name = "Date") +
  scale_y_discrete(name = "Station") +
  mytheme
ggplotly(g)
test <- filter(filter(mydata_master, capture_location == "40 Acres", 
                      Date_day >= "2021-10-08", 
                      Date_day <= "2021-10-11",
                      New_ID == 6, 
                      sensor_units == "Meters"))
test <- filter(filter(mydata_master, capture_location == "40 Acres", 
                      Date_day >= "2021-10-08", 
                      Date_day <= "2021-10-15",
                      New_ID == 7,
                      sensor_units == "Meters"))




test$New_Date_hour <- strftime(test$detection_timestamp_est, format = "%H")
test_hour <- test %>% 
  group_by(New_Date_hour, Date_day) %>% 
  summarise(meanDep = mean(sensor_value_real))
ggplot(test_hour, aes(x = New_Date_hour, y = meanDep)) +
  geom_boxplot()
ggplot(test_hour, aes(x = Date_day, y = meanDep)) +
  geom_boxplot()




hist(test$sensor_value_real)
mean(test$sensor_value_real)

g <- ggplot(filter(mydata_master, capture_location == "Mallorytown", animal_id == 219), aes(x = as_date(detection_timestamp_est), y = station)) +
  geom_point() +
  scale_x_date(name = "Date") +
  scale_y_discrete(name = "Station") +
  mytheme
ggplotly(g)

test <- filter(filter(mydata_master, capture_location == "Mallorytown", animal_id == 237, station == "STL-027"))
