library(lubridate)
library(tidyr)
library(scales)
#temperature Data with Queens 
temp <- read.csv("./TRR_Receiver2019.csv")
BWTTemp <- read.csv("./BellevilleWTTemps.csv")
BWTTemp <- BWTTemp %>% 
  rename(Day = ï..Day)
#remove oinly average temp for each hour 
temp <- temp %>% 
  filter(Description == "Average temperature") %>% 
  rename( Date  = ï..Date.and.Time..UTC.)
temp$Date <- as.POSIXct(temp$Date, format = "%m/%d/%Y %H:%M")
temp$Date_day <- (as_date(temp$Date ))

DailyAVG_REC <- temp %>% 
  filter(Date_day > as_date("2019-10-30")) %>% 
  group_by(Date_day) %>% 
  summarise(AVGtemp = mean(as.numeric(Data))) %>% 
  mutate(Source = "REC")

#Need to get teh data in the same format and then combine the datasets 
month_names <- tolower(month.name)

BWT <- BWTTemp %>%
  mutate(Month = tolower(Month),
         Date_day = as.Date(paste(Year, match(Month, month_names), Day, sep = "-")),
         AVGtemp = Temp,
         Source = "WTP") %>%
  dplyr::select(Date_day, AVGtemp, Source)
BWT$Date_day <- as.character(BWT$Date_day)
DailyAVG_REC$Date_day <- as.character(DailyAVG_REC$Date_day)

vec <- DailyAVG_REC$Date_day

filter_Temp <- BWT %>% 
  filter(Date_day %in% vec)
filter_Temp <- na.omit(filter_Temp)
combined_temp <- na.omit(full_join(filter_Temp, DailyAVG_REC))

temp_range <- combined_temp %>% 
  group_by(Date_day) %>% 
  summarise(range = max(AVGtemp) - min(AVGtemp))

ggplot() +
  geom_line(data = temp_range, aes(x = as.Date(Date_day), y = range), color = "black", size = 1) +
  geom_line(data = combined_temp, aes(x = as.Date(Date_day), y = AVGtemp, colour = Source), size = 1) +
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "1 month", name = "Date") +
  scale_y_continuous(name = "Range", breaks = seq(0,10,2),sec.axis = sec_axis(~., name = "Water Temp", labels = scales::number_format())) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +mytheme
