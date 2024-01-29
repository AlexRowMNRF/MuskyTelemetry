library(lubridate)
library(tidyr)
library(scales)
library(ggplot2)
library(dplyr)
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
  mutate(Source = "TRR")

#Need to get teh data in the same format and then combine the datasets 
month_names <- tolower(month.name)

BWT <- BWTTemp %>%
  mutate(Month = tolower(Month),
         Date_day = as.Date(paste(Year, match(Month, month_names), Day, sep = "-")),
         AVGtemp = Temp,
         Source = "BWTP") %>%
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
  filter(AVGtemp >= 7 & AVGtemp <= 17) %>%
  slice_max(order_by = Date_day) %>%
  pull(Date_day)

ggplot() +
  geom_rect(aes(xmin = as.Date(start_date), xmax = as.Date(end_date), ymin = -Inf, ymax = Inf), 
            fill = "green", alpha = 0.3) +
  geom_rect(aes(xmin = as.Date(start_date2), xmax = as.Date(end_date2), ymin = -Inf, ymax = Inf), 
            fill = "darkgrey", alpha = 0.3) +
  geom_line(data = temp_range, aes(x = as.Date(Date_day), y = range), color = "black", size = 1) +
  geom_line(data = combined_temp, aes(x = as.Date(Date_day), y = AVGtemp, colour = Source), size = 1) +
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "1 month", name = "Date") +
  scale_y_continuous(name = "Range", breaks = seq(0,10,2),sec.axis = sec_axis(~., name = "Water Temp", breaks = seq(0,30,5), labels = scales::number_format())) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +mytheme
