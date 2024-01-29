#
# HEADER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# Author: Colin Lake
# Email:  colin.lake@ontario.ca
# Date: 2023-12-15
#
# Script Description: analysis of Belleville WTP temp data
#
# Notes: time frame to coincide with tagged muskellunge (2018 - current)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Clear working history
rm(list=ls())

library(readxl)
library(tidyr)
library(tidyverse)

excel_file <- "muskellunge/data/temp/RAW TEMP & LEVEL  MONTHLY.xls"

# Specify the starting and ending rows for the region of the sheet to import
start_row <- 6
end_row <- 37

# Read each sheet of the Excel file as individual data frames
all_sheets <- excel_sheets(excel_file)

# these years use different formatting - PITA - just drop them.
all_sheets<-all_sheets[all_sheets!="2002"]
all_sheets<-all_sheets[all_sheets!="2003"]

# Initialize an empty list to store data frames
sheet_data_frames <- list()

# Loop through each sheet and import the specified region as a data frame
for (sheet_name in all_sheets) {
  sheet_data <- read_excel(excel_file, sheet = sheet_name, range = "A6:M37")
  sheet_data_frames[[sheet_name]] <- sheet_data
}

# Access individual data frames using sheet names

# Combine all data frames into a single data frame
combined_df <- do.call(rbind, sheet_data_frames)

# Add a new column for element names
combined_df$year <- rep(names(sheet_data_frames), sapply(sheet_data_frames, nrow))

data_long <- combined_df %>%
  pivot_longer(
    cols = JANUARY:DECEMBER,
    names_to = "MONTH",
    values_to = "TEMP",
    values_drop_na = FALSE
  )

data_long$year <- as.integer(data_long$year)

# Function to convert month name to numeric
month_to_numeric <- function(month_name) {
  months <- c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", 
              "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
  
  month_index <- match(toupper(month_name), toupper(months))
  
  if (!is.na(month_index)) {
    return(month_index)
  } else {
    stop("Invalid month name")
  }
}

data_long$month_num <- month_to_numeric(data_long$MONTH)

df <- janitor::clean_names(data_long)

df$date <- as.Date(with(df, paste(year, month_num, day, sep = "-")), "%Y-%m-%d")

df <- df %>% select(date, temp)

#plot 

df %>% 
  mutate(year = year(date),
         date = yday(date)) %>% 
  ggplot(aes(date, temp,
             group = year,
             colour = year)) +
  geom_line() +
  geom_hline(yintercept = 25)+
  #geom_hline(yintercept = 17)+
  scale_colour_viridis_c() +
  labs(title = "Daily water temperature  - Belleville WTP",
       subtitle = "Temp max (25°C) from Cole and Bettoli (2014).",
       x = "Day of year",
       y = "temp",
       colour = "Year") +
       #caption = ice_cite) +
  theme(text = element_text(size = 15))
