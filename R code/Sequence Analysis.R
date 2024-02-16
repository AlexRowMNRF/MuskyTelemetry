##Sequence Analysis Script for Alex
{library(tidyverse)
  library(glatos)
  library(TraMineR)
  library(rvg)
  library(officer)
}
data(mvad)
Musky<-read_glatos_detections(file.choose())

#idea to create julian date as a continues variable instaed of repeatable - maybe better way to compare individuals throughout enture study duration
##filtering data
Musky <- false_detections(det=Musky, tf=3600,show_plot=TRUE)
Musky <- Musky[Musky$passed_filter == 1,]

##Musky will be you detection file
Musky$detection_timestamp_utc<-as.POSIXct(Musky$detection_timestamp_utc,tz="UTC",format="%m/%d/%Y %H:%M")
Musky$date<-as.Date(Musky$detection_timestamp_utc, "%d/%m/%Y",tz="UTC")
Musky$MoDay<-format(Musky$detection_timestamp_utc,format = "%m-%d")
Musky$julian.date <- as.numeric(format(Musky$date, "%j"))
str(Musky)

##trimming the dataset and setting up locations
detseq<-subset(Musky,detection_timestamp_utc<"2023-01-01" & detection_timestamp_utc>"2018-12-31") #removing thes years since they dont have complete years worth of data 
unique(detseq$glatos_array)
detseq<-detseq[!(detseq$glatos_array%in% c("MPT")),]
detseq<-detseq %>% 
  group_by(animal_id,date) %>%  
  summarise(MoDay=first(MoDay),
            Station=names(which.max(table(glatos_array))))

#define individual years for analysis
#2019
detseq19<-subset(detseq,date<"2020-01-01" & date>"2018-12-31")
detseq19<-detseq19[,-c(2)]
jd19<-data.frame(MoDay=format(seq(as.Date("2019-01-01"),as.Date("2019-12-31"),"day"),format = "%m-%d"))
fishiddetseq19 <- unique(detseq19$animal_id)
detseq19.2<-jd19

for(i in 1:length(fishiddetseq19)){
  sub1<-detseq19[detseq19$animal_id==fishiddetseq19[i],]
  names(sub1)<-c("animal_id","MoDay",paste(fishiddetseq19[i]))
  detseq19.2<- detseq19.2 %>% left_join(sub1, by="MoDay",keep=FALSE)
}
detseq19.2<-detseq19.2[,c(seq(1,9,2))]
detseq19.2 = setNames(data.frame(t(detseq19.2[,-1])), detseq19.2[,1])

#2020
detseq20<-subset(detseq,date<"2021-01-01" & date>"2019-12-31")
detseq20<-detseq20[,-c(2)]
jd20<-data.frame(MoDay=format(seq(as.Date("2020-01-01"),as.Date("2020-12-31"),"day"),format = "%m-%d"))
fishiddetseq20 <- unique(detseq20$animal_id)
detseq20.2<-jd20

for(i in 1:length(fishiddetseq20)){
  sub1<-detseq20[detseq20$animal_id==fishiddetseq20[i],]
  names(sub1)<-c("animal_id","MoDay",paste(fishiddetseq20[i]))
  detseq20.2<- detseq20.2 %>% left_join(sub1, by="MoDay",keep=FALSE)
}
detseq20.2<-detseq20.2[,c(seq(1,9,2))]
detseq20.2 = setNames(data.frame(t(detseq20.2[,-1])), detseq20.2[,1])

#2021
detseq21<-subset(detseq,date<"2022-01-01" & date>"2020-12-31")
detseq21<-detseq21[,-c(2)]
jd21<-data.frame(MoDay=format(seq(as.Date("2021-01-01"),as.Date("2021-12-31"),"day"),format = "%m-%d"))
fishiddetseq21 <- unique(detseq21$animal_id)
detseq21.2<-jd21

for(i in 1:length(fishiddetseq21)){
  sub1<-detseq21[detseq21$animal_id==fishiddetseq21[i],]
  names(sub1)<-c("animal_id","MoDay",paste(fishiddetseq21[i]))
  detseq21.2<- detseq21.2 %>% left_join(sub1, by="MoDay",keep=FALSE)
}
detseq21.2<-detseq21.2[,c(seq(1,9,2))]
detseq21.2 = setNames(data.frame(t(detseq21.2[,-1])), detseq21.2[,1])

#2022
detseq22<-subset(detseq,date<"2023-01-01" & date>"2021-12-31")
detseq22<-detseq22[,-c(2)]
jd22<-data.frame(MoDay=format(seq(as.Date("2022-01-01"),as.Date("2022-12-31"),"day"),format = "%m-%d"))
fishiddetseq22 <- unique(detseq22$animal_id)
detseq22.2<-jd22

for(i in 1:length(fishiddetseq22)){
  sub1<-detseq22[detseq22$animal_id==fishiddetseq22[i],]
  names(sub1)<-c("animal_id","MoDay",paste(fishiddetseq22[i]))
  detseq22.2<- detseq22.2 %>% left_join(sub1, by="MoDay",keep=FALSE)
}
detseq22.2<-detseq22.2[,c(seq(1,9,2))]
detseq22.2 = setNames(data.frame(t(detseq22.2[,-1])), detseq22.2[,1])

detseq19.2$Year<-2019
detseq20.2$Year<-2020
detseq21.2$Year<-2021
detseq22.2$Year<-2022
detseq19.2 <- detseq19.2 %>%select(Year, everything())
detseq20.2 <- detseq20.2 %>%select(Year, everything())
detseq21.2 <- detseq21.2 %>%select(Year, everything())
detseq22.2 <- detseq22.2 %>%select(Year, everything())

detseq20.2<-detseq20.2[,-c(61)] ##leap year so remove Feb29

detseq4<-as.data.frame(rbind(detseq19.2,detseq20.2,detseq21.2,detseq22.2))
write.csv(detseq4,"./Exported Data/MuskySequence.csv")
save(detseq4,file="MuskySequence.RData")

##I reorder and clean up the datasheet in excel then reload from here
##Seq Data Load From Excel Sheet
{library(TraMineR)
  library(cluster)
  library(zoo)
  library(data.table)
  library(tidyverse)
  library(WeightedCluster)
  library(TraMineRextras)}

SeqData<-read.csv(file = "./Exported Data/MuskySequence.csv", )
SeqData2 <- data.frame(t(apply(SeqData, 1, na.locf)))

mvad.alphab=c("TRR","TNT","TNN", "FILL")
dtc.cpal <- c("blue","green3","red3", "white")
ls_subcustom <- cbind(c(0,1,2,0),
                      c(1,0,1,0),
                      c(2,1,0,0),
                      c(0,0,0,0))

colnames(ls_subcustom) <- c("TRR","TNT","TNN", "FILL")
rownames(ls_subcustom) <- c("TRR","TNT","TNN", "FILL")


mvad.seq <- seqdef(SeqData2, 3:367, xtstep = 30, alphabet = mvad.alphab, cpal = dtc.cpal)
#since I added a null sequence to the data for plotting purposes the code below removes the null values for different grouping factors
SeqData3 <- SeqData2 %>% 
  filter(!animal_id==("DUMMY"))
mvad.seq2 <- mvad.seq[c(1:4,6:9, 11:14,16:19),]
seqIplot(mvad.seq2, border = NA, group = SeqData3$animal_id, with.legend = "right") #not using this plot as full sequence has the same info

##this shows clustering dendogram - how closely related are the sequences
ls_dist.ghd <- seqdist(mvad.seq2, method= "OM", norm = "auto", sm = ls_subcustom)
clusterward_ghd <- agnes(ls_dist.ghd, diss =T, method = "ward")
plot(clusterward_ghd, which.plots = 2,hang=-1)

##these are different visual plots of the sequences - there are lots of other options
seqIplot(mvad.seq, border = NA, with.legend = "right")
ppt<-read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(dml(code = {
    seqIplot(mvad.seq, border = NA, with.legend = "right")
  }), location = ph_location_type(type = "body"))  
print(ppt, paste0('./Exported Data/Figures/Animal ID sequences TEST.pptx'))



#might use plot below
seqIplot(mvad.seq2, border = NA, group = SeqData3$Year, with.legend = "right")
ppt<-read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(dml(code = {
    seqIplot(mvad.seq2, border = NA, group = SeqData3$Year, with.legend = "right")
  }), location = ph_location_type(type = "body"))  
print(ppt, paste0('./Exported Data/Figures/Year sequences[.pptx'))
#end
seqdplot(mvad.seq2, group = SeqData3$animal_id, border = NA)
seqdplot(mvad.seq, group = SeqData2$Year, border = NA)
seqmtplot(mvad.seq, border = NA)
#might use this graph to show use of each area
seqmtplot(mvad.seq2, group = SeqData3$animal_id, border = NA)
ppt<-read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(dml(code = {
    seqmtplot(mvad.seq2, group = SeqData3$animal_id, border = NA)
  }), location = ph_location_type(type = "body"))  
print(ppt, paste0('./Exported Data/Figures/Mean_Time_ID.pptx'))
#end
#might use this graph to show yearly use
seqmtplot(mvad.seq2, SeqData3$Year, border = NA)
ppt<-read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(dml(code = {
    seqmtplot(mvad.seq2, SeqData3$Year, border = NA)
  }), location = ph_location_type(type = "body"))  
print(ppt, paste0('./Exported Data/Figures/Mean_Time_Year.pptx'))
#end
#might use this graph to show proportion of space use
seqmsplot(mvad.seq2, border = NA)
ppt<-read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(dml(code = {
    seqmsplot(mvad.seq2, border = NA)
  }), location = ph_location_type(type = "body"))  
print(ppt, paste0('./Exported Data/Figures/StateFrequency.pptx'))
#end
#might use this graph to show proportion of space use by animal ID
seqmsplot(mvad.seq2, group = SeqData3$animal_id, border = NA)
ppt<-read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(dml(code = {
    seqmsplot(mvad.seq2, group = SeqData3$animal_id, border = NA)
  }), location = ph_location_type(type = "body"))  
print(ppt, paste0('./Exported Data/Figures/StateFrequencyAnimalID.pptx'))
#end
seqmsplot(mvad.seq2, group = SeqData3$Year,border = NA)

seqHtplot(mvad.seq2, border = NA)
seqrplot(mvad.seq2, diss=ls_dist.ghd, coverage=.5)
##this is the dissimilarity matrix used for comparison between and among individuals
dist.om1 <- seqdist(mvad.seq2, method = "OM", sm = ls_subcustom)

dis<-dist.om1
ids<-SeqData3$animal_id
dis2<-rbind(ids,dis)
ids2<-(t(t(c(NA,ids))))
dis2<-cbind(ids2,dis2)
write.csv(dis2,file="./Exported Data/Dissimilarity Matrix.CSV")



################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
Questions 
- are the fish more similar to themselves between the years or between others
- are years the same or are they different
mytheme <- theme(
  axis.ticks = element_line(size = 1, colour = "black"),
  axis.text.x = element_text(size = 16, colour = "black"),
  axis.text.y = element_text(size = 16, colour = "black"),
  axis.title = element_text(size = 18, colour = "black", vjust = -0.5),
  panel.background = element_blank(),
  axis.line = element_line(colour = 'black', size = 1))
library(emmeans)
library(multcomp)
library(ggplot2)
library(multcompView)
library(dplyr)
library(graphics)
#Using the dissimilarity values 

FID_within <- read.csv("./Exported Data/Animal ID Dissimilarity Values.csv", header = TRUE)
FID_within <- FID_within %>%
  rename(DissValue = ï..DissValue) %>% 
  mutate(Perc_Sim = (1-(DissValue / 730))*100)

hist(FID_within$DissValue)
wilcox.test(Perc_Sim ~ Comp, data = FID_within, alternative = "less") #use this to hihglight that there is a signifigant difference in within vs between such that between has a lower similarity than within

my_plot <- FID_within %>% 
  ggplot(aes(x = Comp, y = Perc_Sim)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1) +
  geom_boxplot(fill = "lightgrey", color = "black", width = 0.5, lwd = 1 ) +
  scale_y_continuous(name = "Sequence Similarity (%)") +
  scale_x_discrete(name = "") +
  mytheme

ppt <- read_pptx(file.choose())#need to have a powerpoint loaded..need to make one ahead of time 
ppt <- ppt %>% ph_with(dml(ggobj=my_plot),
                             location = ph_location_type(type = "body"))
## save/download ppt 
print(ppt, paste0('btwVSwithin.pptx'))



##############
####################

FID_year <- read.csv("./Exported Data/Yearly Dissimilarity Values.csv")
FID_year <- FID_year%>%
  rename(DissValue = ï..DisVal) %>% 
  mutate(Perc_Sim = (1-(DissValue / 730))*100)
hist(FID_year$DissValue)

year_plot <- FID_year %>% 
  ggplot(aes(x = Year, y = Perc_Sim)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1) +
  geom_boxplot(fill = "lightgrey", color = "black", width = 0.5, lwd = 1 ) +
  scale_y_continuous(name = "Sequence Similarity (%)", breaks = seq(40, 100, 20), limits = c(40,100)) +
  scale_x_discrete(name = "") +
  mytheme;year_plot
str(FID_year)
ppt <- read_pptx(file.choose())#need to have a powerpoint loaded..need to make one ahead of time 
ppt <- ppt %>% ph_with(dml(ggobj=year_plot),
                       location = ph_location_type(type = "body"))
## save/download ppt 
print(ppt, paste0('btwVSwithin.pptx'))
# Perform Kruskal-Wallis test
kruskal_test_result <- kruskal.test(Perc_Sim ~ Year, data = FID_year)

# Print the test result
print(kruskal_test_result)

