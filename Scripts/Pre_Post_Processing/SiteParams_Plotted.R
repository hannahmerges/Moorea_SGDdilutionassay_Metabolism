############ Env Params Plotted for Site Description of Varari and Cabral 
############ Created by: Hannah Merges
############ Updated on: 11/14/2024 

## This script was used to create preliminary plots to assess the environmental data collected at both sites, comparing groundwater and seawater. 

################## 
## load libraries 
################## 
library(tidyverse)
#library(janitor)

################## 
## read in data
################## 
AllChemData_March <- read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/March2022/Allbiogeochemdata_QC_march_fdom2.csv")
#MooreaAugust <- read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Nutrients/Nutrients_Watersampling_Aug21.csv")
MooreaAugust2 <- read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Allbiogeochemdata_QC.csv")


############################## 
### tidy and average both seasons 
###############################

AllChemData_August2 <- MooreaAugust2 %>% 
  dplyr::select(!c("Top_Plate_ID":"Plate_Seep", "Time", "DateTime", "M_C":"Lignin_Like")) %>% 
  filter(CowTagID %in% c("CSEEP", "VSEEP","CSPRING","Varari_Well")) %>% 
  group_by(Location, Tide, Date) %>% 
  filter(Date!="2021-08-06") 


##
AllChemData_March$Date <- mdy(AllChemData_March$Date)

AllChemData_March2 <- AllChemData_March %>% 
  select(!c("Top_Plate_ID":"Plate_Seep", "Time", "DateTime", "TimeBlock", "M_C":"Lignin_Like")) %>% 
  filter(CowTagID %in% c("CSEEP", "VSEEP","CSPRING","Varari_Well", "CPIT", "CRC", "VRC", "CPIT_Bottom","CSPRING_ROAD", "CSPRING_BEACH","CSPRING_BEACH2","VSPRING")) %>% 
  filter(TA > 2096) ##outlier TA 

### join seasons together 
joined_sitedata <- AllChemData_August2 %>% 
  full_join(AllChemData_March2)

### average seasons together 
RawGW_seasonavg <- joined_sitedata %>% 
  dplyr::select(!c("Date", "Tide", "Day_Night")) %>% 
  # filter(!CowTagID %in% c("CSEEP","VSEEP","Reef_Ambient_1","Reef_Ambient_2","Reef_Ambient_3","Reef_SGD_1","Reef_SGD_2", "Reef_SGD_3","Sand_Ambient_1","Sand_Ambient_2","Sand_Ambient_3","Sand_SGD_1","Sand_SGD_2","RockWall_SGD_1", "RockWall_SGD_2", "RockWall_SGD_3")) %>% 
  group_by(Location) %>% 
  dplyr::summarise('Mean Salinity'=mean(Salinity, na.rm=TRUE), 
                   'SE Salinity' = sd(Salinity) / sqrt(length(Salinity)), 
                   'Min Salinity' = min(Salinity, na.rm=TRUE), 
                   'Max Salinity' = max(Salinity, na.rm=TRUE), 
                   'Mean pH'=mean(pH, na.rm=TRUE),
                   'SE pH' = sd(pH) / sqrt(length(pH)), 
                   'Min pH' = min(pH, na.rm=TRUE),
                   'Max pH' = max(pH, na.rm=TRUE), 
                   'Mean Silicate'=mean(Silicate_umolL, na.rm=TRUE),
                   'SE Silicate' = sd(Silicate_umolL) / sqrt(length(Silicate_umolL)), 
                   'Min Silicate'=min(Silicate_umolL, na.rm=TRUE), 
                   'Max Silicate' = max(Silicate_umolL, na.rm=TRUE), 
                   'Mean NN'=mean(NN_umolL, na.rm=TRUE), 
                   'SE NN' = sd(NN_umolL) / sqrt(length(NN_umolL)),
                   'Min NN' = min(NN_umolL, na.rm=TRUE), 
                   'Max NN' = max(NN_umolL, na.rm=TRUE),
                   'Mean TA'=mean(TA, na.rm=TRUE), 
                   'SE TA' = sd(TA) / sqrt(length(TA)),
                   'Min TA' = min(TA, na.rm=TRUE), 
                   'Max TA' = max(TA, na.rm=TRUE)) 

############################## 
### tidy and average both seasons 
###############################

SiteData_MinMaxMean <- tribble(~Site, ~Parameter, ~Min, ~Max,
                               "Varari", "pH", "7.18", "7.18", 
                               "Cabral", "pH", "7.24", "7.24",
                               "Offshore", "pH", "7.92", "8.08", 
                               "Varari", "salinity (psu)", "2.89", "2.89", 
                               "Cabral", "salinity (psu)", "0.44", "0.44", 
                               "Offshore", "salinity (psu)", "36.29", "36.7", 
                               "Varari", "Nitrates (umol/L)", "32.63", "32.63", 
                               "Cabral", "Nitrates (umol/L)", "19.77", "19.77", 
                               "Offshore", "Nitrates (umol/L)","0.23", "0.58", 
                               "Varari", "TA (umol/kg-1)", "3817.70", "3817.70", 
                               "Cabral", "TA (umol/kg-1)", "1345.467", "1345.467", 
                               "Offshore", "TA (umol/kg-1)", "2342.51", "2386.84", 
                               "Varari", "Silicate (umol/L)", "713.06", "713.06", 
                               "Cabral", "Silicate (umol/L)", "737.49", "737.49", 
                               "Offshore", "Silicate (umol/L)", "0.85", "3.68") %>% 
  mutate(Site= as.factor(Site)) %>% 
  pivot_longer(cols = c(Min:Max), names_to = "type", values_to = "value")

View(SiteData_MinMaxMean)

############################## 
### plots for thesis defense plots 
###############################


Salinity_plot <- SiteData_MinMaxMean %>% 
  filter(Parameter== "salinity (psu)") %>% 
  ggplot(aes(x = Site, 
             y = value)) + 
  geom_point(aes(color = Site, 
                 size = 4,
                 shape=type)) + 
  # scale_color_manual(values = c("Min" = "blue", "Max" = "red")) + 
  scale_color_manual(values=c("deepskyblue3", "darkgoldenrod2", "firebrick4"), 
                     limits=c("Offshore", "Cabral", "Varari"), 
                     guide="none") + 
  scale_
labs(x = "Site",
     y = "Value", 
     color = "Type") +
  theme_bw() 

Salinity_plot
ggsave(here::here("Outputs", "PaperFigures","RawGW_salinity.jpg"), 
       width=10, height=7)









#### TA 
SiteComparisons_TA <- sitecomparison_maxdata %>% 
  filter(Parameter=="TA (umol/kg-1)")  %>% 
  ggplot(aes(x=Site,
             y=Value, 
             fill=Site)) + 
  geom_col()  + 
  scale_x_discrete(limits = c("Reef Crest", "Cabral", "Varari")) + 
  scale_fill_manual(values=c("deepskyblue3", "burlywood3", "coral4"), 
                    limits=c("Reef Crest", "Cabral", "Varari"), 
                    guide="none") +
  theme_classic() + 
  labs(x="Location",
       y = expression(TA~(mu*mol~kg^-1))) +
  theme(axis.text.x=element_text(size=25), 
        axis.text.y=element_text(size=25), 
        axis.title.x=element_text(size=25), 
        axis.title.y=element_text(size=25))


#### Salinity 


#### NN 



#### pH





###
###

Varari_March <- MooreaMarch_full %>% 
  filter(Location== "Varari") %>% 
  filter(Plate_Seep!= "Offshore")

V_August <- MooreaAugust2 %>% 
  filter(Location=="Varari") 

### 
max(Varari_March$TA, na.rm=TRUE)
min(Varari_March$TA, na.rm=TRUE)
mean(Varari_March$TA, na.rm=TRUE)

max(Varari_March$NN_umolL, na.rm=TRUE)
min(Varari_March$NN_umolL, na.rm=TRUE)
mean(Varari_March$NN_umolL, na.rm=TRUE)

max(Varari_March$pH, na.rm=TRUE)
min(Varari_March$pH, na.rm=TRUE)
mean(Varari_March$pH, na.rm=TRUE)
#
max(V_August$NN_umolL, na.rm=TRUE)
max(V_August$TA, na.rm=TRUE)
max(V_August$pH, na.rm=TRUE)
min(V_August$pH, na.rm=TRUE)

###
###

Cabral_March <- MooreaMarch_full %>% 
  filter(Location== "Cabral") %>% 
  filter(Plate_Seep!= "Offshore")
#
C_August <- MooreaAugust2 %>% 
  filter(Location=="Cabral")

max(Cabral_March$TA, na.rm=TRUE)
min(Cabral_March$TA, na.rm=TRUE)
mean(Cabral_March$TA, na.rm=TRUE)

max(Cabral_March$NN_umolL, na.rm=TRUE)
min(Cabral_March$NN_umolL, na.rm=TRUE)
mean(Cabral_March$NN_umolL, na.rm=TRUE)

max(Cabral_March$pH, na.rm=TRUE)
min(Cabral_March$pH, na.rm=TRUE)
mean(Cabral_March$pH, na.rm=TRUE)

#
max(C_August$TA, na.rm=TRUE)
max(C_August$NN_umolL, na.rm=TRUE)
max(C_August$pH, na.rm=TRUE)
min(C_August$pH, na.rm=TRUE)


sd(AmbSW$Silicate_umolL) / sqrt(length(AmbSW$Silicate_umolL))




SiteData_MinMaxMean <- tribble(~Site, ~Parameter, ~Min, ~Max, ~Mean, ~SE,
                               "Varari", "pH", "7.73", "NA", "NA", "NA",  
                               "Cabral", "pH", "6.89", "NA", "NA", "NA",
                               "Offshore", "pH", "7.92", "8.08", "8.01", "0.0098",
                               "Varari", "salinity (psu)", "7.73", "NA", "NA", "NA",
                               "Cabral", "salinity (psu)", "6.89", "NA", "NA", "NA", 
                               "Offshore", "salinity (psu)", "7.9", "NA", "NA", "NA", 
                               "Varari", "Nitrates (umol/L)", "279.15", "NA", "NA", "NA",
                               "Cabral", "Nitrates (umol/L)", "18.24", "NA", "NA", "NA",
                               "Offshore", "Nitrates (umol/L)","0.23", "0.58", "0.39", "NA", 
                               "Varari", "TA (umol/kg-1)", "5393.3", "NA", "NA", "NA",
                               "Cabral", "TA (umol/kg-1)", "1755.18", "NA", "NA", "NA",
                               "Offshore", "TA (umol/kg-1)", "2342.51", "2386.84", "2366.54", "2.66", 
                               "Varari", "Silicate (umol/L)", "5393.3", "NA", "NA", "NA",
                               "Cabral", "Silicate (umol/L)", "1755.18", "NA", "NA", "NA",
                               "Offshore", "Silicate (umol/L)", "0.85", "3.68", "2.00", "NA") %>% 
  mutate(Site= as.factor(Site)) %>% 
  pivot_longer(cols = c(Min:SE), names_to = "type", values_to = "value")
