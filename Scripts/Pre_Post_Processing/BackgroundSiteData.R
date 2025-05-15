### Prelim datasets for background info 



## Load libraries 
library(tidyverse)
#library(janitor)



##### Read in Data #### 
MooreaMarch_full <- read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/March2022/Allbiogeochemdata_QC_march_fdom2.csv")
MooreaAugust <- read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Nutrients/Nutrients_Watersampling_Aug21.csv")
MooreaAugust2 <- read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/August2021/Allbiogeochemdata_QC2.csv")

## use this to try and get ambient SW TA and nutrients 
CarbonateChem <- read_csv("https://raw.githubusercontent.com/njsilbiger/MooreaSGD_site-selection/main/Data/March2022/CarbonateChemistry/pHProbe_Data_calculated_POcorrect.csv")

#CarbonateChem2 <- CarbonateChem %>% 
#filter(Tide=="High")


################################################
## figure out values for V, C, and Amb SW for nuts, TA, and pH 
################################################

#### ONLY HAVE OFFSHORE DATA FROM MARCH 
AmbSW <- MooreaMarch_full %>% 
  filter(Plate_Seep == "Offshore")

max(AmbSW$TA, na.rm=TRUE)
min(AmbSW$TA, na.rm=TRUE)






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


SiteData_MinMaxMean <- tribble(~Site, ~Parameter, ~Min, ~Max,
                               "Varari", "pH", "7.73", "NA", 
                               "Cabral", "pH", "6.89", "NA",
                               "Offshore", "pH", "7.92", "8.08", 
                               "Varari", "salinity (psu)", "", "NA", 
                               "Cabral", "salinity (psu)", "", "NA", 
                               "Offshore", "salinity (psu)", "36.29", "36.7", 
                               "Varari", "Nitrates (umol/L)", "279.15", "NA", 
                               "Cabral", "Nitrates (umol/L)", "18.24", "NA", 
                               "Offshore", "Nitrates (umol/L)","0.23", "0.58", 
                               "Varari", "TA (umol/kg-1)", "5393.3", "NA", 
                               "Cabral", "TA (umol/kg-1)", "1755.18", "NA", 
                               "Offshore", "TA (umol/kg-1)", "2342.51", "2386.84", 
                               "Varari", "Silicate (umol/L)", "", "NA", 
                               "Cabral", "Silicate (umol/L)", "", "NA", 
                               "Offshore", "Silicate (umol/L)", "0.85", "3.68") %>% 
  mutate(Site= as.factor(Site)) %>% 
  pivot_longer(cols = c(Min:Max), names_to = "type", values_to = "value")





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


###########################################################
### making bar graphs to illustrate differences ### 
##########################################################
MooreaAugust2_edit <- MooreaAugust2 %>% 
  select(-c(,2:7,10:14,16,24:30))
MooreaMarch_edit <- MooreaMarch_full %>% 
  select(-c(,2:7,10:14,16,24:30))

MooreaAugust_addedOffshore <- MooreaMarch_edit %>%  
  full_join(MooreaAugust2_edit) %>% 
  pivot_longer(cols= Salinity:Ammonia_umolL, names_to = "Parameter") 


Salinitygraph <- MooreaAugust_addedOffshore %>% 
  filter(Parameter=="Salinity") %>% 
  filter(Plate_Seep=="Spring") %>% 
  filter(Location=="Cabral") %>% 
  dplyr::summarize(mean=mean(value))

max(Salinitygraph$value, na.rm=TRUE)




NNgraph <- MooreaAugust_addedOffshore %>% 
  filter(Parameter=="NN_umolL") %>% 
  filter(Plate_Seep=="Spring") %>% 
  ggplot(aes(x=Parameter, 
             y= value)) +
  geom_col() +
  facet_wrap(~Location) + 
  theme_classic() + 
  labs(x = "Nitrates + Nitrites (umol/L)", 
       y= "Value", 
       title = "Nitrates + Nitrites levels at the seeps of both sites") + 
  theme(axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=18), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18), 
        plot.title=element_text(hjust=0.5, ## centers title 
                                size=20))

NNgraph

## make a tibble !!!  that says Offhsore Cabral and V and then max data 
#site=offshore, cabral, varari 
#pivot_longer so can facet by parameter 
# can change color by site 
# sclae_ color_ manual - values=c()


############################## 
### plots for CSUNposium 
###############################


sitecomparison_maxdata <- tribble(~Site, ~Parameter, ~Value, 
                                  "Varari", "pH", "7.73", 
                                  "Cabral", "pH", "6.89", 
                                  "Offshore", "pH", "7.9", 
                                  "Varari", "Nitrates (umol/L)", "279.15", 
                                  "Cabral", "Nitrates (umol/L)", "18.24", 
                                  "Offshore", "Nitrates (umol/L)", "0.58", 
                                  "Varari", "TA (umol/kg-1)", "5393.3", 
                                  "Cabral", "TA (umol/kg-1)", "1755.18", 
                                  "Offshore", "TA (umol/kg-1)", "2386.84") %>% 
  mutate(Site= as.factor(Site))

## nitrates 
SiteComparisons_Nitrates <- sitecomparison_maxdata %>% 
  filter(Parameter=="Nitrates (umol/L)") %>% 
  ggplot(aes(x=Site,
             y=Value, 
             fill=Site)) + 
  geom_col()  + 
  scale_x_discrete(limits = c("Offshore", "Cabral", "Varari")) + 
  scale_fill_manual(values=c("deepskyblue3", "darkgoldenrod2", "firebrick4"), 
                    limits=c("Offshore", "Cabral", "Varari")) +
  theme_classic() + 
  labs(x="Location", 
       y="Nitrates (umol/L)") + 
  theme(axis.text.x=element_text(size=25), 
        axis.text.y=element_text(size=25), 
        axis.title.x=element_text(size=25), 
        axis.title.y=element_text(size=25))

SiteComparisons_Nitrates


### for pH 
SiteComparisons_pH <- sitecomparison_maxdata %>% 
  filter(Parameter=="pH") %>% 
  ggplot(aes(x=Site,
             y=Value, 
             fill=Site)) + 
  geom_col()  + 
  scale_x_discrete(limits = c("Offshore", "Cabral", "Varari")) + 
  scale_fill_manual(values=c("deepskyblue3", "burlywood3", "coral4"), 
                    limits=c("Offshore", "Cabral", "Varari")) +
  theme_classic() + 
  labs(x="Location", 
       y="pH") + 
  theme(axis.text.x=element_text(size=25), 
        axis.text.y=element_text(size=25), 
        axis.title.x=element_text(size=25), 
        axis.title.y=element_text(size=25))

SiteComparisons_pH

## for TA 
SiteComparisons_TA <- sitecomparison_maxdata %>% 
  filter(Parameter=="TA (umol/kg-1)")  %>% 
  ggplot(aes(x=Site,
             y=Value, 
             fill=Site)) + 
  geom_col()  + 
  scale_x_discrete(limits = c("Offshore", "Cabral", "Varari")) + 
  scale_fill_manual(values=c("deepskyblue3", "burlywood3", "coral4"), 
                    limits=c("Offshore", "Cabral", "Varari")) +
  theme_classic() + 
  labs(x="Location", 
       y="TA (umol/kg-1)") + 
  theme(axis.text.x=element_text(size=25), 
        axis.text.y=element_text(size=25), 
        axis.title.x=element_text(size=25), 
        axis.title.y=element_text(size=25))

SiteComparisons_TA



############################## 
### df for thesis defense plots 
###############################

AllChemData_August2 <- AllChemData_August %>% 
  select(!c("Top_Plate_ID":"Plate_Seep", "Time", "DateTime", "M_C":"Lignin_Like")) %>% 
  filter(CowTagID!="CSEEP", CowTagID!="VSEEP", CowTagID!="CSPRING", CowTagID!="Varari_Well") %>% 
  group_by(Location, Tide, Date) %>% 
  filter(Date!="2021-08-06") 

AllChemData_March$Date <- mdy(AllChemData_March$Date)

AllChemData_March2 <- AllChemData_March %>% 
  select(!c("Top_Plate_ID":"Plate_Seep", "Time", "DateTime", "TimeBlock", "M_C":"Lignin_Like")) %>% 
  filter(CowTagID!="CSEEP", CowTagID!="VSEEP", CowTagID!="VRC", CowTagID!="CRC", CowTagID!="CPIT", CowTagID!="CPIT_Bottom", CowTagID!="CSPRING_ROAD", CowTagID!="CSPRING_BEACH", CowTagID!="CSPRING_BEACH2", CowTagID!="VSPRING") %>% 
  filter(TA > 2096) ##outlier TA 

##### tidy and join dfs with percent cover data 

AllChemData_seasonaverage <- AllChemData_August2 %>% 
  full_join(AllChemData_March2)


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
  mutate(Site= as.factor(Site))

View(SiteData_MinMaxMean)

############################## 
### plots for thesis defense plots 
###############################

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
