### Prelim datasets for background info 



## Load libraries 
library(tidyverse)
library(janitor)



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
AmbSW <- MooreaMarch_full %>% 
  filter(Plate_Seep == "Offshore")
max(AmbSW$TA, na.rm=TRUE)
min(AmbSW$TA, na.rm=TRUE)
mean(AmbSW$TA, na.rm=TRUE)

max(AmbSW$NN_umolL, na.rm=TRUE)
min(AmbSW$NN_umolL, na.rm=TRUE)
mean(AmbSW$NN_umolL, na.rm=TRUE)

max(AmbSW$pH, na.rm=TRUE)
min(AmbSW$pH, na.rm=TRUE)
mean(AmbSW$pH, na.rm=TRUE)

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


###########################################################
### making bar graphs to illustrate differences ### 
##########################################################
MooreaAugust2_edit <- MooreaAugust2 %>% 
  select(-c(,2:7,10:16,24:30))
MooreaMarch_edit <- MooreaMarch_full %>% 
  select(-c(,2:7,10:16,24:30))

MooreaAugust_addedOffshore <- MooreaMarch_edit %>%  
  full_join(MooreaAugust2_edit) %>% 
  pivot_longer(cols= TA:Ammonia_umolL, names_to = "Parameter") 



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



pHgraph <- MooreaAugust_addedOffshore %>% 
  filter(Parameter=="pH") %>% 
  filter(Plate_Seep=="Spring") %>% 
  ggplot(aes(x=Parameter, 
             y= value)) +
  geom_col() +
  facet_wrap(~Location) + 
  theme_classic() + 
  labs(x = "pH", 
       y= "Value", 
       title = "pH levels at the seeps of both sites") + 
  theme(axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=18), 
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18), 
        plot.title=element_text(hjust=0.5, ## centers title 
                                size=20))

pHgraph
