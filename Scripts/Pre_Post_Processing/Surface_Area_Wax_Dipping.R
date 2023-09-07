########## Surface Area measurements ###########################
# Calculating surface area from wax dipping coral fragments
# Created by Jamie Kerlin
# Created on 2022_03_20
# Updated by Hannah Merges, on 2023-06-02
##########################################################################

###### Load libraries #######################################################
library(tidyverse)
library(here)

######## Load data ######################################################
## read in CSVs from anyone who wax dipped/calculated surface area curves 
sa <-  read_csv(here("Data", "Wax_Dipping", "Cabral_Varari_coralsurfacearea.csv")) # %>% 
 # drop_na()

## dc <- read_csv(here("Data","Wax_Dipping","donor_colony_info.csv")) #%>%
  #select(!CowTagID) %>%
  #rename(FragmentID = Donor_Colony, CowTagID = PlateID)
  # only need if have Jamie's set up 

hannah_slope <- 0.0002 #slope of Hannah's calibration curve, just take off the excel sheet 
hannah_intercept <- -0.0293 #intercept of Hannah's calibration curve
#maya_slope <- .0293 #slope of Maya's calibration curve
#maya_intercept <- -0.0887 #intercept of Maya's calibration curve

# wax dipping found y = mx + c
# to estimate surface area of coral, use x = (y-c)/m
# where y is the weight of the wax
# I did a calibration curve, so calculating each person's coral fragments with their own calibration curve

### Calculating surface area ###############################################
sa_corals <- sa %>%
 mutate(Weight_wax_g = (Weight_after_wax - Weight_before_wax)) #calculate weight of wax 

sa_hannah_coral <- sa_corals %>%
  filter(Person_Measuring == "Hannah") %>%
  mutate(SA_cm2 = ((Weight_wax_g-hannah_intercept)/hannah_slope)) #calculate surface area of fragments hannah measured

#sa_maya_coral <- sa_corals %>%
  #filter(Person_measuring == "Maya") %>%
  #mutate(Surface_area_cm2 = ((Weight_wax_g-sabrina_c)/sabrina_m)) #calculate surface area of fragments maya measured

#sa_combined <- rbind(sa_hannah_coral, sa_maya_coral) 

