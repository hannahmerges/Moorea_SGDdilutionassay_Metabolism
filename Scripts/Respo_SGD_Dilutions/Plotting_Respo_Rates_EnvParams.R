###### Plots for Respo - using rates and env parameters ####### 
### Created by: Hannah Merges
#### Last updated on: 2023-04-08


#Read in required libraries
##### Include Versions of libraries
library(segmented)
library(plotrix)
library(gridExtra)
library(LoLinR)
library(lubridate)
library(chron)
library(patchwork)
library(tidyverse)
library(here)
library(PNWColors)
library(ggrepel)
#library(PerformanceAnalytics)
library(reshape2)
library(viridis)
library(car)
library(GGally)
library(corrplot)

########## Read in the RespoR df created from the for loop ###############

RespoR <- read_csv(here("Data","RespoFiles","Respo_R.csv"))

RespoR2 <- RespoR %>%
  #drop_na(FileID_csv) %>% # drop NAs
  left_join(Sample_Info) %>% # Join the raw respo calculations with the metadata in Sample Info 
  #mutate(Ch.Volume.ml = ifelse(is.na(volume_ml),ch.vol,ch.vol-volume_ml)) %>% # add 6 L for volume of all blanks and subtract org volume from chamber vol for all else
  mutate(Ch.Volume.mL = 600-volume_mL) %>% # hannah changed all this volume stuff to match my project
  mutate(Ch.Volume.L = Ch.Volume.mL * 0.001) %>% # mL to L conversion
  mutate(umol.sec = umol.L.sec*Ch.Volume.L) %>% #Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
  mutate_if(sapply(., is.character), as.factor) %>% #convert character columns to factors
  mutate(colony_number= as.factor(colony_number), 
         date_block= factor(date)) #make the blank column a factor

########## average all the blanks across all the days to get one value per dilution, then separate by site ########

#RespoR_Normalized_AvgBlanks <- RespoR2 %>% 
#  filter(colony_number== "BLANK") %>%
#  group_by(SGD_number, light_dark, site) %>%
 # summarise(umol.sec = mean(umol.sec, na.rm=TRUE)) %>% # get mean value of blanks per each dilution 
 # dplyr::select(blank.rate = umol.sec, site) #%>%  ## rename the blank column 
#ungroup() %>%
# dplyr::select(blank.rate, light_dark, colony_number, SGD_dil, site, SGD_number) #keep only what we need

#########################################################
################### FIX THIS CODE CHUNK BELOW ###################
##########################################################

RespoR_Normalized_AvgBlanks <- RespoR2 %>% 
  group_by(SGD_number, light_dark, date_block, colony_number, site) %>% 
  filter(colony_number== "BLANK") %>%
  summarise(umol.sec = mean(umol.sec, na.rm=TRUE)) %>% # get mean value of blanks per each dilution 
  dplyr::select(blank.rate = umol.sec, site) %>%  ## rename the blank column 
  ungroup() %>% 
  dplyr::select(SGD_number, light_dark, blank.rate, date_block, site) %>%
  right_join(RespoR2) %>% # join with the respo data
  #arrange(FileID_csv) %>% 
  mutate(umol.sec.corr = umol.sec - blank.rate, # subtract the blank rates from the raw rates #### HOW IS THIS NORMALIZING THE DATA IS EVERYTHING IS JUST GOING TO BE 0 #####   
         mmol.cm2.hr = 0.001*(umol.sec.corr*3600)/SA_cm2, # convert to mmol g-1 hr-1
         mmol.cm2.hr_uncorr = 0.001*(umol.sec*3600)/SA_cm2) %>% 
  filter(colony_number!="BLANK") %>% # remove the Blank data
  #ungroup() %>%
  dplyr::select(date, sample_ID, light_dark, run_block, SA_cm2, run_block, mmol.cm2.hr, chamber_channel, 
                Temp.C, mmol.cm2.hr_uncorr, colony_number, salinity, pH, site, SGD_number)

### plot the averaged blanks to see if it is working properly ### 
plot_Blank_Rates_Averaged <- RespoR_Normalized_AvgBlanks %>% 
  ggplot(aes(x=SGD_number, 
             y=mmol.cm2.hr_uncorr)) +
  scale_x_continuous(trans="log10") +
  geom_point() + 
  facet_wrap(site~light_dark)

##there are still multiple points even tho it is supposed to be taking an average 


########################################
####### using the averaged blanks data - calculate R and GP ################
#########################################

# make the respiration values positive (pull out data for dark treatments)

RespoR_Normalized_dark <- RespoR_Normalized_AvgBlanks %>% 
  filter(light_dark == "DARK") %>% 
  mutate(mmol.cm2.hr = mmol.cm2.hr*-1,
         mmol.cm2.hr_uncorr = mmol.cm2.hr_uncorr*-1) %>% 
  mutate(mmol.cm2.hr = ifelse(mmol.cm2.hr < 0, 0, mmol.cm2.hr), # for any values below 0, make 0
         mmol.cm2.hr_uncorr = ifelse(mmol.cm2.hr_uncorr < 0, 0, mmol.cm2.hr_uncorr)) %>% 
  mutate(P_R = "R") # all dark run rates get R for respiration

# all light run rates get NP for net photosynthesis
RespoR_Normalized_light <- RespoR_Normalized_AvgBlanks %>% 
  filter(light_dark == "LIGHT") %>% 
  mutate(mmol.cm2.hr = ifelse(mmol.cm2.hr < 0, 0, mmol.cm2.hr), # for any values below 0, make 0
         mmol.cm2.hr_uncorr = ifelse(mmol.cm2.hr_uncorr < 0, 0, mmol.cm2.hr_uncorr)) %>% 
  mutate(P_R = "NP")

# rejoin data into single df
RespoR_Normalized2 <- full_join(RespoR_Normalized_light, RespoR_Normalized_dark) #%>% 
#drop_na(mmol.gram.hr) # removes anticipated sampleID's that were not actually run


#make column for GP and group by fragment ID and salinity to keep R and NP together
RespoR_NormalizedGP <- RespoR_Normalized2 %>% 
  group_by(colony_number, SGD_number, site, salinity, pH) %>% 
  summarize(mmol.cm2.hr = sum(mmol.cm2.hr),
            mmol.cm2.hr_uncorr = sum(mmol.cm2.hr_uncorr), # NP + R = GP
            #Temp.C = mean(Temp.C)
  ) %>% 
  mutate(P_R="GP") %>% # Label for Gross Photosynthesis
  mutate(light_dark = "LIGHT") %>% 
  mutate(mmol.cm2.hr = ifelse(mmol.cm2.hr < 0, 0, mmol.cm2.hr), # for any values below 0, make 0
         mmol.cm2.hr_uncorr = ifelse(mmol.cm2.hr_uncorr < 0, 0, mmol.cm2.hr_uncorr))

# rejoin for full df with NP, R, and GP rates
RespoR_Normalized_Full <- RespoR_Normalized2 %>% 
  dplyr::select(colony_number, pH, SGD_number, site, salinity, light_dark, P_R, mmol.cm2.hr, mmol.cm2.hr_uncorr) %>% 
  full_join(RespoR_NormalizedGP)


write_csv(RespoR_Normalized_Full , here("Data","RespoFiles","Respo_RNormalized_AllRates.csv"))  



######################################
####### new plots with edits from 08/02/2023
## updated: 08/30/2023
#########################################
### edits 08/02/2023 = using avg blank data, correcting colonies 5 and 6 from Cabral, and changing titles of plots 
### edits 08/30/2023 = make sure averages are done properly. 

RespoR_Normalized_Full <- read_csv(here("Data","RespoFiles","Respo_RNormalized_AllRates.csv"))

my_pal <- pnw_palette(name="Starfish",n=2,type="discrete")

##### Respiration rates with dilutions and u.mol ######### 
RespRates_SGDdil <- RespoR_Normalized_Full %>%
  filter(P_R == "R") %>%
  ggplot(aes(x=SGD_number, 
             y=mmol.cm2.hr, 
             color=colony_number)) +
  facet_wrap(colony_number~site, scales = "free_y") +
  geom_point() +
  scale_x_continuous(trans="log10") +
  labs(title="Respiration Normalized to Avg Blanks for SGD Dils")
ggsave(here("Outputs", "RespoOutput","RespRates_SGDdils.jpg"), 
       width = 10, height = 10)

##### GP Rates based on SGD Dils #########
RatesPlot_GP <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=SGD_number, 
             y=mmol.cm2.hr, 
             color=colony_number)) +
  facet_wrap(colony_number~site, scales = "free_y") +
  geom_point() +
  scale_x_continuous(trans="log10") +
  labs(title="GP Rates for SGD Dils")
ggsave(here("Outputs", "RespoOutput","GPRates_SGDdils.jpg"), 
       width = 10, height = 10)


### is there a way to get all 8 replicates onto one graph for each site ?? 


RatesPlot_GP_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=salinity, 
             y=mmol.gram.hr, 
             color=colony_number)) +
  facet_wrap(colony_number~site, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title="Respiration with Environmental Parameters")



RatesPlot_R_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "R") %>%
  ggplot(aes(x=salinity, 
             y=mmol.gram.hr, 
             color=colony_number)) +
  geom_smooth(method = "lm")+
  facet_wrap(colony_number~site, scales = "free") +
  geom_point() +
  labs(title="Respiration with Environmental Parameters")

RatesPlot_R_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=SGD_number, 
             y=salinity, 
             color=colony_number)) +
  scale_x_continuous(trans="log10") +
  facet_wrap(colony_number~site, scales = "free") +
  geom_point() +
  geom_line()+
  labs(title="Respiration with Environmental Parameters")


RatesPlot_R_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=SGD_number, 
             y=pH, 
             color=colony_number)) +
  scale_x_continuous(trans="log10") +
  facet_wrap(colony_number~site, scale = "free") +
  geom_line() +
  geom_point()
labs(title="Respiration with Environmental Parameters")












################

### new plots for meeting on 08/02/2023 ### 

### blank rates ### 
## NOT AVERAGED ## 
Blank_Rates_R <- RespoR_Normalized_Blanks %>% 
  ggplot(aes(x=SGD_number, 
             y=blank.rate)) +
  scale_x_continuous(trans="log10") +
  geom_point() + 
  facet_wrap(site~light_dark)

ggsave(here("Outputs", "RespoOutput","BlankRates_NotAveraged.jpg"))


### blank rates AVERAGED ACROSS DAYS ### 

RespoR_Normalized_AvgBlanks <- RespoR2 %>% 
  filter(colony_number== "BLANK") %>%
  group_by(SGD_number, light_dark, site) %>%
  summarise(umol.sec = mean(umol.sec, na.rm=TRUE)) %>% # get mean value of blanks per each dilution 
  dplyr::select(blank.rate = umol.sec, site) #%>%  ## rename the blank column 
#ungroup() %>%
# dplyr::select(blank.rate, light_dark, colony_number, SGD_dil, site, SGD_number) #keep only what we need


Blank_Rates_Averaged <- RespoR_Normalized_AvgBlanks %>% 
  ggplot(aes(x=SGD_number, 
             y=blank.rate)) +
  scale_x_continuous(trans="log10") +
  geom_point() + 
  facet_wrap(site~light_dark)


#################################
############# New plots - for 8/2/23 ##########
###################################
RespoR_Normalized_Full <- read_csv(here("Data","RespoFiles","Respo_RNormalized_AllRates.csv"))

my_pal <- pnw_palette(name="Starfish",n=2,type="discrete")


#### R with dilutions and u.mol 
RatesPlot_R <- RespoR_Normalized_Full %>%
  filter(P_R == "R") %>%
  ggplot(aes(x=SGD_number, 
             y=mmol.gram.hr, 
             color=colony_number)) +
  facet_wrap(colony_number~site, scales = "free_y") +
  geom_point() +
  scale_x_continuous(trans="log10") +
  labs(title="Respiration")
ggsave(here("Outputs", "RespoOutput","Respiration_plot_08022023.jpg"), 
       width = 10, height = 10)


### GP with dilutions and u.mol 
RatesPlot_GP <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=SGD_number, 
             y=mmol.gram.hr, 
             color=colony_number)) +
  facet_wrap(colony_number~site, scales = "free_y") +
  geom_point() +
  scale_x_continuous(trans="log10") +
  labs(title="GP")
ggsave(here("Outputs", "RespoOutput","GP_plot_08022023.jpg"), 
       width = 10, height = 10)




#### R and GP with salinity and other env parameters ######
RatesPlot_GP_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=salinity, 
             y=mmol.gram.hr, 
             color=colony_number)) +
  facet_wrap(colony_number~site, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title="Respiration with Environmental Parameters")



RatesPlot_R_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "R") %>%
  ggplot(aes(x=salinity, 
             y=mmol.gram.hr, 
             color=colony_number)) +
  geom_smooth(method = "lm")+
  facet_wrap(colony_number~site, scales = "free") +
  geom_point() +
  labs(title="Respiration with Environmental Parameters")

RatesPlot_R_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=SGD_number, 
             y=salinity, 
             color=colony_number)) +
  scale_x_continuous(trans="log10") +
  facet_wrap(colony_number~site, scales = "free") +
  geom_point() +
  geom_line()+
  labs(title="Respiration with Environmental Parameters")


RatesPlot_R_EnvParameters <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  ggplot(aes(x=SGD_number, 
             y=pH, 
             color=colony_number)) +
  scale_x_continuous(trans="log10") +
  facet_wrap(colony_number~site, scale = "free") +
  geom_line() +
  geom_point()
labs(title="Respiration with Environmental Parameters")




###################################################
######### correlation plots between environmental parameters #########
######################################################

## data 
corr_matrix_envparams <- BioData[-c(1:6,13:17)] 

##visualize it 
ggcorr(corr_matrix_envparams, method = c("everything", "pearson")) 

###############################
### Dani B's methods #######
cor_mat <- round(cor(corr_matrix_envparams),4)
head(cor_mat)

#melt the correlation matrix means it reassembles data frame to be more effective to complete corr matrix
#to long format
melted_cormat <- melt(cor_mat)
head(melted_cormat)

#visulaize the correlation matrix in general
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower and upper triangle of the correlation matrix
#Note that, a correlation matrix has redundant information. We’ll use the functions below to set half of it to NA
get_lower_tri<-function(cor_mat){
  cormat[upper.tri(cor_mat)] <- NA
  return(cor_mat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_mat){
  cor_mat[lower.tri(cor_mat)]<- NA
  return(cor_mat)
}

#apply upper tri calculation to graphc
upper_tri <- get_upper_tri(cor_mat)
upper_tri

#melt the correlation matrix
#melt the correlation data and drop the rows with NA values 
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#heatmap of correlation matrix
#negative correlations are in purple color and positive correlations in red
#scale_fill_gradient2 is used with the argument limit = c(-1,1) as correlation coefficients range from -1 to 1
#coord_fixed() : this function ensures that one unit on the x-axis is the same length as one unit on the y-axis
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "midnightblue", high = "firebrick4", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

melted_cormat <- melted_cormat %>% mutate_at(vars(starts_with("value")), funs(round(., 2)))

# Create a ggheatmap with basic characteristics, etc. 
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "firebrick3", mid = "white", high = "dodgerblue3", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


# Print the heatmap
print(ggheatmap)

#add correlation coefficients to the heatmap
#geom_text() to add the correlation coefficients on the graph
#guides() to change the position of the legend title
#if else statement in melted data frame to quotes of black and white to adjust text color 
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18, face="bold", color="black"),
        axis.text.y = element_text(size = 18, face="bold", color="black"),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(0.8, 0),
        legend.title = element_text(size = 18, face="bold", color="black"),
        legend.text = element_text(size = 20, face="bold", color="black"),
        legend.position = c(0.48, 0.75),
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 12, barheight = 2, 
                               title.position = "top", title.hjust = 0.5, title.vjust = 1.0))
