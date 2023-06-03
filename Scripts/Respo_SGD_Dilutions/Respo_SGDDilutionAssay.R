###### Respo Code for Light and Dark Runs ####### 
### Created by: Nyssa Silbiger
#### Edited by: Danielle Barnas 
#### Updated by: Hannah Merges
#### Last updated on: 2023-19-05

############## Introduction to code/script ####################
## this script will help us process the raw data gathered during respirometry runs. 
## need to change for specific project/experimental variables 

### Install Packages #####
## if these packages are not yet installed, install them 
## great for updates or new users 
if ("segmented" %in% rownames(installed.packages()) == 'FALSE') install.packages('segmented')
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix')
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra')
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') devtools::install_github('colin-olito/LoLinR')
if ("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate')
if ("chron" %in% rownames(installed.packages()) == 'FALSE') install.packages('chron')
if ("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse')
if ("here" %in% rownames(installed.packages()) == 'FALSE') install.packages('here')
if ("patchwork" %in% rownames(installed.packages()) == 'FALSE') install.packages('patchwork')
if ("PNWColors" %in% rownames(installed.packages()) == 'FALSE') install.packages('PNWColors')

#rm(list=ls())

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

# get the file path

#set the path to all of the raw oxygen datasheets
## these are saved onto the computer in whatever file path/naming scheme you saved things to 
path.p<-here("Data","RespoFiles","RawO2") #the location of all your respirometry files

# bring in all of the individual files
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders

#basename above removes the subdirectory name from the file, re-name as file.names.full
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) 

#empty chamber volume
ch.vol <- 600 #mL #of small chambers 

#Load your respiration data file, with all the times, water volumes(mL), #not doing dry weight just SA
RespoMeta <- read_csv(here("Data","RespoFiles","Respo_TestTrials_MetadataSheet.csv"))
Bio_Data <- read_csv(here("Data","RespoFiles","Fragment_Info_trialrun.csv"))
#View(BioData)
## trying now with prelim fake data, switch to real calculated 
#data after getting volumes and weight and surface area


# join the data together
Sample_Info <- left_join(RespoMeta, BioData)
# set multiple = all warning --> bc sample_id matches multiple rows 

#View(Sample.Info)

##### Make sure times are consistent ####
# make start and stop times real times, so that we can join the respo output and sample_info data frames
Sample_Info <- Sample_Info %>% 
  drop_na(sample_ID) %>% 
  unite(date,start_time,col="start_time",remove=F, sep=" ") %>% 
  unite(date,stop_time,col="stop_time",remove=F, sep=" ") %>% 
  mutate(start_time = mdy_hms(start_time)) %>% 
  mutate(stop_time = mdy_hms(stop_time)) %>% 
  mutate(date = mdy(date))
  

#view(Sample_Info)
## There are some extra files from repeats so I added this line to only select the ones in the actual metadata sheet
# filenames_final<-strsplit(file.names, '.csv') %>% # extract the filename
#   unlist() %>% # make it a vector
#   tibble() %>% # now a tibble so I can filter easily in the pipe
#   filter(. %in% Sample.Info$FileName) %>% # only keep the file names that are on the metadata sheet
#   pull(.) # make it a vector again

filenames_final <- file.names

#generate a 4 column dataframe with specific column names
# data is in umol.L.sec
RespoR <- data.frame(matrix(NA, nrow=length(filenames_final), ncol=4)) # use instead of tidyverse tibble
colnames(RespoR) <- c("FileID_csv","Intercept", "umol.L.sec","Temp.C")

###forloop##### 
for(i in 1:length(filenames_final)) {
  FRow <- as.numeric(which(Sample_Info$FileID_csv==filenames_final[i])) # stringsplit this renames our file
  Respo.Data1 <- read_csv(skip=1,file.path(path.p, paste0(file.names.full[i]))) %>% # reads in each file in list
    dplyr::select(Date, Time, Value, Temp) %>% # keep only what we need: Time stamp per 1sec, Raw O2 value per 1sec, in situ temp per 1sec
    unite(Date,Time,col="Time",remove=T, sep = " ") %>% 
    drop_na() %>%
    mutate(Time = mdy_hms(Time)) %>% # convert time
    drop_na() # drop NAs
  
  Respo.Data1 <- Respo.Data1 %>%
    filter(between(Time, Sample_Info$start_time[FRow], Sample_Info$stop_time[FRow])) # select only data between start and stop time

  
  Respo.Data1 <-  Respo.Data1[-c(1:180),] %>% #we want to start at minute 3 to avoid any noise from the start of the trial
    mutate(sec = 1:n())  # create a new column for every second for the regression
  
  # Get the filename without the .csv
  rename<- sub(".csv","", filenames_final[i])
  
  
  ### plot and export the raw data ####
  p1<- ggplot(Respo.Data1, aes(x = sec, y = Value)) +
    geom_point(color = "dodgerblue") +
    labs(
      x = 'Time (seconds)',
      y = expression(paste(' O'[2],' (',mu,'mol/L)')),
      title = "original"
    )
  
  # thin the data by every 20 seconds to speed it up
  Respo.Data.orig<-Respo.Data1 # save original unthinned data #there is no thin() anymore, created alternative 
  newRespo<-tibble(
    Time=as.numeric(),
    Value=as.numeric(),
    Temp=as.numeric(),
    sec=as.numeric()
  )
  for(j in 1:nrow(Respo.Data.orig)) {#  alternative thinning strategy
    if(j%%20==0){
      newRespo<-rbind(newRespo,Respo.Data1[j,])
    }
  }
  Respo.Data1<-newRespo # assign thinned data to previous df
  
  # Respo.Data1 <- Thin(Respo.Data1 ,By=20)$newData1 #thin data by every 20 points for all the O2 values
  # Respo.Data1$sec <- as.numeric(rownames(Respo.Data1 )) #maintain numeric values for time
  # Respo.Data1$Temp<-NA # add a new column to fill with the thinned data
  # Respo.Data1$Temp <-  Thin(Respo.Data.orig,xy = c(1,3),by=20)#$newData1[,2] #thin data by every 20 points for the temp values
  
  p2 <- ggplot(Respo.Data1, aes(x = sec, y = Value))+
    geom_point(color = "dodgerblue")+
    labs(
      x = 'Time (seconds)',
      y = expression(paste(' O'[2],' (',mu,'mol/L)')),
      title = "thinned"
    )
  
  ##Olito et al. 2017: It is running a bootstrapping technique and calculating the rate based on density
  #option to add multiple outputs method= c("z", "e "pc")
  Regs  <-  rankLocReg(xall=Respo.Data1$sec, yall=Respo.Data1$Value, alpha=0.5, method="pc", verbose=TRUE)  
  
  # Print across two pages so use baseplot to create the pdf
  pdf(paste0(here("Outputs","RespoOutput","ThinningPlots"),"/", rename,"thinning.pdf"))
  
  plot(Regs) # plot the results of Regs
  plot(p2+p1) # use patchwork to bring the raw and thinned data together
  dev.off()
  
  # fill in all the O2 consumption and rate data
  # need clarity on what this is
  RespoR[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  RespoR[i,1] <- paste0(rename,".csv") #stores the file name in the Date column
  RespoR[i,4] <- mean(Respo.Data1$Temp, na.rm=T)  #stores the Temperature from the incubation in the Temp.C column
}  



#export raw data and read back in as a failsafe 
#this allows me to not have to run the for loop again 
write_csv(RespoR, here("Data","RespoFiles","Respo_R.csv"))  

RespoR <- read_csv(here("Data","RespoFiles","Respo_R.csv"))

# Calculate Respiration rate

RespoR2 <- RespoR %>%
  drop_na(FileID_csv) %>% # drop NAs
  left_join(Sample_Info) %>% # Join the raw respo calculations with the metadata
  #mutate(Ch.Volume.ml = ifelse(is.na(volume_ml),ch.vol,ch.vol-volume_ml)) %>% # add 6 L for volume of all blanks and subtract org volume from chamber vol for all else
  mutate(Ch.Volume.mL = 600-volume_mL) %>% # hannah change all  this volume stuff later
  mutate(Ch.Volume.L = Ch.Volume.mL * 0.001) %>% # mL to L conversion
  mutate(umol.sec = umol.L.sec*Ch.Volume.L) %>% #Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
  mutate_if(sapply(., is.character), as.factor) %>% #convert character columns to factors
  mutate(BLANK = as.factor(BLANK)) #make the blank column a factor

# Remove duplicates when assemblages were run multiple times
# anti <- RespoR2 %>% 
#   filter(Date == "2022-07-14") %>% 
#   distinct(SampleID) %>% 
#   left_join(RespoR2) %>% 
#   filter(Date == "2022-07-13")
# 
# RespoR2 <- RespoR2 %>% 
#   anti_join(anti)


#View(RespoR2)


#Account for blank rate by light/Dark and Block (if we do one blank per block)



#View(RespoR)

RespoR_Normalized <- RespoR2 %>%
  #group_by(day_block, SGD_dil, light_dark, colony_number) %>% # also add block here if one blank per block --> Hannah's blanks are weird, so took that out of this grouping 
  #group_by(BLANK)%>% # also add block here if one blank per block
  #summarise(umol.sec = mean(umol.sec, na.rm=TRUE)) %>% # get mean value of blanks per run
  filter(colony_number== "BLANK") %>% # only keep the actual blanks
  #group_by(day_block, SGD_dil, light_dark) %>% 
  mutate(blank.rate = umol.sec) %>%
  dplyr::select(day_block, SGD_dil, light_dark, blank.rate) %>%
  #dplyr::select(blank.rate = umol.sec) %>% # rename the blank rate column
  right_join(RespoR2, multiple = "all") %>% # join with the respo data
  mutate(umol.sec.corr = umol.sec - blank.rate, # subtract the blank rates from the raw rates
         mmol.gram.hr = 0.001*(umol.sec.corr*3600)/SA_cm2, # convert to mmol g-1 hr-1
         mmol.gram.hr_uncorr = 0.001*(umol.sec*3600)/SA_cm2) %>% 
  filter(colony_number!="BLANK") %>% # remove the Blank data
  ungroup() %>% 
  dplyr::select(date, sample_ID, colony_number, SGD_dil, light_dark, run_block, weight_g, Ch.Volume.mL, mmol.gram.hr, chamber_channel, mmol.gram.hr_uncorr, SA_cm2)  #keep only what we need

# CALCULATING R AND GP

# make the respiration values positive (pull out data for dark treatments)
RespoR_Normalized_dark <- RespoR_Normalized %>% 
  filter(light_dark == "DARK") %>% 
  mutate(mmol.gram.hr = mmol.gram.hr*-1,
         mmol.gram.hr_uncorr = mmol.gram.hr_uncorr*-1) %>% 
  mutate(mmol.gram.hr = ifelse(mmol.gram.hr < 0, 0, mmol.gram.hr), # for any values below 0, make 0
         mmol.gram.hr_uncorr = ifelse(mmol.gram.hr_uncorr < 0, 0, mmol.gram.hr_uncorr)) %>% 
  mutate(P_R = "R") # all dark run rates get R for respiration

# all light run rates get NP for net photosynthesis
RespoR_Normalized_light <- RespoR_Normalized %>% 
  filter(light_dark == "LIGHT") %>% 
  mutate(mmol.gram.hr = ifelse(mmol.gram.hr < 0, 0, mmol.gram.hr), # for any values below 0, make 0
         mmol.gram.hr_uncorr = ifelse(mmol.gram.hr_uncorr < 0, 0, mmol.gram.hr_uncorr)) %>% 
  mutate(P_R = "NP")

# rejoin data into single df
RespoR_Normalized2 <- full_join(RespoR_Normalized_light, RespoR_Normalized_dark) %>% 
  drop_na(mmol.gram.hr) # removes anticipated sampleID's that were not actually run


#make column for GP and group by fragment ID and temp to keep R and NP together
RespoR_NormalizedGP <- RespoR_Normalized2 %>% 
  group_by(colony_number, SGD_dil) %>% 
  summarize(mmol.gram.hr = sum(mmol.gram.hr),
            mmol.gram.hr_uncorr = sum(mmol.gram.hr_uncorr), # NP + R = GP
            #Temp.C = mean(Temp.C)
            ) %>% # get mean temperature of light and dark runs
  mutate(P_R="GP") %>% # Label for Gross Photosynthesis
  mutate(light_dark = "LIGHT") %>% 
  mutate(mmol.gram.hr = ifelse(mmol.gram.hr < 0, 0, mmol.gram.hr), # for any values below 0, make 0
         mmol.gram.hr_uncorr = ifelse(mmol.gram.hr_uncorr < 0, 0, mmol.gram.hr_uncorr))

# rejoin for full df with NP, R, and GP rates
RespoR_Normalized_Full <- RespoR_Normalized2 %>% 
  dplyr::select(colony_number, SGD_dil, light_dark, P_R, mmol.gram.hr, mmol.gram.hr_uncorr) %>% 
  full_join(RespoR_NormalizedGP)


write_csv(RespoR_Normalized_Full , here("Data","RespoFiles","Respo_RNormalized_AllRates.csv"))  


### PLOT
RespoR_Normalized_Full <- read_csv(here("Data","RespoFiles","Respo_RNormalized_AllRates.csv"))

my_pal <- pnw_palette(name="Starfish",n=2,type="discrete")

# plot GP, NP and R
# R first 
RatesPlot_R <- RespoR_Normalized_Full %>%
  filter(P_R == "R") %>%
  mutate(colony_number = as.factor(colony_number)) %>%
  ggplot(aes(x=SGD_dil, 
             y=mmol.gram.hr,
             color = colony_number)) +
  geom_line() +
  scale_x_log10(#limits=c(0,5),
    breaks=c(0, 0.01, 0.03, 0.05, 0.07, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0)) +
 # scale_x_continuous(limits=c(0,5),
                    # breaks=c(0, 0.01, 0.03, 0.05, 0.07, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0)) + 
  facet_wrap(~ colony_number)#, scales = "fixed") +
#geom_smooth(method="lm") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))+
  #labs(x = "Environmental Treatment (High or Low)",
      # color = "Assemblage \n Treatment",
      # y = "Rate (mmol O2 gram-1 hr-1)",
      # title = "Rate of O2 production or consumption") +
  scale_color_manual(values=my_pal) 

RatesPlot_R

## GP (NP-R) 
RatesPlot_GP <- RespoR_Normalized_Full %>%
  filter(P_R == "GP") %>%
  mutate(colony_number = as.factor(colony_number)) %>%
  ggplot(aes(x=SGD_dil, 
             y=mmol.gram.hr,
             color = colony_number)) +
  geom_line() +
  #scale_x_continuous() + 
  scale_x_log10(#limits=c(0,5),
                breaks=c(0, 0.01, 0.03, 0.05, 0.07, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0)) +
  facet_wrap(~ colony_number)#, scales = "fixed")
  theme_bw()+
  theme(strip.background = element_rect(fill = "white"))+
  geom_smooth() +
  #labs(x = "Environmental Treatment (High or Low)",
  # color = "Assemblage \n Treatment",
  # y = "Rate (mmol O2 gram-1 hr-1)",
  #title = "Rate of O2 production or consumption") +
  scale_color_manual(values=my_pal)  +
  

RatesPlot_GP
  
ggsave(here("Outputs", "RespoOutput","AllRates.pdf"), RatesPlot, device = "pdf", width = 10, height = 10)

RatesPlot_GP + RatesPlot_R

# quick modeling
# check assumptions for all (referencing Biometry notes below)
# if not seeing growth trends look at my photos and see pigment changes / mortality

library(agricolae) # HSD.test()

# models
GPData <- RespoR_Normalized_Full %>% filter(P_R == "GP")
model1 <- lm(data = GPData, mmol.gram.hr ~ SGD_dil)
anova(model1)
HSD.test(model1, "SGD_dil", console=TRUE)

NPData <- RespoR_Normalized_Full %>% filter(P_R == "NP")
model2 <- lm(data = NPData, mmol.gram.hr ~ SGD_dil)
anova(model2)
HSD.test(model2, "SGD_dil", console=TRUE)

RData <- RespoR_Normalized_Full %>% filter(P_R == "R")
model3 <- lm(data = RData, mmol.gram.hr ~ SGD_dil)
anova(model3)
HSD.test(model3, "SGD-dil", console=TRUE)


#Now let's check our assumptions
plot(model1)
plot(model2)
plot(model3)

# first plot: are variances equal? look at the spread of points of both groups on the different sides of the plot
## anything more than 3x greater are probably unequal and may need to be transformed
## spread is about the same here
# second plot: normality
# third plot: same thing as the first plot, but the square root of the standardized residuals. tests the same thing. are variances equal
# fourth plot: leverage plot.  casey doesn't really use this for anova

# normality was fishy so do a qqp
library(car)
qqp(model1) # just a little off, but it's good enough
qqp(model2) # just a little off, but it's good enough
qqp(model3) # just a little off, but it's good enough

#I'm not 100% sure about the normal probability plot. Let's try it with confidence intervals
library(car)
resid1<-residuals(model1)
qqp(resid1, "norm") # numbers indicate outliers

resid2<-residuals(model2)
qqp(resid2, "norm")

resid3<-residuals(model3)
qqp(resid3, "norm")


#If you wanted to do a post-hoc test
# library(emmeans)
# emmeans(model1, pairwise~Temp*Genotype, adjust="tukey")

#I wanted to use agricolae, but had to futz with this a little to make this happen
#Tukey tests
# tx<-with(mydata, interaction(Temp, Genotype))
# rmod<-lm(r~tx, data=mydata)
# library(agricolae)
# HSD.test(rmod, "tx", console=TRUE)