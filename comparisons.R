#######################################################
## Comparing Methods of Assessing Migratory Behavior ##
########  NSERP - Kristin Barker - June 2016  #########
#######################################################

##WD
####Work computer or personal laptop
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\MigStatus"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\MigStatus"

if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
      cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)

##PACKAGES
library(dplyr)

##DATA

# first glance at migration data (for old bulls)
look <- read.csv("migstatus-prelimlook.csv") %>%
  subset(select = c("AnimalID", "Status", "TimingSpr14", "TimingFall14", 
                    "TimingSpr15", "TimingFall15", "Notes"))

# area overlap - manual calc - spr14 only so far
ao <- read.csv("spr14overlap-manual.csv") %>%
  subset(select = -X)

##
comp <- full_join(look, ao, by = "AnimalID")
##

# volume overlap - to pull from kerneloverlap calc
vo <- read.csv("homerangeoverlap.csv") %>%
  select(-contains("ao"))

##
comp <- full_join(comp, vo, by = "AnimalID")
##

# nsd
nsd14 <- read.csv("AICtable2014.csv") %>%
  mutate(NSD14 = ifelse(bestmodel == 1, "Migrant",
    ifelse(bestmodel == 2, "MixedMigrant", 
      ifelse(bestmodel == 3, "Disperser",
        ifelse(bestmodel == 4, "Resident", "Nomad")))))


nsd15 <- read.csv("AICtable2015.csv") %>%
  mutate(NSD15 = ifelse(bestmodel == 1, "Migrant",
    ifelse(bestmodel == 2, "MixedMigrant", 
      ifelse(bestmodel == 3, "Disperser",
        ifelse(bestmodel == 4, "Resident", "Nomad")))))

