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
mig <- read.csv("migration-analysis.csv") %>%
  mutate(Year = ifelse(grepl("-14", mig$IndivYr), 2014, 2015)) %>%
  transform(SprAORank = ave(SprAO, Year, FUN = function(x) rank(-x, ties.method = "average")),
            FallAORank = ave(FallAO, Year, FUN = function(x) rank(-x, ties.method = "average")),
            SprNSDRank = ave(SprNSD, Year, FUN = function(x) rank(x, ties.method = "average")),
            FallNSDRank = ave(FallNSD, Year, FUN = function(x) rank(x, ties.method = "average")))



