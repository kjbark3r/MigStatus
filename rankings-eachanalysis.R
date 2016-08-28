#######################################################
## Comparing Methods of Assessing Migratory Behavior ##
########  NSERP - Kristin Barker - June 2016  #########
#######################################################

## WD
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

## PACKAGES
library(dplyr)

## DATA
look <- read.csv("migstatus-prelimlook.csv") %>%
  subset(select = c("AnimalID", "Status", "Notes")) %>%
  rename(Look = Status)
mig <- read.csv("migration-analysis.csv")
mig <- mig %>%
  mutate(Year = ifelse(grepl("-14", mig$IndivYr), 2014, 2015)) %>%
  transform(SprAORank = ave(SprAO, Year, FUN = function(x) rank(-x, ties.method = "average")),
            FallAORank = ave(FallAO, Year, FUN = function(x) rank(-x, ties.method = "average")),
            SprVIRank = ave(SprVI, Year, FUN = function(x) rank(-x, ties.method = "average")),
            FallVIRank = ave(FallVI, Year, FUN = function(x) rank(-x, ties.method = "average")),
            SprNSDRank = ave(SprNSD, Year, FUN = function(x) rank(x, ties.method = "average")),
            FallNSDRank = ave(FallNSD, Year, FUN = function(x) rank(x, ties.method = "average"))) %>%
  left_join(look, by = "AnimalID")

## VISUALIZATIONS

# just by rank - this has zero mathematical validity
par(mfrow=c(3,2))
plot(mig$SprAO ~ mig$SprAORank, col = mig$Look)
plot(mig$FallAO ~ mig$FallAORank, col = mig$Look)
plot(mig$SprVI ~ mig$SprVIRank, col = mig$Look)
plot(mig$FallVI ~ mig$FallVIRank, col = mig$Look)
plot(mig$SprNSD ~ mig$SprNSDRank, col = mig$Look)
plot(mig$FallNSD ~ mig$FallNSDRank, col = mig$Look)

# plotting each against the other
# only looking at spring for now bc this is the mign my thesis focuses on
par(mfrow=c(3,1))
plot(SprAORank ~ SprVIRank, data=mig, col=mig$Look)
plot(SprAORank ~ SprNSDRank, data=mig, col=mig$Look)
plot(SprVIRank ~ SprNSDRank, data=mig, col=mig$Look)

# just added nsd total;; checking out what that does
nsd <- read.csv("nsd-avg-total.csv", header = TRUE) 
nsd <- nsd[rowSums(is.na(nsd)) !=2,] #remove nas
plot(SprNSD ~ SprTotalNSD, data=mig, col=Look)
plot(FallNSD ~ FallTotalNSD, data=mig, col=Look)
