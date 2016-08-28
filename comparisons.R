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
library(tidyr)

##DATA

# preliminary look at migration data (first glance)
look <- read.csv("migstatus-prelimlook.csv") %>%
  subset(select = c("AnimalID", "Status", "TimingSpr14", "TimingFall14", 
                    "TimingSpr15", "TimingFall15", "Notes"))

# HR overlap 
ao <- read.csv("areaoverlap.csv") 
ao <- ao[rowSums(is.na(ao)) !=2,] #remove nas
vi <- read.csv("volumeintersection.csv")
vi <- vi[rowSums(is.na(vi)) !=2,] #remove nas
hro <- select(vi, -AnimalID) %>%
  full_join(ao, by = "IndivYr") 

# NSD
nsd <- read.csv("nsd-avg-total.csv", header = TRUE) 
nsd <- nsd[rowSums(is.na(nsd)) !=2,] #remove nas

# All together
mig <- select(nsd, -AnimalID) %>%
  rename(SprNSD = SprAvgNSD, FallNSD = FallAvgNSD) %>%
  full_join(hro, by = "IndivYr") %>%
  select(IndivYr, AnimalID, SprAO, FallAO, SprVI, FallVI, SprNSD, FallNSD)
write.csv(mig, file="migration-analysis.csv", row.names=F)


###############################
## PLOTS
###############################

#############
#2014 VS 2015 FOR EACH ANALYSIS
#to see how much variation occurs between years

## HR overlap
par(mfrow=c(2,2))
plot(hro$spr14ao~hro$spr15ao, main = "Spring Area")
plot(hro$fall14ao~hro$fall15ao, main = "Fall Area")
plot(hro$spr14vi~hro$spr15vi, main = "Spring Volume")
plot(hro$fall14vi~hro$fall15vi, main = "Fall Volume")

## NSD
par(mfrow=c(1,1))
plot(nsd$avgNSD2014~nsd$avgNSD2015, main = "Summer NSD")

#############
#RANK CONTINUUM 
#with prelim look as color

# NSD
par(mfrow=c(2,1))

nsd.p1 <- all %>%
  arrange(avgNSD2014) %>%
  subset(select = c(AnimalID, avgNSD2014, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(nsd.p1$avgNSD2014 ~ nsd.p1$Rank, main = "NSD 2014",
     col = nsd.p1$Status)

nsd.p2 <- all %>%
  arrange(avgNSD2015) %>%
  subset(select = c(AnimalID, avgNSD2015, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(nsd.p2$avgNSD2015 ~ nsd.p2$Rank, main = "NSD 2015",
     col = nsd.p2$Status)

# volume intersection
par(mfrow=c(2,2))

s14vi <- all %>%
  arrange(spr14vi) %>%
  subset(select = c(AnimalID, spr14vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s14vi$spr14vi ~ s14vi$Rank, main = "Spring 2014 VI",
     col = s14vi$Status, ylim = c(0,1))

f14vi <- all %>%
  arrange(fall14vi) %>%
  subset(select = c(AnimalID, fall14vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f14vi$fall14vi ~ f14vi$Rank, main = "Fall 2014 VI",
     col = f14vi$Status, ylim = c(0,1))

s15vi <- all %>%
  arrange(spr15vi) %>%
  subset(select = c(AnimalID, spr15vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s15vi$spr15vi ~ s15vi$Rank, main = "Spring 2015 VI",
     col = s15vi$Status, ylim = c(0,1))

f15vi <- all %>%
  arrange(fall15vi) %>%
  subset(select = c(AnimalID, fall15vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f15vi$fall15vi ~ f15vi$Rank, main = "Fall 2015 VI",
     col = f15vi$Status, ylim = c(0,1))

# area overlap
par(mfrow=c(2,2))

s14a <- all %>%
  arrange(spr14ao) %>%
  subset(select = c(AnimalID, spr14ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s14a$spr14ao ~ s14a$Rank, main = "Spring 2014 AO",
     col = s14a$Status)

f14a <- all %>%
  arrange(fall14ao) %>%
  subset(select = c(AnimalID, fall14ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f14a$fall14ao ~ f14a$Rank, main = "Fall 2014 AO",
     col = f14a$Status)

s15a <- all %>%
  arrange(spr15ao) %>%
  subset(select = c(AnimalID, spr15ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s15a$spr15ao ~ s15a$Rank, main = "Spring 2015 AO",
     col = s15a$Status)

f15a <- all %>%
  arrange(fall15ao) %>%
  subset(select = c(AnimalID, fall15ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f15a$fall15ao ~ f15a$Rank, main = "Fall 2015 AO",
     col = f15a$Status)

#############
#DISTRIBUTIONS

#vol & nsd
par(mfrow=c(2,3))
hist(all$spr14vi, main = "Spr 14 VI")
hist(all$fall14vi, main = "Fall 14 VI")
hist(all$avgNSD2014, main = "Sum 14 NSD")
hist(all$spr15vi, main = "Spr 15 VI")
hist(all$fall15vi, main = "Fall 15 VI")
hist(all$avgNSD2015, main = "Sum 15 NSD")
# all r-skew

  # log-transformed
  par(mfrow=c(2,3))
  hist(log(all$spr14vi), main = "Spr 14 VI")
  hist(log(all$fall14vi), main = "Fall 14 VI")
  hist(log(all$avgNSD2014), main = "Sum 14 NSD")
  hist(log(all$spr15vi), main = "Spr 15 VI")
  hist(log(all$fall15vi), main = "Fall 15 VI")
  hist(log(all$avgNSD2015), main = "Sum 15 NSD")
  # looks good for nsd but not so much for vi

  # sqrt-transformed
  par(mfrow=c(2,3))
  hist(sqrt(all$spr14vi), main = "Spr 14 VI")
  hist(sqrt(all$fall14vi), main = "Fall 14 VI")
  hist(sqrt(all$avgNSD2014), main = "Sum 14 NSD")
  hist(sqrt(all$spr15vi), main = "Spr 15 VI")
  hist(sqrt(all$fall15vi), main = "Fall 15 VI")
  hist(sqrt(all$avgNSD2015), main = "Sum 15 NSD")
  #better?? not super great though
  
  #boxplots of transmformed
  par(mfrow=c(2,3))
  boxplot(sqrt(all$spr14vi), main = "Spr 14 VI")
  boxplot(sqrt(all$fall14vi), main = "Fall 14 VI")
  boxplot(sqrt(all$avgNSD2014), main = "Sum 14 NSD")
  boxplot(sqrt(all$spr15vi), main = "Spr 15 VI")
  boxplot(sqrt(all$fall15vi), main = "Fall 15 VI")
  boxplot(sqrt(all$avgNSD2015), main = "Sum 15 NSD")
  
  
#area & nsd
par(mfrow=c(2,3))
hist(all$spr14ao, main = "Spr 14 AO")
hist(all$fall14ao, main = "Fall 14 AO")
hist(all$avgNSD2014, main = "Sum 14 NSD")
hist(all$spr15ao, main = "Spr 15 AO")
hist(all$fall15ao, main = "Fall 15 AO")
hist(all$avgNSD2015, main = "Sum 15 NSD")
# all r-skew

  # log-transformed
  par(mfrow=c(2,3))
  hist(log(all$spr14ao), main = "Spr 14 AO")
  hist(log(all$fall14ao), main = "Fall 14 AO")
  hist(log(all$avgNSD2014), main = "Sum 14 NSD")
  hist(log(all$spr15ao), main = "Spr 15 AO")
  hist(log(all$fall15ao), main = "Fall 15 AO")
  hist(log(all$avgNSD2015), main = "Sum 15 NSD")
  # looks good for nsd but not so much for ao
  
  # sqrt-transformed
  par(mfrow=c(2,3))
  hist(sqrt(all$spr14ao), main = "Spr 14 AO")
  hist(sqrt(all$fall14ao), main = "Fall 14 AO")
  hist(sqrt(all$avgNSD2014), main = "Sum 14 NSD")
  hist(sqrt(all$spr15ao), main = "Spr 15 AO")
  hist(sqrt(all$fall15ao), main = "Fall 15 AO")
  hist(sqrt(all$avgNSD2015), main = "Sum 15 NSD")
  #better i think



#############
#SCALED DATA
sub <- all[,-c(1:7)]
scaled <- data.frame(scale(sub))

par(mfrow=c(2,3))
boxplot(scaled$spr14vi, main = "Spr 14 VI")
boxplot(scaled$fall14vi, main = "Fall 14 VI")
boxplot(scaled$avgNSD2014, main = "Sum 14 NSD")
boxplot(scaled$spr15vi, main = "Spr 15 VI")
boxplot(scaled$fall15vi, main = "Fall 15 VI")
boxplot(scaled$avgNSD2015, main = "Sum 15 NSD")

#############
#AGAINST EACH OTHER

#VI - NSD
par(mfrow=c(2,1))
plot(all$spr14vi~all$avgNSD2014, main = "2014",
     xlab = "NSD", ylab = "Vol Intersection",
     col = all$Status)
plot(all$spr15vi~all$avgNSD2015, main = "2015",
     xlab = "NSD", ylab = "Vol Intersection",
     col = all$Status)

# VI - INVERSE NSD
all$invNSD14 <- 1/all$avgNSD2014
all$invNSD15 <- 1/all$avgNSD2015

par(mfrow=c(2,1))
plot(all$spr14vi~all$invNSD14, main = "2014",
     xlab = "NSD", ylab = "Vol Intersection",
     col = all$Status)
plot(all$spr15vi~all$invNSD15, main = "2015",
     xlab = "NSD", ylab = "Vol Intersection",
     col = all$Status)

  #identify
  par(mfrow=c(1,1))
  #2014
  plot(all$spr14vi~all$invNSD14, main = "2014",
       xlab = "NSD", ylab = "Vol Intersection",
       col = all$Status)
  weird14 <- identify(all$invNSD14, all$spr14vi, 
           n = 5, labels = all$AnimalID)
  #2015
  plot(all$spr15vi~all$invNSD15, main = "2015",
       xlab = "NSD", ylab = "Vol Intersection",
       col = all$Status)
  identify(all$invNSD15, all$spr15vi, 
           n = 5, labels = all$AnimalID)

#AO - NSD
par(mfrow=c(2,1))
plot(all$spr14ao~all$avgNSD2014, main = "2014",
     xlab = "Area Overlap", ylab = "NSD",
     col = all$Status)
plot(all$spr15ao~all$avgNSD2015, main = "2015",
     xlab = "Area Overlap", ylab = "NSD",
     col = all$Status)

###############################
## TROUBLESHOOTING ETC
###############################

# why different #s of rows in each assessment? (yikes...)
ll <- data.frame(c(unique(look$AnimalID), NA, NA, NA, NA))
  arrange(ll)
oo <- data.frame(c(unique(hro$AnimalID), NA, NA, NA))
  colnames(oo) <- "overlap"
  overlap <- oo[order(oo$overlap),]
  oo <- data.frame(overlap)
nn <- data.frame(unique(nsd$AnimalID))
  arrange(nn)

hm <- bind_cols(ll, oo, nn)
colnames(hm) <- c("look", "overlap", "nsd")
#never mind, it's fine. those are all early drops.

# i can never get identify() to work
identify(all$invNSD14, all$spr14vi, labels = all$AnimalID)
identify(all$invNSD14, all$spr14vi, labels = all$AnimalID, n = 3)
  #one plot at a time?
par(mfrow=c(1,1))
plot(all$spr14vi~all$invNSD14, main = "2014",
     xlab = "NSD", ylab = "Vol Intersection",
     col = all$Status)
identify(all$invNSD14, all$spr14vi, n = 3)
#yep

#investigating weirdos/unexpected classifications
all$Notes[1]
all$Notes[6]
all$Notes[37]
all$Notes[44]
all$Notes[45]
all$Notes[56]
all$Notes[61]
all$Notes[62]
all$Notes[65]
all$Notes[66]