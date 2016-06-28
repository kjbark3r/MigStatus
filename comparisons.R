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

# preliminary look at migration data (first glance)
look <- read.csv("migstatus-prelimlook.csv") %>%
  subset(select = c("AnimalID", "Status", "TimingSpr14", "TimingFall14", 
                    "TimingSpr15", "TimingFall15", "Notes"))

# HR overlap 
##manual area overlap; automated volume intersection
hro <- read.csv("homerangeoverlap.csv") #automated
hrm <- read.csv("overlap-manual.csv") #manual
hro <- select(hro, -contains("ao"))
hro <- full_join(hro, hrm, by = "AnimalID") 
rm(hrm)

# NSD
nsd <- read.csv("nsd-avg.csv", header = TRUE)

# everything
all <- full_join(look, hro, by = "AnimalID")
all <- full_join(all, nsd, by = "AnimalID")
all <- all[!(is.na(all$Status)) | !(is.na(all$spr14ao)) | !(is.na(all$spr15ao)),]
        #remove early drops that have no migration data

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
#SCALED DATA

sub <- select(all,-contains("Timing"))
scaled <- all[,-c(1:7)]
scaled <- 
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
