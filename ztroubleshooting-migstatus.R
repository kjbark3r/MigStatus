#####################
### misc code related to migstatus
### kjb 2016
######################


#########
## NA animal IDs in joined data
## because nsd has more obs than ao/vi

setdiff(nsd$AnimalID, ao$AnimalID)
#[1] 140930 141420 151500
setdiff(nsd$IndivYr, ao$IndivYr)
#[1] "140930-14" "141420-14" "151500-14" "140930-15" "141420-15" "151500-15"

setdiff(ao$AnimalID, vi$AnimalID)
#samesies
setdiff(ao$IndivYr, vi$IndivYr)

#ok all difs are from the ones that are missing all values
#like if they died in 2014 they don't have 2015 indivyr overlaps
#or if the collar malfunctioned befoer we could get any data

#########
# removing NA rows prior to join
# playing with new drop_na fcn in tidyr

test <- drop_na(nsd, c(3,4))
#nope, does 3 or 4 not 3 and 4
test <- drop_na(nsd, 3:4)
#ditto
test <- drop_na(nsd, 3, 4) 
#ditto
#eff it

test <- nsd[rowSums(is.na(nsd)) !=2,]
test2 <- ao[rowSums(is.na(ao)) !=2,]
setdiff(test2$IndivYr, test$IndivYr)
#this diff makes sense; collar malfunctioned in the middle of summer
##which threw off the nsd calculation but not the overlap

rm(test,. test2)
