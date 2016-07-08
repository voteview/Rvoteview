## This file builds the datasets to be used in the package

## Lax Phillips opinion data
lpOpinion <- read.csv("laxPhilips.csv", stringsAsFactors = F)

save(lpOpinion, file = "../data/lpOpinion.rda")

## State codes
states <- read.csv("statecodes.csv", header = F, stringsAsFactors = F)
names(states) <- c("stateICPSR", "stateMail", "stateName")

save(states, file = "../data/states.rda")
