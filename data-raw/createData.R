## This file builds the datasets to be used in the package

## Lax Phillips opinion data
lpOpinion <- read.csv("laxPhilips.csv", stringsAsFactors = F)

save(lpOpinion, file = "../data/lpOpinion.rda")

## State codes
states <- read.csv("statecodes.csv", header = F, stringsAsFactors = F)
names(states) <- c("state_code", "state_abbrev", "state_name")

state_table <- read.csv("state_table.csv", stringsAsFactors = F)
state_table$state_name <- toupper(state_table$name)
states <- merge(states, state_table[, c("state_name", "census_region_name", "census_division_name")],
                all.x = T, all.y = F)
states
save(states, file = "../data/states.rda")
