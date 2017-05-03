###
## required Packages
library(R.utils)
library(plyr)
library(ggplot2)
library(gridExtra)

##  Download the data file and unzip the file
if (!"stormData.csv.bz2" %in% dir("./StormData/")) {
    print("hh")
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                  destfile = "StormData/stormData.csv.bz2")
    bunzip2("StormData/StormData.csv.bz2", overwrite=T, remove=F)
}

## Reading data
if (!"stormData" %in% ls()) {
    storm <- read.csv("StormData/stormData.csv", sep = ",")
}

## Explore the data
dim(storm)
str(storm)

# number of unique event types
length(unique(storm$EVTYPE))

# translate all letters to lowercase
event_types <- tolower(storm$EVTYPE)

# replace all punctuation characters with a space
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
length(unique(event_types))

# update the data frame
storm$EVTYPE <- event_types


##

casualties <- ddply(storm, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused most death and injury
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 10)


#
fatal_events[, c("EVTYPE", "fatalities")]

# Filtering
injury_events[, c("EVTYPE", "injuries")]

#
exp_transform <- function(x) {
    # h -> hundred, k -> thousand, m -> million, b -> billion
    if (x %in% c('h', 'H'))
        return(2)
    else if (x %in% c('k', 'K'))
        return(3)
    else if (x %in% c('m', 'M'))
        return(6)
    else if (x %in% c('b', 'B'))
        return(9)
    else if (!is.na(as.numeric(x))) # if a digit
        return(as.numeric(x))
    else if (x %in% c('', '-', '?', '+'))
        return(0)
    else {
        stop("Invalid exponent value.")
    }
}

## cache=TRUE
prop_dmg_exp <- sapply(storm$PROPDMGEXP, FUN=exp_transform)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(storm$CROPDMGEXP, FUN=exp_transform)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_exp)


## 
# Compute the economic loss by event type

econ_loss <- ddply(storm, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)


##
prop_dmg_events[, c("EVTYPE", "prop_dmg")]


## 
crop_dmg_events[, c("EVTYPE", "crop_dmg")]


## 
# Set the levels in order
p1 <- ggplot(data=fatal_events,
             aes(x=reorder(EVTYPE, fatalities), y=fatalities, fill=fatalities)) +
    geom_bar(stat="identity") +
    coord_flip() +
    ylab("Total number of fatalities") +
    xlab("Event type") +
    theme(legend.position="none")

p2 <- ggplot(data=injury_events,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    ylab("Total number of injuries") +
    xlab("Event type") +
    theme(legend.position="none")

grid.arrange(p1, p2, ncol=2 , top="Top deadly weather events in the US (1950-2011)")


## 
# Set the levels in order
p1 <- ggplot(data=prop_dmg_events,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
    geom_bar(stat="identity") +
    coord_flip() +
    xlab("Event type") +
    ylab("Property damage in dollars (log-scale)") +
    theme(legend.position="none")

p2 <- ggplot(data=crop_dmg_events,
             aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    xlab("Event type") +
    ylab("Crop damage in dollars") + 
    theme(legend.position="none")

grid.arrange(p1, p2, ncol=2 , top="Weather costs to the US economy (1950-2011)")

