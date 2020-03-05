# The main goal of this series of scripts is to merger the numerous raw data 
# files togetherfrom the California database.
# 
# The goal of this script is to clean up the site information data which 
# includes fixed variabels for each site including, site name, lat and long,
# elevation, county, state, property ownership, and pond permance status among 
# others. This includes sites from 47 states. Thus the secondary goal of this 
# script is to filter down to the sites of interest to build a working 
# "SiteCode_List" that we can use to filter down the other data sets. I.E. If 
# are interest in focusing on sites that are from the bay area only, we can 
# do that here. 
. 
## Setting the working directory

library(tidyverse)
library(dplyr)

setwd("~/Desktop/Current_Projects/Data_CleanUp")

############## 1) Site Information Data  ############

# At the end of this section there will be a single file,
# 
#    A) A simple list of the sites that will be used for data analysis
#       This should help eliminate sites that are not in the area of interest
#       for the study . I.E. non Bay Area Sites, each row is a unique Site
#     


site.info.df <- read.csv( "./Data/Raw_Data/Site_Info.csv" )



## First thing is to modify the site info data frame to subset down to the sites
## I am interested in


dim(site.info.df) # 1482 x 19 # seems like way too many sites but ok

str(site.info.df) # also a lot of columns I think i can get away with 
# SiteCode, PropertyName, County, State, Latitude,
# Longitude, Elevation, Longevity so 8 columns


site.info.df <- dplyr::select( site.info.df, c("SiteCode", "PropertyName", "County",
                                               "State","Latitude", "Longitude",
                                               "Elevation", "Longevity"))
# Renaming some of the columns

colnames(site.info.df) <- c("SiteCode", "PropName", "County", "State","Lat",
                            "Long", "Elev", "Longevity")

# Ok lets capitalize everything so there is no weird issue with 

site.info.df$SiteCode <- as.factor(toupper(site.info.df$SiteCode))
site.info.df$PropName <- as.factor(toupper(site.info.df$PropName))
site.info.df$County <- as.factor(toupper(site.info.df$County))
site.info.df$State <- as.factor(toupper(site.info.df$State ))

## Checking to see if there are any errors in the naming structure of the 
## various grouping variables

## State 
unique(site.info.df$State)[order(unique(site.info.df$State))]
# Looks good

## County
unique(site.info.df$County)[order(unique(site.info.df$County))]

# Boulder is misspelled 
site.info.df$County[site.info.df$County=="BOOULDER"] <-"BOULDER" 

# Douglas is misspelled
site.info.df$County[site.info.df$County=="DOUGLASS"] <-"DOUGLAS" 

# Jefferson is in here twice , once as JEFFCO
site.info.df$County[site.info.df$County=="JEFFCO"] <-"JEFFERSON" 

# Monterey county is spelled wrong
site.info.df$County[site.info.df$County=="MONTERRY" ] <-"MONTEREY" 
site.info.df$County[site.info.df$County=="MONTERY" ] <-"MONTEREY" 

# Santa Cruz is in here twice
site.info.df$County[site.info.df$County=="SANTA CRUZ COUNTY"] <-"SANTA CRUZ" 

## Property Name
unique(site.info.df$PropName)[order(unique(site.info.df$PropName))]

# Blue oaks is in here twice , fixing the duplicaiton
site.info.df$PropName[site.info.df$PropName=="BLUE OAK RANCH RESERVE"] <-
  "BLUE OAKS RANCH RESERVE" 

# San Bernardino is in here twice 
site.info.df$PropName[site.info.df$PropName=="SAN BERNARD NATIONAL WILDLIFE REFUGE"] <-
  "SAN BERNARDINO NATIONAL WILDLIFE REFUGE"

# Russell Lakes is in here twice
site.info.df$PropName[site.info.df$PropName=="RUSSEL LAKES SWA"] <-
  "RUSSELL LAKES SWA"

# Ok lets remove all sites that are not from California

site.info.df <- filter( site.info.df, State == "CA" )

dim(site.info.df) # 548 X 8  whoa cut 933 sites 

# lets see what we are missing for site information 
site.info.df[which(is.na(site.info.df), arr.ind=TRUE)[,1],]

## Appears that we are missing a ton of elevation, if we are interested in 
## the effects of elevation we should go back and fill this in
## We are also missing a lot of information regarding "County" and "Property
## Ownership" could be important if this is how we filter the infrmation 

## ok now lerts cut all counties that are not apart of the tri county area
## Contra Costa, Alameda, Santa Clara

site.info.df <- filter( site.info.df, County == "CONTRA COSTA" |
                          County == "ALAMEDA" |
                          County == "SANTA CLARA" )

dim(site.info.df) ## 395 X 8 cute 153 more sites

## lets check out the sites remaining sites

unique(site.info.df$SiteCode) ## 395 unique sites

unique(site.info.df$PropName) ## 40 unique property names

## still a lot to cut, lets focus on the focal properties

site.info.df <- filter( site.info.df, PropName == "5 CANYONS REGIONAL PARK" |
                          PropName == "SILVER OAKS OSTRICH FARM" |
                          PropName == "BLUE OAKS RANCH RESERVE"|
                          PropName == "JOSEPH GRANT COUNTY PARK"|
                          PropName == "BRIONES REGIONAL PARK"|
                          PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR"|
                          PropName == "SAN FELIPE RANCH"|
                          PropName == "PLEASANTON RIDGE REGIONAL PARK"|
                          PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK"|
                          PropName == "VARGAS PLATEAU REGIONAL PRESERVE"
)

dim(site.info.df) ## 169 X 8 we removed 226 sites

# lets see what we are missing for site information 
site.info.df[which(is.na(site.info.df), arr.ind=TRUE)[,1],]

## MIssing a lot of eleveation for a couple core sites so I will add them in 

# BARNBOR
site.info.df$Elev[site.info.df$SiteCode=="BARNBOR"] <- 591

# Junctbor
site.info.df$Elev[site.info.df$SiteCode=="JUNCBOR"] <- 599

# PRNTHJV 
site.info.df$Elev[site.info.df$SiteCode=="PRNTHJV"] <- 319

## Ok sweet I have the list of sites that I will use for the analysis this may
## change as I am sure some of these sites have 1 visit or so but we can deal
## with that later, this is a good place to start

## Saving the site list as an RData 
save(site.info.df, file = "Data/RData/SiteList.RData")
