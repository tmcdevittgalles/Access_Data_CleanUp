# The main goal of this series of scripts is to merger the numerous raw data 
# files togetherfrom the California database.
# 
# The goal of this script is to clean up the site wetland and water chemistry 
# data. There has been a lot of issues with this data, including variaion in
# what unit is used for various water chemistry metric as well as errors in
# site perimeter and area data


## Setting the working directory

library(tidyverse)
library(dplyr)

setwd("~/Desktop/Current_Projects/Data_CleanUp")


###################    Wetland information data frame   #######################

# Entering data
Site.Wet.df <- read.csv( "./Data/Raw_Data/Wetland_Info.csv" ) 

# Data structure
str( Site.Wet.df )

dim( Site.Wet.df ) # 3287 X 20

## So based on the wetland data set I need to seperate the assmt code to get 
## a site code and date into the data but first lets clean up the dataset


Site.Wet.df <- dplyr::select( Site.Wet.df, c("AssmtCode", "PondArea..m.", 
                                             "Perimeter..m.", "MaxDepth..m.",
                                             "ShoreVeg_Pct", "Juncus_Pct", "Typha_Pct",
                                             "Bullrush_Pct","Other_Pct", 
                                             "OpenWater_Pct", "CanopyCov_Pct",
                                             "Tree.Measure", "Dry") )

# simplifying column names

colnames( Site.Wet.df ) <- c("AssmtCode", "Area", 
                             "Perim", "Depth",
                             "ShoreVeg", "Juncus", "Typha",
                             "Bullrush","Other", 
                             "OpenWate", "Canopy",
                             "Trees", "Dry")

## Capitalizing all the AssmtCode

Site.Wet.df$AssmtCode <- as.factor( toupper( Site.Wet.df$AssmtCode ) )

## creating a dummy assmtcode to seperate the SiteCode and Date

Site.Wet.df$dumAssmtCode <- Site.Wet.df$AssmtCode 

# Seperating assmentcode into date and SiteCode
Site.Wet.df <- Site.Wet.df %>%
  separate( dumAssmtCode, c("SiteCode", "Date"), sep =-9) %>% 
  separate( Date, c("Throw1", "Date"), sep =-8) %>%
  dplyr::select( -"Throw1")

# Make sitecode a factor
Site.Wet.df$SiteCode <- as.factor(Site.Wet.df$SiteCode)

## Seperating by month year and date
Site.Wet.df$dumDate <- as.numeric(Site.Wet.df$Date)

Site.Wet.df <- Site.Wet.df %>%
  separate( dumDate, c("Year", "DumDate"), sep = -4) %>% 
  separate( DumDate, c("Month", "Day"), sep = 2) 

## ok lets filter down to the Predefined site list

dim(Site.Wet.df) # 3287 x 15

## Loading in premade sitelist as r data
load(file="./Data/RData/SiteList.RData")

Site.Wet.df <- Site.Wet.df %>%
  filter( SiteCode %in% site.info.df$SiteCode )

dim(Site.Wet.df) # 1872 X 15 wow, we cut out 1415 site assments

unique( Site.Wet.df$SiteCode ) #165 levels how does that compare to the Sitelist

unique( site.info.df$SiteCode ) # 169 levels, 4 SiteCodes don't have SiteAssmt


##### data clean up site wetland info ###
#Area
hist( log10( Site.Wet.df$Area + 1 ))
max( Site.Wet.df$Area, na.rm = T )## very large
Site.Wet.df[which.max( Site.Wet.df$Area),] # Ca-Glake is the large site just
# going to put NA
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "CA-GLAKE_20150804"] <-NA


# Perim
hist( log10( Site.Wet.df$Perim + 1 ))
max( Site.Wet.df$Perim, na.rm = T )## very large
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-GLAKE_20150804"] <-NA



plot( x= log10(Site.Wet.df$Area+1), y= log10(Site.Wet.df$Perim + 1))

## Several points are not lining up so lets idenify and adjust
#identify(x= log10(Site.Wet.df$Area+1), y= log10(Site.Wet.df$Perim + 1))

## Clearly Ca-Mccry should be 10414 not 10.414

Site.Wet.df$Area[Site.Wet.df$AssmtCode == "CA-MCCRY_20170525" ] <- 10414 
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-MCCRY_20170525" ] <- 550 

# Glake is just too weird i am goin to pu NA for all measurements
Site.Wet.df$Area[Site.Wet.df$SiteCode == "CA-GLAKE" ] <- NA
Site.Wet.df$Perim[Site.Wet.df$SiteCode == "CA-GLAKE" ] <- NA

## PRKING does not have an area of 2.875, should probably be 2875
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "PRKING_20150713" ] <- 2875

## Ca-BN016 perimeter is off at 1390, should be 139
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-BN016_20150519" ] <-  139

## BNPND011 area is off at 9087.98, should be 987.98
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "BNPND011_20160802" ]  <- 987.98

## Ca-Sf31 Perimeter is wrong changing it to 
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-SF31_20130722" ] <- 22.7

# Changing Mud65 area to 100 instead of 10
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "CA-MUD65_20160712" ] <- 100

# Changing area to 210.67 instead of just 21 
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "PRNTH2_20160726" ] <- 210.67 

# Changing area to 1260 instead of just 126
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "NBOR_20180518" ] <- 1260 

# Changing area from 140 to 1400
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "BNPND002_20190710" ] <- 1400

# Changing area from 69.8 to 698
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "MUD46_20140730" ] <- 698

# Changing area from 45.6 to 457
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "POGCP_20100716" ] <- 457

# Changing Perimter 
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-BN016_20100712" ] <- 157

# Changing Perim from 405 to 145
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRNTHMIT_20160804" ] <- 145

## changing area from 101.1 to 1011
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "RLSNKGCP_20180523" ] <- 1011

# Changing area from 37.3 to 373
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "VPPND004_20140724" ] <- 373

# Changing area from 38.8 to 138.8
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "WDMLGCP_20160606" ] <- 138.8

# Changing perimeter from 209 to 109
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "GDPND004_20160630" ]  <- 109

# Changing both Perim and area
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "HIDDEN_20130701" ]  <- 154

Site.Wet.df$Area[Site.Wet.df$AssmtCode == "HIDDEN_20130701" ] <- 838.3

## Changing area from 166 to 1660
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "PRPND003_20110519" ] <- 1660

# Changing Hidden area
Site.Wet.df$Area[Site.Wet.df$AssmtCode == "HIDDEN_20190715" ] <- 1420

#Changing perimeter from 211 to 111
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRPND004_20180710" ] <- 111

# Changing perimter from 232 to 132

Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRNTH1_20100727" ] <- 132

## Changing perimeter of EDWD to 230 instead of 130

Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "CA-EDWD_20100524" ] <- 230

## Changing perimeter of PRNTH2 form 55 to 155
Site.Wet.df$Perim[Site.Wet.df$AssmtCode == "PRNTH2_20190611" ] <- 155 


plot( x= log10(Site.Wet.df$Area+1), y= log10(Site.Wet.df$Perim + 1))


## ok i think fixing the perimeter was the most important step, feeling good
## about that now lets improve the rest, i think most things remaining is simply
## adding zeros or just deleting columns,


# lets add 0s to all the NAs in the vegetation data
Site.Wet.df$Juncus[is.na(Site.Wet.df$Juncus)] <- 0
Site.Wet.df$Typha[is.na(Site.Wet.df$Typha)] <- 0
Site.Wet.df$Other[is.na(Site.Wet.df$Other)] <- 0

## Combinding the two data frames
Site.df <- left_join(Site.Wet.df, site.info.df, by = "SiteCode")

## Ok lets check to make sure there are no outliers in perimeter or area
## for sites that only have one of these measurements
## 

at1  <- Site.df
at1$Area[is.na(at1$Area)] <- 0
at1$Perim[is.na(at1$Perim)] <- 0
ggplot(at1, aes(x = log10(Area+1) ,y = log10(Perim+1), color=SiteCode) )+ 
  geom_jitter(size=3,width=.5, height=.5)+
  theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "5 CANYONS REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "5 CANYONS REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## 5 canyon looks good

## Silver oaks

filter( Site.df, PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## Early Barn perim and area seem high but probably a measurement issue not a 
## data issue

## Blue Oaks

filter( Site.df, PropName == "BLUE OAKS RANCH RESERVE")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


filter( Site.df, PropName == "BLUE OAKS RANCH RESERVE")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")
## Blue Oaks looks good

## Grant

filter( Site.df, PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x =Date, y = Depth, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x =Date, y =ShoreVeg, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")



## Grant is looking good, lots of variation , kind of loving it 

## Briones RP

filter( Site.df, PropName == "BRIONES REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "BRIONES REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y =Area, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


filter( Site.df, PropName == "BRIONES REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y =Depth, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "BRIONES REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y =ShoreVeg, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")




# Brinoes 003 early perimenter is way off change from 1212 to 121

Site.df$Perim[Site.df$AssmtCode == "CA-BN003_20100713" ]  <- 121


## EBMUD

filter( Site.df, PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR")  %>%
  ggplot(aes(x =Date, y = Depth, color=SiteCode ,group=SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


filter( Site.df, PropName == "EAST BAY MUNICIPAL UTILITY DISTRICT SAN PABLO RESERVOIR")  %>%
  ggplot(aes(x =Date, y =ShoreVeg, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")




## MUD is looking good

## San Felipe 

filter( Site.df, PropName == "SAN FELIPE RANCH")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "SAN FELIPE RANCH")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "SAN FELIPE RANCH")  %>%
  ggplot(aes(x =Date, y =Depth, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "SAN FELIPE RANCH")  %>%
  ggplot(aes(x =Date, y =ShoreVeg, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


## looking good San Felipe

## Pleasanton RP

filter( Site.df, PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x =Perim, y = Depth, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y =ShoreVeg, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


## PRPDN005 early is wrong, lets fix that should be 83 instead of 283

Site.df$Perim[Site.df$AssmtCode == "PRPND005_20100729" ]  <- 83
Site.df$Area[Site.df$AssmtCode == "PRPND005_20100729" ]  <- 473

## quick also looks off so lets change that

Site.df$Perim[Site.df$AssmtCode == "PRPND014_20170607" ]  <- 280
Site.df$Area[Site.df$AssmtCode == "PRPND014_20170607" ]  <- 4800.61

## Garin Dry Creek RP

filter( Site.df, PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y = Depth, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "GARIN/DRY CREEK PIONEER REGIONAL PARK")  %>%
  ggplot(aes(x =Date, y =ShoreVeg, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")




## Garin looks good except garin 9 is very large , man do i hate GDPND010


Site.df$Perim[Site.df$AssmtCode == "GDPND009_20100603" ]  <- 152

## Vargus RP

filter( Site.df, PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x =Date, y = Perim, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x =Date, y = Area, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x =Date, y = Depth, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x =Date, y =ShoreVeg, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df, PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x =Date, y =Canopy, color=SiteCode ))+ geom_point(size=3)+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")




############################# Water Chemistry Data #############################

Site.Water.df <- read.csv( "./Data/Raw_Data/Water_Quality.csv" )


str( Site.Water.df )

dim( Site.Water.df ) # 3174 X 24


site.19 <- filter(Site.Water.df, Year == 2017)
length(unique(site.19$SiteCode))

## So based on the wetland data set I need to seperate the assmt code to get 
## a sitte code and date into the data but first lets clean up the dataset


Site.Water.df <- dplyr::select( Site.Water.df, c("AssmtCode", "Conductivity..uS.cm.", 
                                                 "TDS..g.l.", "Salinity","WaterTemp..C.",
                                                 "pH", "Turbidity", "SecchiDepth..cm.",
                                                 "NutrientSamples","TotalN..uMOLES.L.", 
                                                 "TotalP..uMOLES.L.", "DOC.mg.C.L",
                                                 "NH4...uEQ.L.", "DON..uM.L.",
                                                 "PO4.3...uEQ.L.","IP..uM.L.",
                                                 "DOP..uM.L.", "NO3..uEQ.L.",
                                                 "IN..uM.L.") )

# simplifying column names

colnames( Site.Water.df ) <- c("AssmtCode", "Conduct", 
                               "TDS", "Salinity", "Temp",
                               "pH", "Turbid", "Secchi",
                               "NutrientSamples","TotalN", 
                               "TotalP", "DOC",
                               "NH4", "DON",
                               "PO4","IP",
                               "DOP", "NO3",
                               "IN")

## Capitalizing all the AssmtCode

Site.Water.df$AssmtCode <- as.factor( toupper( Site.Water.df$AssmtCode ) )

## creating a dummy assmtcode to seperate the SiteCode and Date

Site.Water.df$dumAssmtCode <- Site.Water.df$AssmtCode 


Site.Water.df <- Site.Water.df %>%
  separate( dumAssmtCode, c("SiteCode", "Date"), sep =-9) %>% 
  separate( Date, c("Throw1", "Date"), sep =-8) %>%
  dplyr::select( -"Throw1")

Site.Water.df$SiteCode <- as.factor(Site.Water.df$SiteCode)


## Seperating by month year and date

Site.Water.df$dumDate <- as.numeric(Site.Water.df$Date)

Site.Water.df <- Site.Water.df %>%
  separate( dumDate, c("Year", "DumDate"), sep = -4) %>% 
  separate( DumDate, c("Month", "Day"), sep = 2) 

## ok lets filter out the unused SiteCodes

dim(Site.Water.df) # 3174 x 20

Site.Water.df <-Site.Water.df %>%
  filter( SiteCode %in% site.info.df$SiteCode )


dim(Site.Water.df) # 1836 X 20 wow, we cut out 1338 site assments

unique( Site.Water.df$SiteCode ) #165 levels how does that compare to the Sitelist

unique( site.info.df$SiteCode ) # 169 levels, 4 SiteCodes don't have SiteAssmt

## cool lets clean up so data

##first lets look at the relationship betweem conductivity and TDS

plot( x = log10(Site.Water.df$Conduct+1), log10(Site.Water.df$TDS+1))

ggplot( Site.Water.df, aes(x = log10(Conduct+1), log10(TDS+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) 

## seems like there is a unit that is off for years 2009-2014, lets try ato fix
## that

Site.Water.df$TDS[ Site.Water.df$Year == 2009 ] <- Site.Water.df$TDS[ Site.Water.df$Year == 2009 ] * 1000
Site.Water.df$TDS[ Site.Water.df$Year == 2010 ] <- Site.Water.df$TDS[ Site.Water.df$Year == 2010 ] * 1000
Site.Water.df$TDS[ Site.Water.df$Year == 2011 ] <- Site.Water.df$TDS[ Site.Water.df$Year == 2011 ] * 1000
Site.Water.df$TDS[ Site.Water.df$Year == 2012 ] <- Site.Water.df$TDS[ Site.Water.df$Year == 2012 ] * 1000
Site.Water.df$TDS[ Site.Water.df$Year == 2013 ] <- Site.Water.df$TDS[ Site.Water.df$Year == 2013 ] * 1000
Site.Water.df$TDS[ Site.Water.df$Year == 2014 ] <- Site.Water.df$TDS[ Site.Water.df$Year == 2014 ] * 1000


plot( x = log10(Site.Water.df$Conduct+1), log10(Site.Water.df$TDS+1))
#identify( x = log10(Site.Water.df$Conduct+1), log10(Site.Water.df$TDS+1) )

## ok still some issues so lets identify some outliers

## appears prpnd010's conductivity is off , should be 257
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND010_20120720"] <- 257

## Again Conductivity seems to be off for MUD34 change to 825
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "MUD34_20110725"] <- 825

## Condunctivity is way off again looks like it is a reoccuring theme
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND015_20120717"] <- 1063

## Conductivity was way to low, also super weird mud 77 site ph of 2
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "MUD77_20180604"] <- 2290
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUD77_20160609"] <- 842

## Both washburn metric are off for the first visity of 2014 and 11
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "WASHGCP_20140520"] <- 122
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "WASHGCP_20140520"] <- 85.2
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "WASHGCP_20110606"] <- 74

## PRNTH2 is also off looks like tds was normal in 2009 and need to readjust
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "PRNTH2_20090626"] <- 213

## BNPND024's conductivity shiould be 217 instead of 2.17
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "BNPND024_20110623"] <- 217

## YBBA is off, for conductivity and TDS
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "YBBA_20120724"] <- 140
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "YBBA_20140520"] <- 145.5

## PRPND014 is off at several Conductivity
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND014_20150707"] <- 317
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND014_20170718"] <- 240

## VPPND004 is off at several conductivity
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "VPPND004_20100803"] <- 660

## GDPND010 conductivity is off from 2010 , need to go to change to 526
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "GDPND010_20100603"] <- 526

## GDPND006 conditivity is off, should be very high 1370.5 also turbidty is off
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "GDPND006_20140630"] <- 1370
Site.Water.df$Turbid[ Site.Water.df$AssmtCode == "GDPND006_20140630"] <- NA

## GDPND008 is off for 2011 for 
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "GDPND008_20110708"] <- 713
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "GDPND008_20110708"] <- 526

## PRPND014 is off for 2015 
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND014_20150707"] <- 317
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND014_20170718"] <- 240

## EDWD is off for 2018
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "CA-EDWD_20180614"] <- 545

## CA-BN017 is off for 2018 should be 256
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "CA-BN017_20180611"] <- 256

## VPPND005 is off should be 150 instead of 15
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "VPPND005_20190625"] <- 150

## PRPND009 has two issues 2019 should 93 instead of 938, and 2013 139 not 13.9
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND009_20130520"] <- 139
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND009_20190709"] <- 93

## WDMLGCP 2011 looks off, 16.7 instead 167
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "WDMLGCP_20110726"] <- 167
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "WDMLGCP_20180620"] <- 174

## PRNTHOWL is off for 2016 shoud be 272
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRNTHOWL_20160607"] <- 272

### 5CN3 is off 1 to many 0s
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "5CN3_20130812"] <- 513

## MUD31 is off for TDS, for the first visit, but damn did this site get high
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUD31_20090622"] <- 351

## mud 37 is off, to high of tds
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUD37_20130530"] <- 231

## CA-BN018 has a lot of things off lets get to fixing it
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "CA-BN018_20140623"] <- 865
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "CA-BN018_20160531"] <- 638
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "CA-BN018_20170731"] <- 1250
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "CA-BN018_20190701"] <- 1170

## MUD46 is off for 2014
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUD46_20140625"] <- 73.5

## SHPTLGCO us off for 2009 should be 56
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "SHPTLGCP_20090519"] <- 56

## Hidden is off  for 2012 looks like it should be for conduct
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "HIDDEN_20120725"] <- 732

## BNPND011 is off for again 2009
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "BNPND011_20090512"] <- 630
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "BNPND011_20140605"] <- 1225
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "BNPND011_20170731"] <- 1110

## SF31 is off in 2015
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "CA-SF31_20150615"] <- 473

## SF51 is off as well
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "SF51_20150618"] <- 72

## MUD 70 is off for 2015 tds should be 228
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUD70_20150708"] <- 228

## PRPND008 us off for 2015 should be very high 
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "PRPND008_20150624"] <- 1010

## VPPND001 TDS is off or so it appears
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "VPPND001_20140529"] <- 877

## GDPND004 is off looks like for TDS
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "GDPND004_20140630"] <- 1030

## MUDLG is off tds should be higher
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUDLG_20140728"] <- 1235

## BNPND005 is off at several visits so lets fix it
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "BNPND005_20140617"] <- 217
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "BNPND005_20140617"] <- 305
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "BNPND005_20150526"] <- 1220

## MUDEF is off at lots of places
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUDEF_20150623"] <- 1060
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUDEF_20150817"] <- 1460
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "MUDEF_20150817"] <- 2070
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "MUDEF_20140618"] <- 1420
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "MUDEF_20140618"] <- 2035

## PRNTH3 conductivity is off in 2013
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRNTH3_20130603"] <- 267

## SF19 is off for 2014 might be high but 2014 was much higher
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "CA-SF19_20140722"] <- 2750

## NBOR is a bit to high for conductivity
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "NBOR_20180518"] <- 175

## SF79 is off
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "CA-SF79_20150618"] <- 61
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "CA-SF79_20130724"] <- 115

# Gramps is off for 2016
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "GRAMPBOR_20160526"] <- 106

## Kamm is off for 2016
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "KAMM_20160516"] <- 123

## RLSNKGCP 2009 is off 
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "RLSNKGCP_20090708"] <- 240

## GDPND002 is off for tds 
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "GDPND002_20120528"] <- 340

## PRTY03 is off for tds
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "PRTY03_20120525"] <- 315

## PRPND015 is off for conductivity
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "PRPND015_20160601"] <- 430

## TGIF conductivity is off
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "TGIF_20170606"] <- 76

## Ronjr is off for 2019 check paper data sheet?
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "RONJRGCP_20190515"] <- 569

## SF42 seems off
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "SF42_20140612"] <- 588

## PRNTH2 seems off conduct should be higher
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRNTH2_20140513"] <- 826

## GDPMND001's conductivity is off should be 852
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "GDPND001_20110524"] <- 852

## MUD65 has a very low conductivity
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "CA-MUD65_20160517"] <- 890

## Conductivity seems off for MANZA
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "CA-MANZA_20140512"] <- 74

## BEAVER SEEMS OFF 
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "BEAVER_20100625"] <- 434

## Mud34 looks off
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "MUD34_20100721"] <- 889

## VPPND006
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "VPPND006_20100615"] <- 94

## Eagle should be way higher on all fronts
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "EAGLEGCP_20140626"] <- 2925
Site.Water.df$TDS[ Site.Water.df$AssmtCode == "EAGLEGCP_20140626"] <- 2070



ggplot( Site.Water.df, aes(x = log10(Conduct+1), log10(TDS+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

### Sweet Conductivity and TDS are looking pretty damn good, not we need to
### fix Salinity


ggplot( Site.Water.df, aes(x = log10(Conduct+1), log10(Salinity+1),color=Year))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

filter(Site.Water.df, Year == 2015) %>%
  ggplot(  aes(x = log10(Conduct+1), log10(Salinity+1),color=Month))+
  geom_point(size=2, alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Month)


Site.Water.df$Salinity[ Site.Water.df$Year == 2009 ] <- Site.Water.df$Salinity[ Site.Water.df$Year == 2009 ] * 1000
Site.Water.df$Salinity[ Site.Water.df$Year == 2010 ] <- Site.Water.df$Salinity[ Site.Water.df$Year == 2010 ] * 1000
Site.Water.df$Salinity[ Site.Water.df$Year == 2011 ] <- Site.Water.df$Salinity[ Site.Water.df$Year == 2011 ] * 1000
Site.Water.df$Salinity[ Site.Water.df$Year == 2012 ] <- Site.Water.df$Salinity[ Site.Water.df$Year == 2012 ] * 1000
Site.Water.df$Salinity[ Site.Water.df$Year == 2013 ] <- Site.Water.df$Salinity[ Site.Water.df$Year == 2013 ] * 1000
Site.Water.df$Salinity[ Site.Water.df$Year == 2015 ] <- Site.Water.df$Salinity[ Site.Water.df$Year == 2015 ] * 10


Site.Water.df$Salinity[ Site.Water.df$Year == 2016 & Site.Water.df$Month=="05"] <-
  Site.Water.df$Salinity[ Site.Water.df$Year == 2016 & Site.Water.df$Month=="05"] * 10


### ok this should have fixed abumch of the errors now lets clean it up

plot( x = log10(Site.Water.df$Conduct+1), log10(Site.Water.df$Salinity+1))
#identify( x = log10(Site.Water.df$Conduct+1), log10(Site.Water.df$Salinity+1) )

##  BNPND005 , 2014 viswit is low should be 158.5
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BNPND005_20140617"] <- 158.5

Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUDCR_20140625"] <- NA


## Crater salinity is off 
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRCRATER_20160607"] <- 133

## GDPND006 is off for 3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND006_20150518"] <- 269
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND006_20150721"] <- 399
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND006_20160608"] <- 213

## MUD eff is way off for three visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUDEF_20140618"] <- 1015
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUDEF_20150623"] <- 746
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUDEF_20150817"] <- 1040

## Eagle is way off for several visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "EAGLEGCP_20140626"] <- 1520
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "EAGLEGCP_20150604"] <- 132
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "EAGLEGCP_20150727"] <- 231
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "EAGLEGCP_20170720"] <- 96.3

## BNPND001, has twoo visits that are off
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BNPND001_20150625"] <- 75.1
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BNPND001_20150806"] <- 89.7
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "BNPND001_20090512"] <- 87.2

## PRPND003 is off for 3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND003_20150701"] <- 50.2
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND003_20150728"] <- 54.7
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND003_20160518"] <- 32.6

## PRPND009 is off for 4 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND009_20110719"] <- 60
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND009_20150624"] <- 71.6
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND009_20150722"] <- 83.4
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND009_20170606"] <- 33.4

## NDFGCP, no idea what site this is but salinity is obviously off
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "NDFGCP_20090522"] <- 65

## WestWing is off for one visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "WESTWING_20120722"] <- 71
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "WESTWING_20110728"] <- 50


## MUD41 is off for 3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUD41_20140619"] <- 118.5
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUD41_20150630"] <- 135
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUD41_20150730"] <- 167

## Barn is off for 3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BARN_20120725"] <- 290
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BARN_20150602"] <- 269
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BARN_20150712"] <- 260

## GDPND013 is off for 2 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND013_20110706"] <- 450
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND013_20150518"] <- 324

## PRPND004 is off at 3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND004_20160518"] <- 65.5
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND004_20150701"] <- 112
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND004_20150728"] <- 134

## Washgcop is off for 3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "WASHGCP_20140520"] <- 59.4
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "WASHGCP_20150622"] <- 290
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "WASHGCP_20160606"] <- 97

## 5CN1 is off for 3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "5CN1_20150527"] <- 75.7
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "5CN1_20150819"] <- 99.2
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "5CN1_20160523"] <- 60.9

## VPPND006 is off for  3 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "VPPND006_20100615"] <- 70
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "VPPND006_20150528"] <- 113
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "VPPND006_20150805"] <- 213

## GrampBor is off for 1  visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GRAMPBOR_20150608"] <- 123
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GRAMPBOR_20150813"] <- 335
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GRAMPBOR_20160526"] <- 73.8

## PR Crator is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRCRATER_20160607"] <- 133

## BNPND005 ius off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BNPND005_20140617"] <- 158.5

## GDPND006 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND006_20160608"] <- 213

## MUDEF is off for 2 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUDEF_20140618"] <- 1015
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUDEF_20150817"] <- 1040

## Eagle is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "EAGLEGCP_20140626"] <- 1520

## CA-PIG is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "CA-PIG_20160602"] <- 34.5

## GLORGCP is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GLORGCP_20160602"] <- 53.6

## PRPND007 is off for two visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND007_20160601"] <- 70.5
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND007_20160629"] <- 73.6

## WDMLGCP is off for 2 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "WDMLGCP_20160606"] <- 82.0
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "WDMLGCP_20160606"] <- 69

## PRNTHMIT is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRNTHMIT_20160607"] <- 92.7

# PRNTHOWL is off for 2 visits
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRNTHOWL_20150520"] <- 190
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRNTHOWL_20160607"] <- 132

## GDPND004 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND004_20160608"] <- 155

## GDPND005 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND005_20160608"] <- 165

## PRPND008 is off for 
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND008_20160601"] <- 211

## MUD39 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUD39_20160621"] <- 79.2

## SF42 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "SF42_20140612"] <- 607

## GDPND001 IS OFF FOR 1 VISIT 
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "GDPND001_20160608"] <- 500

## MUD77 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "MUD77_20160609"] <- 581

##PRPND002 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND002_20130529"] <- 127
Site.Water.df$Conduct[ Site.Water.df$AssmtCode == "PRPND002_20130529"] <- 300

## BNPND019 is off for one visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "BNPND019_20150519"] <- 67.5

## PRPND014 IS OFF FOR 1 VISIT
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRPND014_20150513"] <- 125

## HeronGCP is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "HERONGCP_20150604"] <- 196

## PRNTH3 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "PRNTH3_20150707"] <- 245

## Kamm is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "KAMM_20150723"] <- 463

## CA-BN004 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "CA-BN004_20150720"] <- 533

## CA-BN018 is off for 1 visit
Site.Water.df$Salinity[ Site.Water.df$AssmtCode == "CA-BN018_20150519"] <- 248


## One temp looks off
Site.Water.df$Temp[ Site.Water.df$AssmtCode == "SF41_20140612"] <- 38


## Ok lets beging to look at density plots

## Turbidity ##
ggplot( Site.Water.df, aes( log10(Turbid +1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year)

## Turbidity looking pretty good, some potentialyl interesting results

## PH ##
ggplot( Site.Water.df, aes( pH ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## ph looking good
## PH ##
ggplot( Site.Water.df, aes( (Temp) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18))+ facet_wrap(~Year) + 
  theme( legend.position = "none")

## ph looking good


## Conductivity ##
ggplot( Site.Water.df, aes( log10(Conduct+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## Conductivity looking constant

## Salinity ##

ggplot( Site.Water.df, aes( log10(Salinity+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## hmm not just a a potneital shift in unit but also distributuion, more
## broad in later years

## TDS ##

ggplot( Site.Water.df, aes( log10(TDS+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## Similare patterns with TDS as with Salinity , probably connected or at least
## similare issue, hopefully one fix for all 

## TotalN

ggplot( Site.Water.df, aes( log10(TotalN+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## looking normal

## TotalP

ggplot( Site.Water.df, aes( log10(TotalP+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")

## looking normal expter for 2012 i wonder how much these daynamics are based
## on when the data is collected 

ggplot( Site.Water.df, aes( log10(DOC+1) ,fill=Year))+
  geom_density( alpha=.5)+ theme_classic() +
  theme(legend.key.size = unit(1, "cm"),
        legend.text=element_text(size=18), 
        legend.title=element_text( size=18)) + facet_wrap(~Year) + 
  theme( legend.position = "none")


## Ok i am going to assume most of the nutrient data is consitent since we 
## dont run it so happy to move on

## lets combind all the data shall we and first lets simplify the data set we
## want



Site.df <- Site.Water.df %>% 
  dplyr::select( c("AssmtCode","Conduct","TDS" ,"Salinity","pH" ,"Temp",         
                   "Turbid"  ,"Secchi", "NutrientSamples" ,"TotalN" , "TotalP"  ,      
                   "DOC" , "NH4", "DON","PO4" , "IP"    ,         
                   "DOP","NO3" , "IN" ) ) %>%
  right_join(Site.df, by = "AssmtCode")


## Checking ou the patterns across time 
filter( Site.df , PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x = Date, y = log10(Conduct+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x = Date, y = log10(Salinity+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x = Date, y = log10(TDS+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "VARGAS PLATEAU REGIONAL PRESERVE")  %>%
  ggplot(aes(x = Date, y = Turbid, color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


## Silver oaks

filter( Site.df , PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x = Date, y = log10(Conduct+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x = Date, y = log10(Salinity+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x = Date, y = log10(TDS+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "SILVER OAKS OSTRICH FARM")  %>%
  ggplot(aes(x = Date, y = Turbid, color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


## Pleasanton Ridge


filter( Site.df , PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(Conduct+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(Salinity+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(TDS+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(Turbid+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(TotalN), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "PLEASANTON RIDGE REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(TotalP), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")


# oh snap PRPND015 looks off for assmcode PRPND015_20170628
Site.df$Salinity[ Site.df$AssmtCode == "PRPND015_20170628"] <- 433
Site.df$Conduct[ Site.df$AssmtCode == "PRPND015_20170628"] <- 812
Site.df$TDS[ Site.df$AssmtCode == "PRPND015_20170628"] <- 581


## 5CN 
filter( Site.df , PropName == "5 CANYONS REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(Conduct+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "5 CANYONS REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(Salinity+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "5 CANYONS REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = log10(TDS+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "5 CANYONS REGIONAL PARK")  %>%
  ggplot(aes(x = Date, y = Turbid, color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## Grant County Park
filter( Site.df , PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x = Date, y = log10(Conduct+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x = Date, y = log10(Salinity+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x = Date, y = log10(TDS+1), color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

filter( Site.df , PropName == "JOSEPH GRANT COUNTY PARK")  %>%
  ggplot(aes(x = Secchi, y = Turbid, color=SiteCode ,group =SiteCode))+ 
  geom_point(size=3)+ geom_line()+
  facet_wrap(~SiteCode) + theme_classic() + theme( legend.position = "none")

## yearly Summary plots

ggplot(Site.df, aes(x = Year, y = log10(Perim+1), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = Temp, fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")


ggplot(Site.df, aes(x = Year, y = log10(Conduct+1), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = log10(TDS +1 ), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = log10(Salinity +1 ), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = log10(Turbid+1), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = log10(TotalN/TotalP), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = (log10(TotalN+1)), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = log10(TotalP+1), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")

ggplot(Site.df, aes(x = Year, y = log10(DOC+1), fill=Year))+ 
  geom_boxplot()+ geom_jitter(alpha=.5, width=.2)+
  theme_classic() + theme( legend.position = "none")




