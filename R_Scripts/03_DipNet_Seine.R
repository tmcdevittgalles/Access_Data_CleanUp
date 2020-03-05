# The main goal of this series of scripts is to merger the numerous raw data 
# files togetherfrom the California database.
# 
# The goal of this script is to clean up the site dipnet and seine information
# data
 
## Setting the working directory

library(tidyverse)
library(dplyr)

setwd("~/Desktop/Current_Projects/Data_CleanUp")


###########################   Dipnet data    #################################

# At the end of this section I will have 1 data set 

#   A) A site x vist x species x sample data frame where each taxa will be
#   its own row and a count of all the taxa observed in the XX number of dipnets

dip.info.df <- read.csv(  "./Data/Raw_Data/Netting_Info.csv" )

dip.surv.df <- read.csv( "./Data/Raw_Data/Survey_Spp.csv" )


## ok lets look at dipnett info first, we need to use this to get the total 
## number of dipnets per site

dim( dip.info.df ) # 25696 X 9

str( dip.info.df ) # looks like we need to get seperate AssmtCode and nettype

dip.info.df <- dplyr::select( dip.info.df, c("SurveyCode","SeineCode", "NetSeq", 
                                             "Depth..ft.",
                                             "Distance..ft."))

colnames( dip.info.df ) <- c("SurveyCode","SeineCode", "NetNum", "Depth", 
                             "Distance")

dip.info.df$SurveyCode <- as.factor( toupper(dip.info.df$SurveyCode) )

dip.info.df$DumCode <- dip.info.df$SurveyCode

## Adding AssmtCode
dip.info.df <- dip.info.df %>%
  separate( DumCode, c("AssmtCode","NetType"), sep =-5) %>% 
  separate( NetType, c("Throw1", "NetType"), sep =1) %>%
  dplyr::select( -"Throw1")


## adding date and SiteCode
dip.info.df <- dip.info.df %>%
  separate( SurveyCode, c("SiteCode","Dum1"), sep =-14) %>% 
  separate( Dum1, c("Date", "Throw1"), sep =-5) %>%
  separate( Date, c("Throw2", "Date"), sep =1) %>%
  dplyr::select( -"Throw1") %>%
  dplyr::select( -"Throw2")

## Adding Year
dip.info.df$DumDate <- dip.info.df$Date

dip.info.df <- dip.info.df %>%
  separate( DumDate, c("Year","Throw1"), sep =-4) %>% 
  dplyr::select( -"Throw1") 

## ok lets filter to the sites we care about

dim(dip.info.df) # 25696 X 8

dip.info.df <- dip.info.df %>%
  filter( SiteCode %in% site.info.df$SiteCode )

dim( dip.info.df ) # 15081 X 8

## ok lets see how well depth and distance match up 

ggplot( dip.info.df , aes( x=Depth, y= Distance , color =Year ))+
  geom_abline(intercept=1, slope=0, size=1)+
  geom_point(size=3, alpha=.5) + theme_classic() + theme(legend.position = "none")

# mostly good lets see who these outliers are


plot( x= dip.info.df$Depth, y=  dip.info.df$Distance)

## Several points are not lining up so lets idenify and adjust
#identify( x= dip.info.df$Depth, y=  dip.info.df$Distance)

## ok it is one visit at pig where they say 11, 12 and 13 need to change to 1.1
## 1.2, and 1.

dip.info.df$Depth[dip.info.df$SeineCode=="CA-PIG_20130708_NS02-01"] <- 1.3
dip.info.df$Depth[dip.info.df$SeineCode=="CA-PIG_20130708_NS02-02"] <- 1.2
dip.info.df$Depth[dip.info.df$SeineCode=="CA-PIG_20130708_NS02-03"] <- 1.1

ggplot( dip.info.df , aes( x=Depth, y= Distance , color =Year ))+
  geom_point(size=3, alpha=.5) + 
  theme_classic() + theme(legend.position = "none")

## I say it all looks good so lets summarise this shit

sum.net.df <- dip.info.df %>%
  group_by( SiteCode, Date, AssmtCode, NetType, Year) %>%
  summarise(
    nSweep = n(),
    Dist = sum(Distance, na.rm = T)
  )
## Checking some distances and it looks pretty good
ggplot( sum.net.df, aes( x=Dist, fill = Year )) + geom_density() +
  facet_wrap(~Year) +theme(legend.position = "none") + theme_classic()

## Lets check number of dipnets and sweeps, it is looking pretty good
ggplot( sum.net.df, aes( x=nSweep, fill = Year )) + geom_density() +
  facet_wrap(~NetType) +theme(legend.position = "none") + theme_classic()




### Ok now lets bring in the actual dipnet data


dip.surv.df <- read.csv( "./Data/Raw_Data/Survey_Spp.csv" )


## ok lets look at dipnett info first, we need to use this to get the total 
## number of dipnets per site

dim( dip.surv.df ) # 120212 X 9

str( dip.surv.df ) # looks like we need to get seperate AssmtCode and nettype

dip.surv.df <- dplyr::select( dip.surv.df , c("SeineCode", "SppGrp", "Sppcode",
                                              "Count"))

colnames( dip.surv.df  ) <- c("SurveyCode",  "SppGrp", "SppCode",
                              "Count")

dip.surv.df$SurveyCode <- as.factor( toupper(dip.surv.df$SurveyCode) )

dip.surv.df$SppGrp <- as.factor( toupper(dip.surv.df$SppGrp) )

dip.surv.df$SppCode <- as.factor( toupper(dip.surv.df$SppCode) )

dip.surv.df$DumCode <- dip.surv.df$SurveyCode

## Adding AssmtCode
dip.surv.df <- dip.surv.df %>%
  separate( DumCode, c("AssmtCode","NetType"), sep =-8) %>% 
  separate( NetType, c("Throw1", "NetType"), sep =1) %>%
  separate( NetType, c("NetType", "Throw2"), sep =4) %>%
  dplyr::select( -"Throw1") %>%
  dplyr::select( -"Throw2")

## adding survey code
dip.surv.df  <- dip.surv.df %>%
  separate( SurveyCode, c("SurveyCode","Dum1"), sep =-3) %>% 
  dplyr::select( -"Dum1")



## adding SiteCode and Date

dip.surv.df$DumCode <- dip.surv.df$AssmtCode

dip.surv.df <- dip.surv.df %>%
  separate( DumCode, c("SiteCode","Date"), sep =-9) %>% 
  separate( Date, c("Throw1", "Date"), sep =1) %>%
  dplyr::select( -"Throw1") 

## Adding Year
dip.surv.df$DumDate <- dip.surv.df$Date

dip.surv.df <- dip.surv.df %>%
  separate( DumDate, c("Year","Throw1"), sep =-4) %>% 
  dplyr::select( -"Throw1") 

## ok lets filter to the sites we care about

dim(dip.surv.df) # 120212 X 9

dip.surv.df <- dip.surv.df %>%
  filter( SiteCode %in% site.info.df$SiteCode )

dim( dip.surv.df ) # 85502 X 9 # lost 34710 rows 

dip.surv.df[which.max(dip.surv.df$Count),]

## cool now lets check to make sure the count data only contain whole numbers
in.check <- dip.surv.df$Count %% 1
## if they are whole number then the sum of in check should be 0
sum(in.check, na.rm=T)

unique( dip.surv.df$SppCode)

## lets see if we need to combind any of the species codes into new SppCode

spp.sum.df <- dip.surv.df %>%
  group_by(SiteCode, NetType, Year , SppCode, SppGrp) %>%
  summarise(
    Count = sum(Count, na.rm=T)
  )


filter(spp.sum.df, SppGrp =="SNAIL") %>%
  ggplot( aes(x=Year, y=log10(Count+1), fill=SppCode))+
  geom_jitter(size=2,alpha=.5, width=.1,aes(color=SppCode))+
  geom_boxplot(alpha=.5, color="black") + theme_classic()+facet_wrap(~SppCode) + 
  theme( legend.position = "none" )

filter(spp.sum.df, SppGrp =="AMPHIBIAN") %>%
  ggplot( aes(x=Year, y=log10(Count+1), fill=SppCode))+
  geom_jitter(size=2,alpha=.5, width=.1,aes(color=SppCode))+
  geom_boxplot(alpha=.5, color="black") + theme_classic()+facet_wrap(~SppCode) + 
  theme( legend.position = "none" )

filter(spp.sum.df, SppGrp =="INVERTEBRATE") %>%
  ggplot( aes(x=Year, y=log10(Count+1), fill=SppCode))+
  geom_jitter(size=2,alpha=.5, width=.1,aes(color=SppCode))+
  geom_boxplot(alpha=.5, color="black") + theme_classic()+facet_wrap(~SppCode) + 
  theme( legend.position = "none" )

## Replacing Mite with correct SppCode "ACAR"
dip.surv.df$SppCode[dip.surv.df$SppCode=="MITE"] <- "ACAR"

## Replacing the various BACA codes with correct SppCode
dip.surv.df$SppCode[dip.surv.df$SppCode=="BAET"] <- "BACA"
dip.surv.df$SppCode[dip.surv.df$SppCode=="EPHE"] <- "BACA"


## Replacing the various BESP codes with correct SppCode
dip.surv.df$SppCode[dip.surv.df$SppCode=="BELO"] <- "BESP"
dip.surv.df$SppCode[dip.surv.df$SppCode=="LETH"] <- "BESP"

## Replacing the various Clam codes with correct SppCode 
dip.surv.df$SppCode[dip.surv.df$SppCode=="CLSP"] <- "SPHN"

## Replacing the various Annelid codes with correct SppCode
dip.surv.df$SppCode[dip.surv.df$SppCode=="ANNE"] <- "OLIG"


## Replacing the various HAHA codes with correct SppCode
dip.surv.df$SppCode[dip.surv.df$SppCode=="HAHA"] <- "HASP"
dip.surv.df$SppCode[dip.surv.df$SppCode=="HAPE"] <- "HASP"


## Replacing the various GYRA codes with correct SppCode
dip.surv.df$SppCode[dip.surv.df$SppCode=="GYCI"] <- "GYRA"
dip.surv.df$SppCode[dip.surv.df$SppCode=="GYPA"] <- "GYRA"

## Replacing the various PHSP codes with correct SppCode
dip.surv.df$SppCode[dip.surv.df$SppCode=="PHGY"] <- "PHSP"
dip.surv.df$SppCode[dip.surv.df$SppCode=="PHAC"] <- "PHSP"

## Replacing the various Annelid codes with correct SppCode
dip.surv.df$SppCode[dip.surv.df$SppCode=="LYCO"] <- "LYMN"


View(dip.surv.df[dip.surv.df$SppCode=="AMCA",])

## Lots of the outliers appear to be from some very high amphipod counts 
## which seems about right, i am going to start by just subsetting taxa i am
## interested in

dip.surv.df <- dip.surv.df %>%
  filter( SppCode == "HELI" | SppCode == "PHSP" | SppCode == "GYRA"|
            SppCode == "LYMN" | SppCode == "RARI" | SppCode == "AMCA" |
            SppCode == "BUBO" | SppCode == "PSRE" | SppCode == "RACA" | 
            SppCode == "RADR" | SppCode == "TATO" | SppCode == "TAGR" |
            SppCode == "ACAR" | SppCode == "AESH" | SppCode == "AMPH" | 
            SppCode == "ANIS" | SppCode == "BACA" | SppCode == "BESP" |
            SppCode == "BOAT" | SppCode == "CHAO" | SppCode == "CHIR" |
            SppCode == "COEN" | SppCode == "COLE" | SppCode == "CYSP" | 
            SppCode == "DIPT" | SppCode == "DYTI" | SppCode == "HASP" |
            SppCode == "HYDR" | SppCode == "LEEC" | SppCode == "LEST" |
            SppCode == "LIBE" | SppCode == "MOSQ" | SppCode == "NEFL" |
            SppCode == "NEPI" | SppCode == "NOTO" | SppCode == "OLIG" |
            SppCode == "PLAN" | SppCode == "SPHN" | SppCode == "ZYGP" )

dip.surv.df <- dip.surv.df %>%
  filter( NetType != "VS01")

dim( dip.surv.df ) # 81958 X 9 cut 34353 rows

ggplot( dip.surv.df, aes(x = (Count), fill = SppGrp)) + geom_density( alpha=.5)+
  theme_classic() + theme_classic() + facet_wrap(~SppCode, scales = 'free')

## still seem to have very large values lets see what that is like

dip.surv.df[which.max(dip.surv.df$Count),] 

## Hmm it looks there might be some high counts but they could be right lets
## break it downn by net type

filter( dip.surv.df,NetType == "NS01" ) %>%
  ggplot( aes(x = log10(Count), fill = SppGrp)) + geom_density( alpha=.5)+
  theme_classic() + theme_classic() + facet_wrap(~SppCode)

-## Ok lets get the summary stats going
  
  sum.surv.df <- dip.surv.df %>%
  group_by(AssmtCode, SiteCode, Date, Year, NetType, SppGrp,
           SppCode  ) %>%
  summarise(
    Count = sum(Count, na.rm = T)
  )

sum.surv.df$OrdCode <- NA 
sum.surv.df$Total <- NA

## adding the survey codes for the more coarse taxonomic resolution

sum.surv.df$OrdCode[sum.surv.df$SppCode == "BEST" |
                      sum.surv.df$SppCode == "BOAT" |
                      sum.surv.df$SppCode == "NOTO" |
                      sum.surv.df$SppCode == "BOAT" |
                      sum.surv.df$SppCode == "NEPI" ] <- "HEMI"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "DYTI" |
                      sum.surv.df$SppCode == "HYDR" |
                      sum.surv.df$SppCode == "COLE" |
                      sum.surv.df$SppCode == "HASP" ] <- "COLE"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "COEN" |
                      sum.surv.df$SppCode == "LEST" |
                      sum.surv.df$SppCode == "ZYGO" ] <- "ZYGO"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "ANIS" |
                      sum.surv.df$SppCode == "AESH" |
                      sum.surv.df$SppCode == "LIBE" ] <- "ANIS"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "CHIR" |
                      sum.surv.df$SppCode == "CHAO" |
                      sum.surv.df$SppCode == "NEFL" |
                      sum.surv.df$SppCode == "DIPT" |
                      sum.surv.df$SppCode == "MOSQ" ] <- "DIPT"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "PSRE" |
                      sum.surv.df$SppCode == "BUBO" |
                      sum.surv.df$SppCode == "RACA" |
                      sum.surv.df$SppCode == "RADR" ] <- "ANUR"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "TATO" |
                      sum.surv.df$SppCode == "TAGR" |
                      sum.surv.df$SppCode == "AMCA" ] <- "CAUD"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "BACA" ] <- "EPHE"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "PHSP" |
                      sum.surv.df$SppCode == "HELI" |
                      sum.surv.df$SppCode == "GYRA" |
                      sum.surv.df$SppCode == "LYMN" |
                      sum.surv.df$SppCode == "RARI" ] <- "PLAN"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "ACAR" ] <- "ACAR"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "AMPH" |
                      sum.surv.df$SppCode == "CYSP"  ] <- "CRUS"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "LEEC" |
                      sum.surv.df$SppCode == "OLIG"  ] <- "ANNE"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "PLAN" ] <- "TURB"

sum.surv.df$OrdCode[sum.surv.df$SppCode == "SPHN" ] <- "BIVA"



ord.surv.df <- sum.surv.df %>%
  group_by(AssmtCode, SiteCode, Date, Year, NetType, OrdCode
  ) %>%
  summarise(
    Total= sum(Count, na.rm=T)
  )

## Ok lets combind these data frames 

annoying_Dip <- sum.net.df %>%
  dplyr::select( c("AssmtCode", "NetType", "nSweep", "Dist" ) )%>%
  right_join( sum.surv.df, by =c( "AssmtCode", "NetType"))

full.dip.df <- dplyr::select(annoying_Dip, c("AssmtCode", "SiteCode.x","Date.x","Year",
                                             "NetType",  "nSweep", "Dist", "SppGrp",
                                             "SppCode", "Count"))

colnames(full.dip.df) <- c("AssmtCode", "SiteCode","Date","Year",
                           "NetType",  "nSweep", "Dist", "SppGrp",
                           "SppCode", "Count")

explore.dip.df <- right_join(full.dip.df, site.info.df, by="SiteCode")


explore.dip.df %>%
  filter(  SppGrp == "AMPHIBIAN" & NetType == "NS02") %>%
  ggplot( aes( x = as.factor(Year), y=log10((Count/Dist)+1), color=Year )) +
  geom_jitter(size=1.5,alpha=.7)  +   geom_boxplot(color="black",fill="grey",alpha=.6) + 
  theme_classic()+
  facet_wrap(~SppCode, scales= "free_y")

explore.dip.df %>%
  filter(  SppGrp == "SNAIL" & NetType == "NS01") %>%
  ggplot( aes( x = as.factor(Year), y=log10((Count/nSweep)+1), color=Year )) +
  geom_jitter(size=1.5,alpha=.7)  +   geom_boxplot(color="black",fill="grey",alpha=.6) + 
  theme_classic()+
  facet_wrap(~SppCode, scales= "free_y")

explore.dip.df %>%
  filter(  SppGrp == "INVERTEBRATE" & NetType == "NS01" ) %>%
  ggplot( aes( x = as.factor(Year), y=log10((Count/nSweep)+1), color=Year )) +
  geom_jitter(size=1.5,alpha=.7)  +   geom_boxplot(color="black",fill="grey",alpha=.6) + 
  theme_classic()+
  facet_wrap(~SppCode, scales= "free_y")


